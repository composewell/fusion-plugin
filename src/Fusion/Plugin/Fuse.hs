{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
#endif

module Fusion.Plugin.Fuse
    ( fusionMarkInline
    , fusionSimplify
    )
where

#include "Fusion/Plugin/Common.h"

#if MIN_VERSION_ghc(8,6,0)
import Control.Monad (when)
import Data.Either (partitionEithers)
import Data.Maybe (isJust)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Lint.Interactive (interactiveInScope)
import GHC.Driver.Config.Core.Opt.Simplify (initSimplMode, initSimplifyOpts)
#endif
#endif

-- Implicit imports
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

import Fusion.Plugin.Types

import Fusion.Plugin.Common
    ( ReportMode(..)
    , altsContainsAnn
    , dbgLevel
    , debug
    , dumpBindCore
    , dumpCore
    , getAnnotationsByStableName
    , getNonRecBinder
    , listPath
    , modulePackageName
    , qualifiedTyConName
    , resolveTHNames
    , showDetailsCaseMatch
    , showDetailsConstr
    , showInfo
    , showWithUnique
    )

#if MIN_VERSION_ghc(8,6,0)


-------------------------------------------------------------------------------
-- Set always INLINE on a binder
-------------------------------------------------------------------------------

unfoldCompulsory :: Arity -> Unfolding -> Unfolding
unfoldCompulsory arity cuf@CoreUnfolding{} =
    cuf
        { uf_src=
#if MIN_VERSION_ghc(9,6,0)
            StableSystemSrc
#else
            InlineStable
#endif
        , uf_guidance = UnfWhen arity True True
        }
unfoldCompulsory _ x = x -- NoUnfolding

-- Sets the inline pragma on a bndr, and forgets the unfolding.
setAlwaysInlineOnBndr :: DynFlags -> CoreBndr -> CoreBndr
setAlwaysInlineOnBndr dflags n =
    let info =
            case zapUsageInfo $ idInfo n of
                Just i -> i
                Nothing ->
                    error "The impossible happened!! Or GHC changed their api."
        unf = unfoldingInfo info
        info' =
            setUnfoldingInfo
                (setInlinePragInfo info alwaysInlinePragma)
                (unfoldCompulsory (arityInfo info) unf)
     in debug 1
            ("Forcing inline on: " ++ showWithUnique dflags n)
            (lazySetIdInfo n info')

--TODO: Replace self-recursive definitions with a loop breaker.
-- | Set inline on specific binders inside a given bind.
setInlineOnBndrs :: DynFlags -> [CoreBndr] -> CoreBind -> CoreBind
setInlineOnBndrs dflags bndrs = everywhere $ mkT go
  where
    go :: CoreBind -> CoreBind
    go (NonRec b expr) | any (b ==) bndrs =
        NonRec (setAlwaysInlineOnBndr dflags b) expr
    go x = x


hasInlineBinder :: CoreBndr -> Bool
hasInlineBinder bndr =
    let inl = inlinePragInfo $ idInfo bndr
    in isInlinePragma inl && IS_ACTIVE (inlinePragmaActivation inl)

hasNoInlineBinder :: CoreBndr -> Bool
#if MIN_VERSION_ghc(9,4,0)
hasNoInlineBinder bndr = isNoInlinePragma (inlinePragInfo (idInfo bndr))
#else
hasNoInlineBinder bndr =
    case inlinePragmaSpec (inlinePragInfo (idInfo bndr)) of
        NoInline -> True
        _ -> False
#endif

data InlineNeed a
    = InlineNeeded a
      -- ^ Matches a fusible type and the binder carries no active inline
      -- pragma of its own yet -- force it inline.
    | InlineBlockedByNoInline TyCon
      -- ^ Matches the given fusible type, but the binder carries an
      -- explicit user @NOINLINE@ pragma, which the plugin must never
      -- override. Reported once per binder (not once per matching site) by
      -- the caller -- see 'transformBind'.
    | InlineNotNeeded
      -- ^ Either no fusible match, or the binder is already marked inline.

-------------------------------------------------------------------------------
-- Inspect case alternatives for interesting constructor matches
-------------------------------------------------------------------------------


needInlineCaseAlt
    :: DynFlags
    -> [CoreBind]
    -> UNIQ_FM
    -> [Alt CoreBndr]
    -> InlineNeed (Alt CoreBndr)
needInlineCaseAlt dflags parents anns bndr =
    let mesg = "Binder: " ++ listPath dflags parents
        parentBndr = getNonRecBinder (head parents)
        matched = altsContainsAnn dflags (isJust . lookupUFM anns) bndr
    in if hasNoInlineBinder parentBndr
       then case matched of
                Just (ALT_CONSTR(DataAlt dcon,_,_)) ->
                    InlineBlockedByNoInline (dataConTyCon dcon)
                _ -> InlineNotNeeded
       else if not (hasInlineBinder parentBndr)
       then debug 2 (mesg ++ " not inlined") $
                case matched of
                    Just alt -> InlineNeeded alt
                    Nothing -> InlineNotNeeded
       else debug 2 (mesg ++ " already inlined") InlineNotNeeded

-------------------------------------------------------------------------------
-- Determine if a let binder contains a case match on an annotated type
-------------------------------------------------------------------------------

-- XXX Can check the call site and return only those that would enable
-- case-of-known constructor to kick in. Or is that not relevant?
--
-- | Discover binders that start with a pattern match on constructors that are
-- annotated with Fuse. For example, for the following code:
--
-- @
-- joinrec { $w$g0 x y z = case y of predicateAlt -> ... } -> returns [$w$go]
-- join { $j1_sGH1 x y z = case y of predicateAlt -> ... } -> returns [$j1_sGH1]
-- @
--
-- It will return @$w$go@ and @$j1_sGH1@ if they are matching on fusible
-- constructors.
--
-- Returns all the binds in the hierarchy from the parent to the bind
-- containing the case alternative. Along with the binders it also returns the
-- case alternative scrutinizing the annotated type for better errors with
-- context.
-- 'Left' entries are binders whose case-alt matched a fusible type but were
-- blocked from being forced inline by a user @NOINLINE@ pragma (see
-- 'InlineNeed'); 'Right' entries are the actual pattern-match hits to force
-- inline. The caller ('transformBind') is responsible for deduplicating the
-- 'Left' paths by binder before warning, since the same @NOINLINE@'d binder
-- can contain more than one matching case.
letBndrsThatAreCases
    :: DynFlags
    -> UNIQ_FM
    -> CoreBind
    -> [Either ([CoreBind], TyCon) ([CoreBind], Alt CoreBndr)]
letBndrsThatAreCases dflags anns bind = goLet [] bind
  where
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    --
    -- When second argument is "False" it means we do not examine the case
    -- alternatives for annotated constructors when we encounter a case
    -- statement. We pass the second arg as "True" in recursive calls to "go"
    -- after we encounter a let binder. We reset it to "False" when we do not
    -- want to consider inlining the current binder.
    --
    go :: [CoreBind] -> Bool -> CoreExpr
       -> [Either ([CoreBind], TyCon) ([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents True (Case _ _ _ alts) =
        let result = alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents False expr1)
        in case needInlineCaseAlt dflags parents anns alts of
            InlineNeeded x -> Right (parents, x) : result
            InlineBlockedByNoInline tycon -> Left (parents, tycon) : result
            InlineNotNeeded -> result

    -- Only traverse the Alt expressions of the case to discover new let
    -- bindings. Do not match for annotated constructors in the Alts.
    go parents False (Case _ _ _ alts) =
        alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents False expr1)

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents _ (Let bndr expr1) =    goLet parents bndr
    -- If the binding starts with a "let" expression we ignore the case matches
    -- in its expression. Can inlining such lets be useful in some cases?
                                    ++ go parents False expr1

    -- Traverse these to discover new let bindings. We ignore any case matches
    -- directly in the application expr. There should not be any harm in
    -- chasing expr1 with True here?
    go parents _ (App expr1 expr2) =    go parents False expr1
                                     ++ go parents False expr2
    go parents x (Lam _ expr1) = go parents x expr1
    go parents _ (Cast expr1 _) = go parents False expr1

    -- There are no let bindings in these.
    go _ _ (Var _) = []
    go _ _ (Lit _) = []
    go _ _ (Tick _ _) = []
    go _ _ (Type _) = []
    go _ _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind
          -> [Either ([CoreBind], TyCon) ([CoreBind], Alt CoreBndr)]
    -- Here we pass the second argument to "go" as "True" i.e. we are now
    -- looking to match the case alternatives for annotated constructors.
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) True expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

needInlineTyCon :: CoreBind -> UNIQ_FM -> TyCon -> InlineNeed ()
needInlineTyCon parent anns tycon =
    let parentBndr = getNonRecBinder parent
    in case lookupUFM anns (GET_NAME tycon) of
        Just _ | hasNoInlineBinder parentBndr -> InlineBlockedByNoInline tycon
        Just _ | not (hasInlineBinder parentBndr) -> InlineNeeded ()
        _ -> InlineNotNeeded

-- XXX Currently this function and containsAnns are equivalent. So containsAnns
-- can be used in place of this. But we may want to restrict this to certain
-- cases and keep containsAnns unrestricted so it is kept separate for now.
--
-- | Discover binders whose return type is a fusible constructor and the
-- constructor is directly used in the binder definition rather than through an
-- identifier.
--
-- See 'letBndrsThatAreCases' for the meaning of the 'Either' result: 'Left'
-- is a binder blocked from force-inlining by a user @NOINLINE@ pragma,
-- 'Right' is an actual hit to force inline.
constructingBinders
    :: UNIQ_FM -> CoreBind -> [Either ([CoreBind], TyCon) ([CoreBind], Id)]
constructingBinders anns bind = goLet [] bind
  where
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    --
    go :: [CoreBind] -> CoreExpr
       -> [Either ([CoreBind], TyCon) ([CoreBind], Id)]

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (Case _ _ _ alts) =
        alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents expr1)
    -- If the head of the application spine is a data constructor, record a hit
    -- for its type -- this recognizes a constructor applied to its fields
    -- (e.g. `Yield x y`), which checking a bare 'Var' node's own type cannot:
    -- the unapplied constructor 'Id' has a function type, not the constructed
    -- type, so that check only fires for nullary constructors.
    --
    -- XXX Inlining these cases can bloat the code, need to prove the benefit
    -- before enabling this.
    {-
    go parents e@(App _ _) =
        let (fun, args) = collectArgs e
            needInline = needInlineTyCon (head parents) anns
            hit = case fun of
                Var i
                    | Just dcon <- isDataConId_maybe i ->
                        case needInline (dataConTyCon dcon) of
                            InlineNeeded () -> [Right (parents, i)]
                            InlineBlockedByNoInline tycon ->
                                [Left (parents, tycon)]
                            InlineNotNeeded -> []
                _ -> []
        in hit ++ go parents fun ++ concatMap (go parents) args
    -}
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- Check if the Var is a data constructor of interest
    go parents (Var i) =
        let needInline = needInlineTyCon (head parents) anns
        in case tyConAppTyConPicky_maybe (varType i) of
            Just tycon ->
                case needInline tycon of
                    InlineNeeded () -> [Right (parents, i)]
                    InlineBlockedByNoInline tc -> [Left (parents, tc)]
                    InlineNotNeeded -> []
            Nothing -> []

    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind
          -> [Either ([CoreBind], TyCon) ([CoreBind], Id)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

-- | If the given top level binder carries a 'FuseTypes' annotation, return the
-- module-wide 'Fuse' annotation map augmented with an entry for each of the
-- named types, so that they are treated exactly like 'Fuse'-annotated types
-- while inlining inside this binding -- and nowhere else. Returns the map
-- unchanged if the binder is not annotated.
augmentFuseTypes :: UNIQ_FM -> FUSE_TYPES_FM -> CoreBndr -> CoreM (UNIQ_FM)
augmentFuseTypes anns fuseTypesAnns b =
    case Map.lookup (getOccString (GET_NAME b)) fuseTypesAnns of
        Nothing -> return anns
        Just (FuseTypes ns) -> do
            names <- resolveTHNames ns
            return $ plusUFM anns (listToUFM (map (\n -> (n, [Fuse])) names))

removeFuseTypes :: UNIQ_FM -> NO_FUSE_TYPES_FM -> CoreBndr -> CoreM (UNIQ_FM)
removeFuseTypes anns noFuseTypesAnns b =
    case Map.lookup (getOccString (GET_NAME b)) noFuseTypesAnns of
        Nothing -> return anns
        Just (NoFuseTypes ns) -> do
            names <- resolveTHNames ns
            return $ delListFromUFM anns names

removeFuse :: UNIQ_FM -> NO_FUSE_FM -> CoreBndr -> UNIQ_FM
removeFuse anns noFuseAnns b =
    case Map.lookup (getOccString (GET_NAME b)) noFuseAnns of
        Nothing -> anns
        Just NoFuse -> emptyUFM

markInline :: Int -> ReportMode -> Bool -> ModGuts -> CoreM ModGuts
markInline pass reportMode transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- FMAP_SND getAnnotations deserializeWithData guts
    fuseTypesAnns <-
        getAnnotationsByStableName "FuseTypes" deserializeWithData guts
    noFuseTypesAnns <-
        getAnnotationsByStableName "NoFuseTypes" deserializeWithData guts
    noFuseAnns <-
        getAnnotationsByStableName "NoFuse" deserializeWithData guts
    let pkgName = modulePackageName (mg_module guts)
        modName = moduleNameString (moduleName (mg_module guts))
        modBinds = flattenBinds (mg_binds guts)
    if (anyUFM (any (== Fuse)) anns || not (Map.null fuseTypesAnns))
    then do
        r <- bindsOnlyPass
                (mapM
                    (transformBind
                        dflags anns fuseTypesAnns noFuseTypesAnns noFuseAnns
                        pkgName modName modBinds))
                guts
        if dbgLevel > 0
        then dumpCore True 0 (text ("Fusion-plugin-" ++ show pass)) r
        else return r
    else return guts

    where

    -- transformBind :: DynFlags -> UniqFM Unique [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind
            dflags anns0 fuseTypesAnns noFuseTypesAnns noFuseAnns
            pkgName modName modBinds bind@(NonRec b _) = do
        -- Types named in a 'FuseTypes' annotation on this binding act as if
        -- they were 'Fuse'-annotated, but only while inlining inside it.
        -- Types named in a 'NoFuseTypes' annotation are stripped of their
        -- 'Fuse' status locally, overriding the above and any module-wide
        -- 'Fuse' annotation for this binding only. A 'NoFuse' annotation
        -- overrides all of the above, disabling forced inlining for this
        -- binding altogether irrespective of type.
        anns1 <- augmentFuseTypes anns0 fuseTypesAnns b
        anns2 <- removeFuseTypes anns1 noFuseTypesAnns b
        let anns = removeFuse anns2 noFuseAnns b
        let (blockedPat, patternMatches) =
                partitionEithers (letBndrsThatAreCases dflags anns bind)
        let uniqPat = DL.nub (map (getNonRecBinder. head . fst) patternMatches)

        let (blockedConstr, constrs) = partitionEithers (constructingBinders anns bind)
        let uniqConstr = DL.nub (map (getNonRecBinder. head . fst) constrs)

        -- Warn at most once per binder (regardless of how many separate
        -- sites inside it triggered the check) that a NOINLINE pragma is
        -- blocking a fusible match/construction from being force-inlined,
        -- naming every distinct fusible type found, and dump its Core (and
        -- the closure it reaches) so the blocked fusion can be diagnosed
        -- without a separate 'DumpCore' annotation.
        --
        -- A NOINLINE'd binder is never touched by 'setInlineOnBndrs', so the
        -- same block is reported identically on every one of the 3
        -- 'fusionMarkInline' passes 'install' runs per compile; restrict to
        -- pass 1 so it is reported (and the core dumped) only once per
        -- compile rather than 3 times.
        let blocked = blockedPat ++ blockedConstr
            blockedBinders = DL.nub $ map (getNonRecBinder . head . fst) blocked
        when (pass == 1) $ mapM_
            (\bb -> do
                let tycons = DL.nub
                        [ qualifiedTyConName tc
                        | (ps, tc) <- blocked, getNonRecBinder (head ps) == bb ]
                putMsgS $ "fusion-plugin: " ++ showWithUnique dflags bb
                        ++ ": NOINLINE pragma blocks fusion ("
                        ++ DL.intercalate ", " tycons ++ ")"
                dumpBindCore dflags pkgName modName modBinds bb)
            blockedBinders

        -- TBD: For ReportWarn level prepare a single consolidated list of
        -- paths with one entry for each binder and giving one example of what
        -- it scrutinizes and/or constructs, for example:
        --
        -- \$sconcat_s8wu/step5_s8M4: Scrutinizes ConcatOuter, Constructs Yield
        --
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> do
                let allBinds = map fst patternMatches ++ map fst constrs
                when (not $ null allBinds) $ do
                    putMsgS "INLINE required on:"
                    putMsgS $ DL.unlines $ DL.nub $ map (listPath dflags) allBinds
            _ -> do
                showInfo b dflags reportMode "SCRUTINIZE"
                    uniqPat patternMatches showDetailsCaseMatch
                showInfo b dflags reportMode "CONSTRUCT"
                    uniqConstr constrs showDetailsConstr

        let bind' = do
                let allBinders = uniqPat ++ uniqConstr
                if transform && (not $ null allBinders)
                then setInlineOnBndrs dflags allBinders bind
                else bind
        return bind'

    transformBind
            dflags anns fuseTypesAnns noFuseTypesAnns noFuseAnns
            pkgName modName modBinds (Rec bs) = do
        fmap Rec (mapM transformAsNonRec bs)

        where

        transformAsNonRec (b, expr) = do
            r <- transformBind
                    dflags anns fuseTypesAnns noFuseTypesAnns noFuseAnns
                    pkgName modName modBinds (NonRec b expr)
            case r of
                NonRec b1 expr1 -> return (b1, expr1)
                _ -> error "Bug: expecting NonRec binder"

-- | Core pass to mark functions scrutinizing constructors marked with Fuse
fusionMarkInline :: Int -> ReportMode -> Bool -> CoreToDo
fusionMarkInline pass opt transform =
    CoreDoPluginPass "Mark for inlining" (markInline pass opt transform)

-------------------------------------------------------------------------------
-- Simplification pass after marking inline
-------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,6,0)
fusionSimplify :: RuleBase -> HscEnv -> DynFlags -> CoreToDo
fusionSimplify hpt_rules hsc_env dflags =
    let mode = initSimplMode dflags InitialPhase "Fusion Plugin Inlining"
        extra_vars = interactiveInScope (hsc_IC hsc_env)
     in CoreDoSimplify
            (initSimplifyOpts
                dflags extra_vars (maxSimplIterations dflags) mode hpt_rules)
#else
fusionSimplify :: HscEnv -> DynFlags -> CoreToDo
fusionSimplify _hsc_env dflags =
    let mode =
            SimplMode
            { sm_phase = InitialPhase
            , sm_names = ["Fusion Plugin Inlining"]
            , sm_dflags = dflags
            , sm_rules = gopt Opt_EnableRewriteRules dflags
            , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
            , sm_inline = True
            , sm_case_case = True
#if MIN_VERSION_ghc(9,2,0)
            , sm_uf_opts = unfoldingOpts dflags
            , sm_pre_inline = gopt Opt_SimplPreInlining dflags
            , sm_logger = hsc_logger _hsc_env
#endif
#if MIN_VERSION_ghc(9,2,2)
            , sm_cast_swizzle = True
#endif
#if MIN_VERSION_ghc(9,5,0)
            , sm_float_enable = floatEnable dflags
#endif
            }
    in CoreDoSimplify
#if MIN_VERSION_ghc(9,5,0)
        (CoreDoSimplifyOpts (maxSimplIterations dflags) mode)
#else
        (maxSimplIterations dflags) mode
#endif
#endif

#endif
