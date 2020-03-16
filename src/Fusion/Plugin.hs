-- |
-- Module      : Fusion.Plugin
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stream fusion depends on the GHC case-of-case transformations eliminating
-- intermediate constructors.  Case-of-case transformation in turn depends on
-- inlining. During core-to-core transformations GHC may create several
-- internal bindings (e.g. join points) which may not get inlined because their
-- size is bigger than GHC's inlining threshold. Even though we know that after
-- fusion the resulting code would be smaller and more efficient. The
-- programmer cannot force inlining of these bindings as there is no way for
-- the programmer to address these bindings at the source level because they
-- are internal, generated during core-to-core transformations. As a result
-- stream fusion fails unpredictably depending on whether GHC was able to
-- inline the internal bindings or not.
--
-- [See GHC ticket #17075](https://gitlab.haskell.org/ghc/ghc/issues/17075) for
-- more details.
--
-- This plugin provides the programmer with a way to annotate certain types
-- using a custom 'Fuse' annotation. The programmer would annotate the
-- types that are to be eliminated by fusion via case-of-case transformations.
-- During the simplifier phase the plugin goes through the relevant bindings
-- and if one of these types are found inside a binding then that binding is
-- marked to be inlined irrespective of the size.
--
-- At the right places, fusion can provide dramatic performance improvements
-- (e.g. 10x) to the code.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Fusion.Plugin
    (
    -- * Using the Plugin
    -- $using

    -- * Implementation Details
    -- $impl

    -- * Results
    -- $results
      plugin
    )
where

-- Explicit/qualified imports
import Control.Monad (when)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Outputable (Outputable(..), text)

import qualified Data.List as DL

-- Implicit imports
import GhcPlugins

-- Imports from this package
import Fusion.Plugin.Types (Fuse(..))

-- $using
--
-- This plugin was primarily motivated by fusion issues discovered in
-- [streamly](https://github.com/composewell/streamly) but it can be used in
-- general.
--
-- To use this plugin, add this package to your @build-depends@
-- and pass the following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin
-- @

-- $impl
--
-- The plugin runs after the simplifier phase 0. It finds all non recursive
-- join point bindings whose definition begins with a case match on a type that
-- is annotated with 'Fuse'. It then sets AlwaysInlinePragma on those
-- bindings. This is followed by two runs of a gentle simplify pass that does
-- both inlining and case-of-case. This is followed by the rest of CoreToDos.

-- TODO:
--
-- This inlining could further create a recursive join point that does an
-- explicit case match on a type that would benefit again from inlining, so in
-- the second run we should create a loop breaker and transform the recursive
-- join point to a non-recursive join point and inline. This is not currently
-- done, the machinery is already available, just create a loop breaker for Let
-- Rec in `setInlineOnBndrs`.

-- $results
--
-- This plugin has been used extensively in the streaming library
-- [streamly](https://github.com/composewell/streamly).  Several file IO
-- benchmarks have shown 2x-6x improvements. With the use of this plugin stream
-- fusion in streamly has become much more predictable which has been verified
-- by inspecting the core generated by GHC and by inspection testing for the
-- presence of the stream state constructors.

#if MIN_VERSION_ghc(8,6,0)

-------------------------------------------------------------------------------
-- Set always INLINE on a binder
-------------------------------------------------------------------------------

unfoldCompulsory :: Arity -> Unfolding -> Unfolding
unfoldCompulsory arity cuf@CoreUnfolding{} =
    cuf {uf_src=InlineStable, uf_guidance = UnfWhen arity True True}
unfoldCompulsory _ x = x -- NoUnfolding

-- Sets the inline pragma on a bndr, and forgets the unfolding.
setAlwaysInlineOnBndr :: CoreBndr -> CoreBndr
setAlwaysInlineOnBndr n =
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
     in lazySetIdInfo n info'

--TODO: Replace self-recursive definitions with a loop breaker.
setInlineOnBndrs :: [CoreBndr] -> CoreBind -> CoreBind
setInlineOnBndrs bndrs = everywhere $ mkT go
  where
    go :: CoreBind -> CoreBind
    go (NonRec nn expr1)
        | any (nn ==) bndrs = NonRec (setAlwaysInlineOnBndr nn) expr1
    go x = x

-------------------------------------------------------------------------------
-- Inspect case alternatives for interesting constructor matches
-------------------------------------------------------------------------------

-- Checks whether a case alternative contains a type with the
-- annotation.  Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn :: UniqFM [Fuse] -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
altsContainsAnn _ [] = Nothing
altsContainsAnn anns (bndr@(DataAlt dcon, _, _):_) =
    case lookupUFM anns (getUnique $ dataConTyCon dcon) of
        Nothing -> Nothing
        Just _ -> Just bndr
altsContainsAnn anns ((DEFAULT, _, _):alts) = altsContainsAnn anns alts
altsContainsAnn _ _ = Nothing

-------------------------------------------------------------------------------
-- Determine if a let binder contains a case match on an annotated type
-------------------------------------------------------------------------------

-- XXX Can check the call site and return only those that would enable
-- case-of-known constructor to kick in. Or is that not relevant?
--
-- | Returns the Bndrs, that are either of the form:
--
-- joinrec { $w$g0 x y z = case y of predicateAlt -> ... } -> returns [$w$go]
-- join { $j1_sGH1 x y z = case y of predicateAlt -> ... } -> returns [$j1_sGH1]
--
-- Returns all the binds in the hierarchy from the parent to the bind
-- containing the case alternative as well as the case alternative scrutinizing
-- the annotated type.
letBndrsThatAreCases
    :: ([Alt CoreBndr] -> Maybe (Alt CoreBndr))
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
letBndrsThatAreCases f bind = goLet [] bind
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
    go :: [CoreBind] -> Bool -> CoreExpr -> [([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents True (Case _ _ _ alts) =
        let binders = alts >>= (\(_, _, expr1) -> go parents False expr1)
        in case f alts of
            Just x -> (parents, x) : binders
            Nothing -> binders

    -- Only traverse the Alt expressions of the case to discover new let
    -- bindings. Do not match for annotated constructors in the Alts.
    go parents False (Case _ _ _ alts) =
        alts >>= (\(_, _, expr1) -> go parents False expr1)

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents _ (Let bndr expr1) =    goLet parents bndr
                                    ++ go parents False expr1

    -- Traverse these to discover new let bindings
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

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Alt CoreBndr)]
    -- Here we pass the second argument to "go" as "True" i.e. we are no
    -- looking to match the case alternatives for annotated constructors.
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) True expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

containsAnns
    :: ([Alt CoreBndr] -> Maybe (Alt CoreBndr))
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
containsAnns f bind =
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    goLet [] bind
  where
    go :: [CoreBind] -> CoreExpr -> [([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents (Case _ _ _ alts) =
        let binders = alts >>= (\(_, _, expr1) -> go parents expr1)
        in case f alts of
            Just x -> (parents, x) : binders
            Nothing -> binders

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- There are no let bindings in these.
    go _ (Var _) = []
    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Alt CoreBndr)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

data ReportMode = ReportSilent | ReportWarn | ReportVerbose | ReportVerbose2

getNonRecBinder :: CoreBind -> CoreBndr
getNonRecBinder (NonRec b _) = b
getNonRecBinder (Rec _) = error "markInline: expecting only nonrec binders"

-- XXX we can possibly have a FUSE_DEBUG annotation to print verbose
-- messages only for a given type.
--
-- XXX we mark certain functions (e.g. toStreamK) with a NOFUSION
-- annotation so that we do not report them.

addMissingUnique :: (Outputable a, Uniquable a) => DynFlags -> a -> String
addMissingUnique dflags bndr =
    let suffix = showSDoc dflags $ ppr (getUnique bndr)
        bndrName = showSDoc dflags $ ppr bndr
    in if DL.isSuffixOf suffix bndrName
       then bndrName
       else bndrName ++ "_" ++ suffix

showInfo
    :: CoreBndr
    -> DynFlags
    -> ReportMode
    -> Bool
    -> [CoreBndr]
    -> [([CoreBind], Alt CoreBndr)]
    -> CoreM ()
showInfo parent dflags reportMode failIt uniqBinders annotated =
    when (uniqBinders /= []) $ do
        let showDetails (binds, c@(con,_,_)) =
                let path = DL.intercalate "/"
                        $ reverse
                        $ map (addMissingUnique dflags)
                        $ map getNonRecBinder binds
                in path ++ ": " ++
                    case reportMode of
                        ReportWarn -> showSDoc dflags (ppr con)
                        ReportVerbose -> showSDoc dflags (ppr c)
                        ReportVerbose2 ->
                            showSDoc dflags (ppr $ head binds)
                        _ -> error "transformBind: unreachable"
        let msg = "In "
                  ++ addMissingUnique dflags parent
                  ++ " binders "
                  ++ show (map (addMissingUnique dflags) (uniqBinders))
                  ++ " scrutinize data types annotated with "
                  ++ showSDoc dflags (ppr Fuse)
        case reportMode of
            ReportSilent -> return ()
            _ -> do
                putMsgS msg
                putMsgS $ DL.unlines $ map showDetails annotated
        when failIt $ error "failing"

markInline :: ReportMode -> Bool -> Bool -> ModGuts -> CoreM ModGuts
markInline reportMode failIt transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns)
    then bindsOnlyPass (mapM (transformBind dflags anns)) guts
    else return guts
  where
    transformBind :: DynFlags -> UniqFM [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bind@(NonRec b _) = do
        let annotated = letBndrsThatAreCases (altsContainsAnn anns) bind
        let uniqBinders = DL.nub (map (getNonRecBinder. head . fst) annotated)

        when (uniqBinders /= []) $
            showInfo b dflags reportMode failIt uniqBinders annotated

        let bind' =
                if transform
                then setInlineOnBndrs uniqBinders bind
                else bind
        return bind'

    transformBind _ _ bndr =
        -- This is probably wrong, but we don't need it for now.
        --mapM_ (\(b, expr) -> transformBind dflags anns (NonRec b expr)) bs
        return bndr

reportAnns :: ReportMode -> ModGuts -> CoreM ModGuts
reportAnns reportMode guts = do
    putMsgS $ "fusion-plugin: Checking presence of [Fuse] annotated types..."
    dflags <- getDynFlags
    anns <- getAnnotations deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns)
    then bindsOnlyPass (mapM (transformBind dflags anns)) guts
    else return guts
  where
    transformBind :: DynFlags -> UniqFM [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bind@(NonRec b _) = do
        let parentName = showSDoc dflags (ppr b)
        when ("main" `DL.isPrefixOf` parentName) $ do
            let annotated = containsAnns (altsContainsAnn anns) bind
            let uniqBinders = DL.nub (map (getNonRecBinder . head . fst) annotated)
            when (uniqBinders /= []) $
                showInfo b dflags reportMode False uniqBinders annotated
        return bind

    transformBind _ _ bndr = return bndr

-------------------------------------------------------------------------------
-- Install our plugin core pass
-------------------------------------------------------------------------------

-- Inserts the given list of 'CoreToDo' after the simplifier phase @n@.
insertAfterSimplPhase
    :: Int -> [CoreToDo] -> [CoreToDo] -> CoreToDo -> [CoreToDo]
insertAfterSimplPhase i ctds todos' report = go ctds ++ [report]
  where
    go [] = []
    go (todo@(CoreDoSimplify _ SimplMode {sm_phase = Phase o}):todos)
        | o == i = todo : (todos' ++ todos)
        | otherwise = todo : go todos
    go (todo:todos) = todo : go todos

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    dflags <- getDynFlags
    let doMarkInline opt failIt transform =
            CoreDoPluginPass "Inline Join Points"
                             (markInline opt failIt transform)
        simplsimplify =
            CoreDoSimplify
                (maxSimplIterations dflags)
                SimplMode
                    { sm_phase = InitialPhase
                    , sm_names = ["Fusion Plugin Inlining"]
                    , sm_dflags = dflags
                    , sm_rules = gopt Opt_EnableRewriteRules dflags
                    , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
                    , sm_inline = True
                    , sm_case_case = True
                    }
    -- We run our plugin once the simplifier finishes phase 0,
    -- followed by a gentle simplifier which inlines and case-cases
    -- twice.
    --
    -- TODO: The gentle simplifier runs on the whole program,
    -- however it might be better to call `simplifyExpr` on the
    -- expression directly.
    --
    -- TODO do not run simplify if we did not do anything in markInline phase.
    return $
        insertAfterSimplPhase
            0
            todos
            [ doMarkInline ReportSilent False True
            , simplsimplify
            , doMarkInline ReportSilent False True
            , simplsimplify
            ]
            (CoreDoPluginPass "Check fusion" (reportAnns ReportWarn))
#else
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    putMsgS "Warning! fusion-plugin does nothing on ghc versions prior to 8.6"
    return todos
#endif

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}

-- Orphan instance for 'Fuse'
instance Outputable Fuse where
    ppr _ = text "Fuse"
