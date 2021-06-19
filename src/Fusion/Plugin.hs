-- |
-- Module      : Fusion.Plugin
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
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

#if MIN_VERSION_ghc(8,6,0)
-- Explicit/qualified imports
import Control.Monad (mzero, when)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe (mapMaybe)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)

#if MIN_VERSION_ghc(9,2,0)
import Data.Char (isSpace)
import Text.Printf (printf)
import GHC.Core.Ppr (pprCoreBindingsWithSize, pprRules)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.Name.Ppr (mkPrintUnqualified)
import GHC.Utils.Logger (Logger)
#endif

#if MIN_VERSION_ghc(9,3,0)
import GHC.Utils.Logger (putDumpFile, logFlags)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Logger (putDumpMsg)
#elif MIN_VERSION_ghc(9,0,0)
-- dump core option not supported
#else
import Control.Monad (unless)
import Data.Char (isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hSetEncoding, utf8)
import Text.Printf (printf)
import ErrUtils (mkDumpDoc, Severity(..))
import PprCore (pprCoreBindingsWithSize, pprRules)
import qualified Data.Set as Set
#endif

import qualified Data.List as DL
#endif

-- Implicit imports

#if MIN_VERSION_ghc(9,2,0)
import GHC.Plugins
import qualified GHC.Plugins as GhcPlugins
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

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
--
-- The following currently works only for GHC versions less than 9.0.
--
-- To dump the core after each core to core transformation, pass the
-- following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin -fplugin-opt=Fusion.Plugin:dump-core
-- @
-- Output from each transformation is then printed in a different file.

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
-- Commandline parsing lifted from streamly/benchmark/Chart.hs
-------------------------------------------------------------------------------

data ReportMode =
      ReportSilent
    | ReportWarn
    | ReportVerbose
    | ReportVerbose1
    | ReportVerbose2
    deriving (Show)

data Options = Options
    { optionsDumpCore :: Bool
    , optionsVerbosityLevel :: ReportMode
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optionsDumpCore = False
    , optionsVerbosityLevel = ReportSilent
    }

setDumpCore :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCore val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCore = val })

setVerbosityLevel :: Monad m
    => ReportMode -> StateT ([CommandLineOption], Options) m ()
setVerbosityLevel val = do
    (args, opts) <- get
    put (args, opts { optionsVerbosityLevel = val })

-- Like the shell "shift" to shift the command line arguments
shift :: StateT ([String], Options) (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

-- totally imperative style option parsing
parseOptions :: [CommandLineOption] -> IO Options
parseOptions args = do
    maybeOptions <- runMaybeT
                        $ flip evalStateT (args, defaultOptions)
                        $ do parseLoop
                             fmap snd get
    return $ maybe defaultOptions id maybeOptions

    where

    parseOpt opt =
        case opt of
            "dump-core" -> setDumpCore True
            "verbose=1" -> setVerbosityLevel ReportWarn
            "verbose=2" -> setVerbosityLevel ReportVerbose
            "verbose=3" -> setVerbosityLevel ReportVerbose1
            "verbose=4" -> setVerbosityLevel ReportVerbose2
            str -> do
                liftIO
                    $ putStrLn
                    $ "fusion-plugin: Unrecognized option - \"" ++ str ++ "\""
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

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
-- | Set inline on specific binders inside a given bind.
setInlineOnBndrs :: [CoreBndr] -> CoreBind -> CoreBind
setInlineOnBndrs bndrs = everywhere $ mkT go
  where
    go :: CoreBind -> CoreBind
    go (NonRec b expr) | any (b ==) bndrs =
        NonRec (setAlwaysInlineOnBndr b) expr
    go x = x

#if MIN_VERSION_ghc(9,0,0)
#define IS_ACTIVE isActive (Phase 0)
#define UNIQ_FM UniqFM Name [Fuse]
#define GET_NAME getName
#define FMAP_SND fmap snd $
#else
#define IS_ACTIVE isActiveIn 0
#define UNIQ_FM UniqFM [Fuse]
#define GET_NAME getUnique
#define FMAP_SND
#endif

hasInlineBinder :: CoreBndr -> Bool
hasInlineBinder bndr =
    let inl = inlinePragInfo $ idInfo bndr
    in isInlinePragma inl && IS_ACTIVE (inlinePragmaActivation inl)

-------------------------------------------------------------------------------
-- Inspect case alternatives for interesting constructor matches
-------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,2,0)
#define ALT_CONSTR(x,y,z) Alt (x) y z
#else
#define ALT_CONSTR(x,y,z) (x, y, z)
#endif

-- Checks whether a case alternative contains a type with the
-- annotation.  Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn :: UNIQ_FM -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
altsContainsAnn _ [] = Nothing
altsContainsAnn anns (bndr@(ALT_CONSTR(DataAlt dcon,_,_)):_) =
    case lookupUFM anns (GET_NAME $ dataConTyCon dcon) of
        Nothing -> Nothing
        Just _ -> Just bndr
altsContainsAnn anns ((ALT_CONSTR(DEFAULT,_,_)):alts) = altsContainsAnn anns alts
altsContainsAnn _ _ = Nothing

getNonRecBinder :: CoreBind -> CoreBndr
getNonRecBinder (NonRec b _) = b
getNonRecBinder (Rec _) = error "markInline: expecting only nonrec binders"

needInlineCaseAlt
    :: CoreBind -> UNIQ_FM -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
needInlineCaseAlt parent anns bndr =
    case altsContainsAnn anns bndr of
        Just alt | not (hasInlineBinder $ getNonRecBinder parent) -> Just alt
        _ -> Nothing

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
letBndrsThatAreCases
    :: UNIQ_FM
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
letBndrsThatAreCases anns bind = goLet [] bind
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
        let binders = alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents False expr1)
        in case needInlineCaseAlt (head parents) anns alts of
            Just x -> (parents, x) : binders
            Nothing -> binders

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

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Alt CoreBndr)]
    -- Here we pass the second argument to "go" as "True" i.e. we are now
    -- looking to match the case alternatives for annotated constructors.
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) True expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

needInlineConstr :: CoreBind -> UNIQ_FM -> DataCon -> Bool
needInlineConstr parent anns dcon =
    case lookupUFM anns (GET_NAME $ dataConTyCon dcon) of
        Just _ | not (hasInlineBinder $ getNonRecBinder parent) -> True
        _ -> False

-- XXX Currently this function and containsAnns are equivalent. So containsAnns
-- can be used in place of this. But we may want to restrict this to certain
-- cases and keep containsAnns unrestricted so it is kept separate for now.
--
-- | Discover binders whose return type is a fusible constructor and the
-- constructor is directly used in the binder definition rather than through an
-- identifier.
--
constructingBinders :: UNIQ_FM -> CoreBind -> [([CoreBind], DataCon)]
constructingBinders anns bind = goLet [] bind
  where
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    --
    go :: [CoreBind] -> CoreExpr -> [([CoreBind], DataCon)]

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (Case _ _ _ alts) =
        alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents expr1)
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- Check if the Var is a data constructor of interest
    go parents (Var i) =
        let needInline = needInlineConstr (head parents) anns
        in case idDetails i of
            DataConWorkId dcon | needInline dcon -> [(parents, dcon)]
            _ -> []

    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], DataCon)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

data Context = CaseAlt (Alt CoreBndr) | Constr DataCon

-- letBndrsThatAreCases restricts itself to only case matches right on
-- entry to a let. This one looks for case matches anywhere.
--
-- | Report whether data constructors of interest are case matched or returned
-- anywhere in the binders, not just case match on entry or construction on
-- return.
--
containsAnns :: UNIQ_FM -> CoreBind -> [([CoreBind], Context)]
containsAnns anns bind =
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    goLet [] bind
  where
    go :: [CoreBind] -> CoreExpr -> [([CoreBind], Context)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents (Case _ _ _ alts) =
        let binders = alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents expr1)
        in case altsContainsAnn anns alts of
            Just x -> (parents, CaseAlt x) : binders
            Nothing -> binders

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- Check if the Var is a data constructor of interest
    go parents (Var i) =
        case idDetails i of
            DataConWorkId dcon ->
                case lookupUFM anns (GET_NAME $ dataConTyCon dcon) of
                    Just _ -> [(parents, Constr dcon)]
                    Nothing -> []
            _ -> []

    -- There are no let bindings in these.
    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Context)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

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

listPath :: DynFlags -> [CoreBind] -> [Char]
listPath dflags binds =
      DL.intercalate "/"
    $ reverse
    $ map (addMissingUnique dflags)
    $ map getNonRecBinder binds

showDetailsCaseMatch
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Alt CoreBndr)
    -> String
showDetailsCaseMatch dflags reportMode (binds, c@(ALT_CONSTR(con,_,_))) =
    listPath dflags binds ++ ": " ++
        case reportMode of
            ReportVerbose -> showSDoc dflags (ppr con)
            ReportVerbose1 -> showSDoc dflags (ppr c)
            ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
            _ -> error "transformBind: unreachable"

showDetailsConstr
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], DataCon)
    -> String
showDetailsConstr dflags reportMode (binds, con) =
    listPath dflags binds ++ ": " ++
        case reportMode of
            ReportVerbose -> showSDoc dflags (ppr con)
            ReportVerbose1 -> showSDoc dflags (ppr con)
            ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
            _ -> error "transformBind: unreachable"

-- Orphan instance for 'Fuse'
instance Outputable Fuse where
    ppr _ = text "Fuse"

showInfo
    :: CoreBndr
    -> DynFlags
    -> ReportMode
    -> Bool
    -> String
    -> [CoreBndr]
    -> [([CoreBind], a)]
    -> (DynFlags -> ReportMode -> ([CoreBind], a) -> String)
    -> CoreM ()
showInfo parent dflags reportMode failIt
        tag uniqBinders annotated showDetails =
    when (uniqBinders /= []) $ do
        let mesg = "In "
                  ++ addMissingUnique dflags parent
                  ++ " binders "
                  ++ show (map (addMissingUnique dflags) (uniqBinders))
                  ++ " " ++ tag
                  ++ " data types annotated with "
                  ++ showSDoc dflags (ppr Fuse)
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> return ()
            _ -> do
                putMsgS mesg
                putMsgS $ DL.unlines
                        $ DL.nub
                        $ map (showDetails dflags reportMode) annotated
        when failIt $ error "failing"

markInline :: ReportMode -> Bool -> Bool -> ModGuts -> CoreM ModGuts
markInline reportMode failIt transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- FMAP_SND getAnnotations deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns)
    then bindsOnlyPass (mapM (transformBind dflags anns)) guts
    else return guts
  where
    -- transformBind :: DynFlags -> UniqFM Unique [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns bind@(NonRec b _) = do
        let patternMatches = letBndrsThatAreCases anns bind
        let uniqPat = DL.nub (map (getNonRecBinder. head . fst) patternMatches)

        let constrs = constructingBinders anns bind
        let uniqConstr = DL.nub (map (getNonRecBinder. head . fst) constrs)

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
                showInfo b dflags reportMode failIt "SCRUTINIZE"
                    uniqPat patternMatches showDetailsCaseMatch
                showInfo b dflags reportMode failIt "CONSTRUCT"
                    uniqConstr constrs showDetailsConstr

        let bind' = do
                let allBinders = uniqPat ++ uniqConstr
                if transform && (not $ null allBinders)
                then setInlineOnBndrs allBinders bind
                else bind
        return bind'

    transformBind dflags anns (Rec bs) = do
        fmap Rec (mapM transformAsNonRec bs)
      where
        transformAsNonRec (b, expr) = do
            r <- transformBind dflags anns (NonRec b expr)
            case r of
                NonRec b1 expr1 -> return (b1, expr1)
                _ -> error "Bug: expecting NonRec binder"

-- | Core pass to mark functions scrutinizing constructors marked with Fuse
fusionMarkInline :: ReportMode -> Bool -> Bool -> CoreToDo
fusionMarkInline opt failIt transform =
    CoreDoPluginPass "Mark for inlining" (markInline opt failIt transform)

-------------------------------------------------------------------------------
-- Simplification pass after marking inline
-------------------------------------------------------------------------------

fusionSimplify :: HscEnv -> DynFlags -> CoreToDo
fusionSimplify _hsc_env dflags =
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
#if MIN_VERSION_ghc(9,2,0)
            , sm_uf_opts = unfoldingOpts dflags
            , sm_pre_inline = gopt Opt_SimplPreInlining dflags
            , sm_logger = logger
#endif
            }

#if MIN_VERSION_ghc(9,2,0)
    where logger = hsc_logger _hsc_env
#endif

-------------------------------------------------------------------------------
-- Report unfused constructors
-------------------------------------------------------------------------------

fusionReport :: String -> ReportMode -> ModGuts -> CoreM ModGuts
fusionReport mesg reportMode guts = do
    putMsgS $ "fusion-plugin: " ++ mesg ++ "..."
    dflags <- getDynFlags
    anns <- FMAP_SND getAnnotations deserializeWithData guts
    when (anyUFM (any (== Fuse)) anns) $
        mapM_ (transformBind dflags anns) $ mg_binds guts
    return guts
  where
    transformBind :: DynFlags -> UNIQ_FM -> CoreBind -> CoreM ()
    transformBind dflags anns bind@(NonRec b _) = do
        let results = containsAnns anns bind

        let getAlts x =
                case x of
                    (bs, CaseAlt alt) -> Just (bs, alt)
                    _ -> Nothing
        let patternMatches = mapMaybe getAlts results
        let uniqBinders = DL.nub (map (getNonRecBinder . head . fst)
                                      patternMatches)

        -- let constrs = constructingBinders anns bind
        let getConstrs x =
                case x of
                    (bs, Constr con) -> Just (bs, con)
                    _ -> Nothing
        let constrs = mapMaybe getConstrs results
        let uniqConstr = DL.nub (map (getNonRecBinder. head . fst) constrs)

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
                    putMsgS "Unfused bindings:"
                    putMsgS $ DL.unlines $ DL.nub $ map (listPath dflags) allBinds
            _ -> do
                showInfo b dflags reportMode False "SCRUTINIZE"
                    uniqBinders patternMatches showDetailsCaseMatch
                showInfo b dflags reportMode False "CONSTRUCT"
                    uniqConstr constrs showDetailsConstr

    transformBind dflags anns (Rec bs) =
        mapM_ (\(b, expr) -> transformBind dflags anns (NonRec b expr)) bs

-------------------------------------------------------------------------------
-- Dump core passes
-------------------------------------------------------------------------------

-- Only for GHC versions before 9.0.0
#if !MIN_VERSION_ghc(9,0,0)
chooseDumpFile :: DynFlags -> FilePath -> Maybe FilePath
chooseDumpFile dflags suffix
        | Just prefix <- getPrefix

        = Just $ setDir (prefix ++ suffix)

        | otherwise

        = Nothing

        where getPrefix
                 -- dump file location is being forced
                 --      by the --ddump-file-prefix flag.
               | Just prefix <- dumpPrefixForce dflags
                  = Just prefix
                 -- dump file location chosen by DriverPipeline.runPipeline
               | Just prefix <- dumpPrefix dflags
                  = Just prefix
                 -- we haven't got a place to put a dump file.
               | otherwise
                  = Nothing
              setDir f = case dumpDir dflags of
                         Just d  -> d </> f
                         Nothing ->       f

-- Copied from GHC.Utils.Logger
withDumpFileHandle :: DynFlags -> FilePath -> (Maybe Handle -> IO ()) -> IO ()
withDumpFileHandle dflags suffix action = do
    let mFile = chooseDumpFile dflags suffix
    case mFile of
      Just fileName -> do
        let gdref = generatedDumps dflags
        gd <- readIORef gdref
        let append = Set.member fileName gd
            mode = if append then AppendMode else WriteMode
        unless append $
            writeIORef gdref (Set.insert fileName gd)
        createDirectoryIfMissing True (takeDirectory fileName)
        withFile fileName mode $ \handle -> do
            -- We do not want the dump file to be affected by
            -- environment variables, but instead to always use
            -- UTF8. See:
            -- https://gitlab.haskell.org/ghc/ghc/issues/10762
            hSetEncoding handle utf8
            action (Just handle)
      Nothing -> action Nothing

dumpSDocWithStyle :: PprStyle -> DynFlags -> FilePath -> String -> SDoc -> IO ()
dumpSDocWithStyle sty dflags suffix hdr doc =
    withDumpFileHandle dflags suffix writeDump
  where
    -- write dump to file
    writeDump (Just handle) = do
        doc' <- if null hdr
                then return doc
                else do t <- getCurrentTime
                        let timeStamp = if (gopt Opt_SuppressTimestamps dflags)
                                          then empty
                                          else text (show t)
                        let d = timeStamp
                                $$ blankLine
                                $$ doc
                        return $ mkDumpDoc hdr d
        defaultLogActionHPrintDoc dflags handle doc' sty

    -- write the dump to stdout
    writeDump Nothing = do
        let (doc', severity)
              | null hdr  = (doc, SevOutput)
              | otherwise = (mkDumpDoc hdr doc, SevDump)
        putLogMsg dflags NoReason severity noSrcSpan sty doc'

dumpSDoc :: DynFlags -> PrintUnqualified -> FilePath -> String -> SDoc -> IO ()
dumpSDoc dflags print_unqual
    = dumpSDocWithStyle dump_style dflags
  where dump_style = mkDumpStyle dflags print_unqual
#endif

-- dump core not supported on 9.0.0
#if __GLASGOW_HASKELL__!=900
-- Only for GHC versions >= 9.2.0
#if MIN_VERSION_ghc(9,2,0)
dumpPassResult ::
      Logger
   -> DynFlags
   -> PrintUnqualified
   -> SDoc                  -- Header
   -> SDoc                  -- Extra info to appear after header
   -> CoreProgram -> [CoreRule]
   -> IO ()
dumpPassResult logger dflags unqual hdr extra_info binds rules = do
#if MIN_VERSION_ghc(9,3,0)
    let flags = logFlags logger
    let getDumpAction = putDumpFile
#else
    let flags = dflags
    let getDumpAction = putDumpMsg
#endif
    (getDumpAction logger)
        flags dump_style Opt_D_dump_simpl title undefined dump_doc

    where

    title = showSDoc dflags hdr

    dump_style = mkDumpStyle unqual

#else

dumpPassResult :: DynFlags
               -> PrintUnqualified
               -> FilePath
               -> SDoc                  -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult dflags unqual suffix hdr extra_info binds rules = do
   dumpSDoc dflags unqual suffix (showSDoc dflags hdr) dump_doc

  where

#endif
    dump_doc  = vcat [ nest 2 extra_info
                     , blankLine
                     , pprCoreBindingsWithSize binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]

filterOutLast :: (a -> Bool) -> [a] -> [a]
filterOutLast _ [] = []
filterOutLast p [x]
    | p x = []
    | otherwise = [x]
filterOutLast p (x:xs) = x : filterOutLast p xs

dumpResult
#if MIN_VERSION_ghc(9,2,0)
    :: Logger
    -> DynFlags
#else
    :: DynFlags
#endif
    -> PrintUnqualified
    -> Int
    -> SDoc
    -> CoreProgram
    -> [CoreRule]
    -> IO ()
#if MIN_VERSION_ghc(9,2,0)
dumpResult logger dflags print_unqual counter todo binds rules =
    dumpPassResult logger dflags1 print_unqual hdr (text "") binds rules
#else
dumpResult dflags print_unqual counter todo binds rules =
    dumpPassResult dflags print_unqual suffix hdr (text "") binds rules
#endif

    where

    hdr = text "["
        GhcPlugins.<> int counter
        GhcPlugins.<> text "] "
        GhcPlugins.<> todo

    suffix = printf "%02d" counter ++ "-"
        ++ (map (\x -> if isSpace x then '-' else x)
               $ filterOutLast isSpace
               $ takeWhile (/= '(')
               $ showSDoc dflags todo)
        ++ "."

#if MIN_VERSION_ghc(9,2,0)
    dflags1 = dflags
        { dumpPrefix = fmap (++ suffix) (dumpPrefix dflags)
        , dumpPrefixForce = fmap (++ suffix) (dumpPrefixForce dflags)
        }
#endif

dumpCore :: Int -> SDoc -> ModGuts -> CoreM ModGuts
dumpCore counter todo
    guts@(ModGuts
        { mg_rdr_env = rdr_env
        , mg_binds = binds
        , mg_rules = rules
        }) = do
    dflags <- getDynFlags
    putMsgS $ "fusion-plugin: dumping core "
        ++ show counter ++ " " ++ showSDoc dflags todo

#if MIN_VERSION_ghc(9,2,0)
    hscEnv <- getHscEnv
    let logger = hsc_logger hscEnv
    let print_unqual = mkPrintUnqualified (hsc_unit_env hscEnv) rdr_env
    liftIO $ dumpResult logger dflags print_unqual counter
                todo binds rules
#else
    let print_unqual = mkPrintUnqualified dflags rdr_env
    liftIO $ dumpResult dflags print_unqual counter todo binds rules
#endif
    return guts

dumpCorePass :: Int -> SDoc -> CoreToDo
dumpCorePass counter todo =
    CoreDoPluginPass "Fusion plugin dump core" (dumpCore counter todo)

_insertDumpCore :: [CoreToDo] -> [CoreToDo]
_insertDumpCore todos = dumpCorePass 0 (text "Initial ") : go 1 todos
  where
    go _ [] = []
    go counter (todo:rest) =
        todo : dumpCorePass counter (text "After " GhcPlugins.<> ppr todo)
             : go (counter + 1) rest
#endif

-------------------------------------------------------------------------------
-- Install our plugin core pass
-------------------------------------------------------------------------------

-- | Inserts the given list of 'CoreToDo' after the simplifier phase 0.
-- A final 'CoreToDo' (for reporting) passed is executed after all the phases.
insertAfterSimplPhase0
    :: [CoreToDo] -> [CoreToDo] -> CoreToDo -> [CoreToDo]
insertAfterSimplPhase0 origTodos ourTodos report =
    go False origTodos ++ [report]
  where
    go False [] = error "Simplifier phase 0/\"main\" not found"
    go True [] = []
    go _ (todo@(CoreDoSimplify _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            }):todos)
        = todo : ourTodos ++ go True todos
    go found (todo:todos) = todo : go found todos

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = do
    options <- liftIO $ parseOptions args
    dflags <- getDynFlags
    hscEnv <- getHscEnv
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
#if __GLASGOW_HASKELL__!=900
        (if optionsDumpCore options
         then _insertDumpCore
         else id) $
#endif
        insertAfterSimplPhase0
            todos
            [ fusionMarkInline ReportSilent False True
            , fusionSimplify hscEnv dflags
            , fusionMarkInline ReportSilent False True
            , fusionSimplify hscEnv dflags
            , fusionMarkInline ReportSilent False True
            , fusionSimplify hscEnv dflags
            -- This lets us know what was left unfused after all the inlining
            -- and case-of-case transformations.
            , let mesg = "Check unfused (post inlining)"
              in CoreDoPluginPass mesg (fusionReport mesg ReportSilent)
            ]
            (let mesg = "Check unfused (final)"
                 report = fusionReport mesg (optionsVerbosityLevel options)
            in CoreDoPluginPass mesg report)
#else
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    putMsgS "Warning! fusion-plugin does nothing on ghc versions prior to 8.6"
    return todos
#endif

plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = install
#if MIN_VERSION_ghc(8,6,0)
    , pluginRecompile = purePlugin
#endif
    }
