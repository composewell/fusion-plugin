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
{-# OPTIONS_GHC -Wno-x-partial #-}
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
-- Imports for all compiler versions
import Control.Monad (mzero, when)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Data (Data)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Word (Word8)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Debug.Trace (trace)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH.Syntax as TH

-- DumpCore annotation related imports (available on all supported GHC versions)
import Data.Char (isDigit)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Imports for specific compiler versions
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Lint.Interactive (interactiveInScope)
import GHC.Core.Opt.Simplify.Env (SimplMode(..))
import GHC.Core.Opt.Simplify (SimplifyOpts(..))
import GHC.Driver.Config.Core.Opt.Simplify (initSimplMode, initSimplifyOpts)
#endif

#if MIN_VERSION_ghc(9,14,0)
import GHC.Unit.Env (ue_hpt)
import GHC.Unit.Home.ModInfo (HomeModInfo(..))
import GHC.Unit.Home.PackageTable (concatHpt)
import GHC.Unit.Module.ModDetails (ModDetails(..))
#endif

#if MIN_VERSION_ghc(9,6,0)
#elif MIN_VERSION_ghc(9,2,0)
import Data.Char (isSpace)
import Text.Printf (printf)
import GHC.Core.Ppr (pprCoreBindingsWithSize, pprRules)
import GHC.Types.Name.Ppr (mkPrintUnqualified)
import GHC.Utils.Logger (Logger)
#endif

-- dump-core option related imports
#if MIN_VERSION_ghc(9,6,0)
#elif MIN_VERSION_ghc(9,3,0)
import GHC.Utils.Logger (putDumpFile, logFlags, LogFlags(..))
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
#endif

-- Implicit imports
#if MIN_VERSION_ghc(9,6,0)
import GHC.Plugins
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
import qualified GHC.Plugins as GhcPlugins
#else
import GhcPlugins
#endif

-- Core size reporting
#if MIN_VERSION_ghc(9,0,0)
import GHC.Core.Stats (exprStats, CoreStats(cs_tm))
#else
import CoreStats (exprStats, CoreStats(cs_tm))
#endif

-- Imports from fusion-plugin-types
import Fusion.Plugin.Types
    ( Fuse(..), FuseTypes(..), NoFuseTypes(..), InspectTypes(..)
    , InspectTypeClasses(..), MaxCoreSize(..)
    , DumpCore(..)
    )

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
--
-- 'Fuse' marks a type as fusible everywhere it is used. To make a type act as
-- fusible only while inlining inside one specific binding (and nowhere else),
-- annotate that binding with 'Fusion.Plugin.Types.FuseTypes' instead:
--
-- @
-- {-\# LANGUAGE TemplateHaskellQuotes #-}
--
-- {-\# ANN myFunction (FuseTypes [''Step]) #-}
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
-- Debug stuff
-------------------------------------------------------------------------------

-- XXX Can use the debugLevel from dflags
-- Increase this level to see debug output
dbgLevel :: Int
dbgLevel = 0

debug :: Int -> String -> a -> a
debug level str x =
    if dbgLevel >= level
    then trace str x
    else x

showBndr :: Outputable a => DynFlags -> a -> String
showBndr dflags bndr = showSDoc dflags $ ppr bndr

showWithUnique :: (Outputable a, Uniquable a) => DynFlags -> a -> String
showWithUnique dflags bndr =
    let suffix = showSDoc dflags $ ppr (getUnique bndr)
        bndrName = showBndr dflags bndr
    in if DL.isSuffixOf suffix bndrName
       then bndrName
       else bndrName ++ "_" ++ suffix

listPath :: DynFlags -> [CoreBind] -> [Char]
listPath dflags binds =
      DL.intercalate "/"
    $ reverse
    $ map (showWithUnique dflags)
    $ map getNonRecBinder binds

-------------------------------------------------------------------------------
-- Commandline parsing lifted from streamly/benchmark/Chart.hs
-------------------------------------------------------------------------------

data ReportMode =
      ReportSilent
    | ReportWarn
    | ReportVerbose
    | ReportVerbose1
    | ReportVerbose2
    deriving (Eq, Show)

data Options = Options
    { optionsDumpCore :: Bool
    , optionsVerbosityLevel :: ReportMode
    , optionsWError :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optionsDumpCore = False
    , optionsVerbosityLevel = ReportSilent
    , optionsWError = False
    }

setDumpCore :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCore val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCore = val })

setWError :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setWError val = do
    (args, opts) <- get
    put (args, opts { optionsWError = val })

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
            "werror" -> setWError True
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

-- Keyed by 'OccName' string rather than by 'Name'/'Unique'. A top-level Id's
-- Unique -- and even its 'NameSort' -- is not guaranteed to survive the
-- Core-to-core passes while OccName stays the same.
#define INSPECT_FM Map.Map String InspectTypes
#define INSPECT_CLASSES_FM Map.Map String InspectTypeClasses
#define FUSE_TYPES_FM Map.Map String FuseTypes
#define NO_FUSE_TYPES_FM Map.Map String NoFuseTypes
#define MAX_CORE_SIZE_FM Map.Map String MaxCoreSize
#define DUMP_CORE_FM Map.Map String DumpCore

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

-- Checks whether a case alternative contains a type for which the given
-- predicate is true. Only checks the first typed element in the list, so
-- only pass alternatives from one case expression.
altsContainsAnn ::
    DynFlags -> (Name -> Bool) -> [Alt CoreBndr] -> Maybe (Alt CoreBndr)
altsContainsAnn _ _ [] = Nothing
altsContainsAnn _ _ ((ALT_CONSTR(DEFAULT,_,_)):[]) =
    debug 2 "Case trivial default" Nothing
altsContainsAnn dflags isInteresting (bndr@(ALT_CONSTR(DataAlt dcon,_,_)):_) =
    let name = GET_NAME $ dataConTyCon dcon
        mesg = "Case DataAlt type " ++ showWithUnique dflags name
    in if isInteresting name
       then debug 2 (mesg ++ " annotated") (Just bndr)
       else debug 2 (mesg ++ " not annotated") Nothing
altsContainsAnn dflags isInteresting ((ALT_CONSTR(DEFAULT,_,_)):alts) =
    altsContainsAnn dflags isInteresting alts
altsContainsAnn _ _ ((ALT_CONSTR(LitAlt _,_,_)):_) =
    debug 2 "Case LitAlt" Nothing

getNonRecBinder :: CoreBind -> CoreBndr
getNonRecBinder (NonRec b _) = b
getNonRecBinder (Rec _) = error "markInline: expecting only nonrec binders"

needInlineCaseAlt
    :: DynFlags
    -> [CoreBind]
    -> UNIQ_FM
    -> [Alt CoreBndr]
    -> Maybe (Alt CoreBndr)
needInlineCaseAlt dflags parents anns bndr =
    let mesg = "Binder: " ++ listPath dflags parents
    in if not (hasInlineBinder $ getNonRecBinder (head parents))
       then
            debug 2
                (mesg ++ " not inlined")
                $ case altsContainsAnn dflags (isJust . lookupUFM anns) bndr of
                    Just alt -> Just alt
                    _ -> Nothing
       else debug 2 (mesg ++ " already inlined") Nothing

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
    :: DynFlags
    -> UNIQ_FM
    -> CoreBind
    -> [([CoreBind], Alt CoreBndr)]
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
    go :: [CoreBind] -> Bool -> CoreExpr -> [([CoreBind], Alt CoreBndr)]

    -- Match and record the case alternative if it contains a constructor
    -- annotated with "Fuse" and traverse the Alt expressions to discover more
    -- let bindings.
    go parents True (Case _ _ _ alts) =
        let result = alts >>= (\(ALT_CONSTR(_,_,expr1)) -> go parents False expr1)
        in case needInlineCaseAlt dflags parents anns alts of
            Just x -> (parents, x) : result
            Nothing -> result

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

needInlineTyCon :: CoreBind -> UNIQ_FM -> TyCon -> Bool
needInlineTyCon parent anns tycon =
    case lookupUFM anns (GET_NAME tycon) of
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
constructingBinders :: UNIQ_FM -> CoreBind -> [([CoreBind], Id)]
constructingBinders anns bind = goLet [] bind
  where
    -- The first argument is current binder and its parent chain. We add a new
    -- element to this path when we enter a let statement.
    --
    go :: [CoreBind] -> CoreExpr -> [([CoreBind], Id)]

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
        let needInline = needInlineTyCon (head parents) anns
        in case tyConAppTyConPicky_maybe (varType i) of
            Just tycon | needInline tycon -> [(parents, i)]
            _ -> []

    go _ (Lit _) = []
    go _ (Tick _ _) = []
    go _ (Type _) = []
    go _ (Coercion _) = []

    goLet :: [CoreBind] -> CoreBind -> [([CoreBind], Id)]
    goLet parents bndr@(NonRec _ expr1) = go (bndr : parents) expr1
    goLet parents (Rec bs) =
        bs >>= (\(b, expr1) -> goLet parents $ NonRec b expr1)

data Context = CaseAlt (Alt CoreBndr) | Constr Id

-- letBndrsThatAreCases restricts itself to only case matches right on
-- entry to a let. This one looks for case matches anywhere.
--
-- | Report whether data constructors of interest are case matched or returned
-- anywhere in the binders, not just case match on entry or construction on
-- return.
--
containsAnns
    :: DynFlags -> (Name -> Bool) -> CoreBind -> [([CoreBind], Context)]
containsAnns dflags isInteresting bind =
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
        in case altsContainsAnn dflags isInteresting alts of
            Just x -> (parents, CaseAlt x) : binders
            Nothing -> binders

    -- Enter a new let binding inside the current expression and traverse the
    -- let expression as well.
    go parents (Let bndr expr1) = goLet parents bndr ++ go parents expr1

    -- Traverse these to discover new let bindings
    go parents (App expr1 expr2) = go parents expr1 ++ go parents expr2
    go parents (Lam _ expr1) = go parents expr1
    go parents (Cast expr1 _) = go parents expr1

    -- Check if the Var is of the type of a data constructor of interest
    go parents (Var i) =
        case tyConAppTyConPicky_maybe (varType i) of
            Just tycon | isInteresting (GET_NAME tycon) ->
                [(parents, Constr i)]
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

contextTyConName :: Context -> Maybe Name
contextTyConName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (GET_NAME $ dataConTyCon dcon)
contextTyConName (CaseAlt _) = Nothing
contextTyConName (Constr con) =
    GET_NAME <$> tyConAppTyConPicky_maybe (varType con)

-- | Like 'contextTyConName' but yields the fully-qualified @Module.Type@ name
-- (via 'qualifiedTyConName') used in reports rather than the raw 'Name'.
contextQualifiedName :: Context -> Maybe String
contextQualifiedName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (qualifiedTyConName (dataConTyCon dcon))
contextQualifiedName (CaseAlt _) = Nothing
contextQualifiedName (Constr con) =
    qualifiedTyConName <$> tyConAppTyConPicky_maybe (varType con)

-- | Drop any hit whose TyCon 'Name' is in the given exclusion list.
filterExcluded
    :: [Name] -> [([CoreBind], Context)] -> [([CoreBind], Context)]
filterExcluded excl =
    filter (\(_, ctx) -> maybe True (`notElem` excl) (contextTyConName ctx))

-- | True for TyCons whose values GHC never heap-allocates: unboxed
-- primitives (e.g. 'Int#', 'State#'), unboxed tuples/sums, and true
-- enumeration types (every data constructor nullary, e.g. '()', 'Bool')
-- which are compiled as statically shared singletons.
isNonAllocatingTyCon :: TyCon -> Bool
isNonAllocatingTyCon tycon =
    isPrimTyCon tycon
    || isUnboxedTupleTyCon tycon
    || isUnboxedSumTyCon tycon
    || isEnumerationTyCon tycon

-- | False for hits that can never represent leftover boxing: a case match
-- or construction of a non-allocating type (see 'isNonAllocatingTyCon'), or
-- a bare reference to something of function type (e.g. a primop like
-- \"+#\", or a specialized worker) which is not a data construction at
-- all. Applied unconditionally, regardless of which types the active
-- 'InspectTypes' predicate flags as \"interesting\" -- these are never useful
-- signal for a boxing/fusion report.
isAllocating :: Context -> Bool
isAllocating (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    not (isNonAllocatingTyCon (dataConTyCon dcon))
isAllocating (CaseAlt _) = True
isAllocating (Constr con) =
    not (isFunTy (varType con))
    && maybe True (not . isNonAllocatingTyCon)
             (tyConAppTyConPicky_maybe (varType con))

-- | Drop hits that can never represent leftover boxing (see 'isAllocating').
filterNonAllocating
    :: [([CoreBind], Context)] -> [([CoreBind], Context)]
filterNonAllocating = filter (isAllocating . snd)

-- | Like GHC 'getAnnotations' but keyed by 'OccName' string instead of by
-- 'Name' (i.e. by 'Unique'). 'getAnnotations' folds annotations into a
-- 'NameEnv', and it stores only an 'Int' key internally. We read 'mg_anns'
-- directly: it is populated once from the module's '{-# ANN #-}' pragmas
-- (before any Core-to-core pass runs) and it is never rewritten by any
-- 'CoreToDo', so the 'Name's inside it are exactly as resolved by the renamer,
-- unaffected by any later binder cloning/renaming.
--
-- At most one annotation of this type is permitted per binding: a binding
-- carrying more than one is a compile error (the @annName@ argument names the
-- annotation type in that message).
getAnnotationsByStableName
    :: Data a => String -> ([Word8] -> a) -> ModGuts -> CoreM (Map.Map String a)
getAnnotationsByStableName annName deserialize guts =
    Map.traverseWithKey single
        (Map.fromListWith (++) (mapMaybe annPair (mg_anns guts)))

    where

    annPair (Annotation (NamedTarget name) payload) =
        (\v -> (getOccString name, [v])) <$> fromSerialized deserialize payload
    annPair _ = Nothing

    single _ [v] = return v
    single name _ =
        error $ "fusion-plugin: the binding '" ++ name
              ++ "' has more than one " ++ annName
              ++ " annotation; at most one " ++ annName
              ++ " annotation is allowed per binding."

-- | Resolve a list of Template Haskell 'TH.Name's to GHC 'Name's.
resolveTHNames :: [TH.Name] -> CoreM [Name]
resolveTHNames = fmap catMaybes . mapM thNameToGhcName

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

-- | Build the "isInteresting" predicate and the exclusion list for a given
-- 'InspectTypes' directive.
inspectPredicate :: UNIQ_FM -> InspectTypes -> CoreM (Name -> Bool, [Name])
inspectPredicate _ (ForbidTypes thNames) = do
    names <- resolveTHNames thNames
    return (\n -> n `elem` names, [])
inspectPredicate anns (ForbidFused thForbid thAllow) = do
    forbidden <- resolveTHNames thForbid
    allowed <- resolveTHNames thAllow
    return (\n -> isJust (lookupUFM anns n) || n `elem` forbidden, allowed)
inspectPredicate _ (PermitTypes thAllow) = do
    allowed <- resolveTHNames thAllow
    return (const True, allowed)

-------------------------------------------------------------------------------
-- Core-to-core pass to mark interesting binders to be always inlined
-------------------------------------------------------------------------------

-- XXX we can possibly have a FUSE_DEBUG annotation to print verbose
-- messages only for a given type.
--
-- XXX we mark certain functions (e.g. toStreamK) with a NOFUSION
-- annotation so that we do not report them.

showDetailsCaseMatch
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Alt CoreBndr)
    -> String
showDetailsCaseMatch dflags reportMode (binds, c@(ALT_CONSTR(con,_,_))) =
    let vstr =
            case reportMode of
                ReportVerbose -> showSDoc dflags (ppr con)
                ReportVerbose1 -> showSDoc dflags (ppr c)
                ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
                _ -> error "transformBind: unreachable"
        tstr =
            case con of
                DataAlt dcon ->
                    " :: " ++ qualifiedTyConName (dataConTyCon dcon)
                _ -> ""
    in listPath dflags binds ++ ": " ++ vstr ++ tstr

-- | Show a 'TyCon' fully qualified as @Module.Name@ so that types with the
-- same unqualified name defined in different modules (e.g. several @Step@
-- types) can be told apart in a report. Wired-in types with no defining
-- module are shown by their bare name.
qualifiedTyConName :: TyCon -> String
qualifiedTyConName tc =
    let name = getName tc
    in case nameModule_maybe name of
        Just m ->
            moduleNameString (moduleName m) ++ "." ++ getOccString name
        Nothing -> getOccString name

showDetailsConstr
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Id)
    -> String
showDetailsConstr dflags reportMode (binds, con) =
    let t = tyConAppTyConPicky_maybe (varType con)
        vstr =
            case reportMode of
                ReportVerbose -> showSDoc dflags (ppr con)
                ReportVerbose1 -> showSDoc dflags (ppr con)
                ReportVerbose2 -> showSDoc dflags (ppr $ head binds)
                _ -> error "transformBind: unreachable"
        tstr =
            case t of
                Nothing -> " :: Not a Type Constructor"
                Just x -> " :: " ++ qualifiedTyConName x
    in listPath dflags binds ++ ": " ++ vstr ++ tstr

-- Orphan instance for 'Fuse'
instance Outputable Fuse where
    ppr _ = text "Fuse"

-- Orphan instance for 'InspectTypes', used only to print a banner naming the
-- directive that triggered a focused report; TH 'TH.Name's are shown via
-- their derived 'Show' instance rather than GHC's 'Outputable' (which has
-- no instance for 'TH.Name').
instance Outputable InspectTypes where
    ppr (ForbidTypes names) = text "ForbidTypes" <+> text (show names)
    ppr (ForbidFused f a) =
        text "ForbidFused" <+> text (show f) <+> text (show a)
    ppr (PermitTypes names) = text "PermitTypes" <+> text (show names)

-- Orphan instance for 'InspectTypeClasses', used only to print a banner naming
-- the directive that triggered a focused report.
instance Outputable InspectTypeClasses where
    ppr (ForbidTypeClasses names) =
        text "ForbidTypeClasses" <+> text (show names)
    ppr (PermitTypeClasses names) =
        text "PermitTypeClasses" <+> text (show names)

showInfo
    :: CoreBndr
    -> DynFlags
    -> ReportMode
    -> String
    -> [CoreBndr]
    -> [([CoreBind], a)]
    -> (DynFlags -> ReportMode -> ([CoreBind], a) -> String)
    -> CoreM ()
showInfo parent dflags reportMode
        tag uniqBinders annotated showDetails =
    when (uniqBinders /= []) $ do
        let mesg = "In "
                  ++ showWithUnique dflags parent
                  ++ " binders "
                  ++ show (map (showWithUnique dflags) (uniqBinders))
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

-- | If the given top level bind's own binder carries an 'InspectTypes'
-- annotation, print a report of interesting types case-matched or
-- constructed anywhere in its RHS, per the annotation's rules. No-op if
-- the binder is not annotated, or if the binding has no offending types.
--
-- The output honours the module-wide verbosity: at the default (or
-- @verbose=1@) a single terse @found forbidden types@ line is printed; at
-- @verbose=2@ and above the full "Inspecting ..." banner plus per-hit
-- @SCRUTINIZE@/@CONSTRUCT@ breakdown is printed.
-- Returns 0 on no violations and 1 otherwise.
reportInspected
    :: DynFlags -> ReportMode -> UNIQ_FM -> INSPECT_FM -> CoreBind -> CoreM Int
reportInspected dflags reportMode anns inspectAnns bind@(NonRec b _) =
    case Map.lookup (getOccString (GET_NAME b)) inspectAnns of
        Nothing -> return 0
        Just ispec -> go ispec

    where

    go ispec = do
        (isInteresting, exclusion) <- inspectPredicate anns ispec
        let results = filterNonAllocating
                    $ filterExcluded exclusion (containsAnns dflags isInteresting bind)
        if null results
        then return 0
        else do
            case reportMode of
                ReportSilent -> terse results
                ReportWarn -> terse results
                _ -> detailed ispec results
            return 1

    terse results =
        let names = DL.nub (mapMaybe (contextQualifiedName . snd) results)
        in putMsgS $ "fusion-plugin: found forbidden types in "
                   ++ getOccString (GET_NAME b)
                   ++ ": [" ++ DL.intercalate ", " names ++ "]"

    detailed ispec results = do
        putMsgS $ "fusion-plugin: Inspecting "
                ++ showWithUnique dflags b
                ++ " (" ++ showSDoc dflags (ppr ispec) ++ ")..."
        let getAlts x =
                case x of
                    (bs, CaseAlt alt) -> Just (bs, alt)
                    _ -> Nothing
            patternMatches = mapMaybe getAlts results
            uniqBinders =
                DL.nub (map (getNonRecBinder . head . fst) patternMatches)

            getConstrs x =
                case x of
                    (bs, Constr con) -> Just (bs, con)
                    _ -> Nothing
            constrs = mapMaybe getConstrs results
            uniqConstr = DL.nub (map (getNonRecBinder . head . fst) constrs)

        showInfo b dflags reportMode "SCRUTINIZE"
            uniqBinders patternMatches showDetailsCaseMatch
        showInfo b dflags reportMode "CONSTRUCT"
            uniqConstr constrs showDetailsConstr
reportInspected _ _ _ _ (Rec _) =
    error "reportInspected: expecting only NonRec binders"

-------------------------------------------------------------------------------
-- Inspect the presence/absence of type classes in a binding's Core
-------------------------------------------------------------------------------

-- | Collect the class 'TyCon's appearing anywhere in the Core of a binding. A
-- type class manifests in Core as a dictionary; its type has the class 'TyCon'
-- at the head (see 'isClassTyCon'). We gather the 'TyCon's mentioned by the
-- type of every 'Var', binder, case scrutinee and 'Type' node in the RHS and
-- keep those that are classes.
classTyConsInBind :: CoreBind -> [TyCon]
classTyConsInBind bind =
    nonDetEltsUniqSet (goBind bind)

    where

    fromType t = filterUniqSet isClassTyCon (tyConsOfType t)

    go (Var i) = fromType (varType i)
    go (Lit _) = emptyUniqSet
    go (App e1 e2) = go e1 `unionUniqSets` go e2
    go (Lam b e) = fromType (varType b) `unionUniqSets` go e
    go (Let b e) = goBind b `unionUniqSets` go e
    go (Case e b t alts) =
        go e
            `unionUniqSets` fromType (varType b)
            `unionUniqSets` fromType t
            `unionUniqSets` unionManyUniqSets (map goAlt alts)
    go (Cast e _) = go e
    go (Tick _ e) = go e
    go (Type t) = fromType t
    go (Coercion _) = emptyUniqSet

    goAlt (ALT_CONSTR(_,bs,e)) =
        unionManyUniqSets (map (fromType . varType) bs) `unionUniqSets` go e

    goBind (NonRec b e) = fromType (varType b) `unionUniqSets` go e
    goBind (Rec bs) =
        unionManyUniqSets
            (map (\(b, e) -> fromType (varType b) `unionUniqSets` go e) bs)

-- | Build the "isInteresting" predicate over class 'Name's for a given
-- 'InspectTypeClasses' directive.
inspectClassPredicate :: InspectTypeClasses -> CoreM (Name -> Bool)
inspectClassPredicate (ForbidTypeClasses thNames) = do
    names <- resolveTHNames thNames
    return (\n -> n `elem` names)
inspectClassPredicate (PermitTypeClasses thAllow) = do
    allowed <- resolveTHNames thAllow
    return (\n -> n `notElem` allowed)

-- | If the given top level bind's own binder carries an 'InspectTypeClasses'
-- annotation, print a report of the type classes present in its Core that the
-- directive flags as forbidden. No-op if the binder is not annotated, or if no
-- offending class is present.
-- Returns 0 on no violations and 1 otherwise.
reportInspectedClasses
    :: DynFlags
    -> ReportMode
    -> INSPECT_CLASSES_FM
    -> CoreBind
    -> CoreM Int
reportInspectedClasses dflags reportMode classAnns bind@(NonRec b _) =
    case Map.lookup (getOccString (GET_NAME b)) classAnns of
        Nothing -> return 0
        Just ispec -> go ispec

    where

    go ispec = do
        isInteresting <- inspectClassPredicate ispec
        let hits = filter (isInteresting . getName) (classTyConsInBind bind)
        if null hits
        then return 0
        else do
            case reportMode of
                ReportSilent -> report ispec hits
                ReportWarn -> report ispec hits
                _ -> do
                    putMsgS $ "fusion-plugin: Inspecting "
                            ++ showWithUnique dflags b
                            ++ " (" ++ showSDoc dflags (ppr ispec) ++ ")..."
                    report ispec hits
            return 1

    report _ hits =
        let names = DL.nub (map qualifiedTyConName hits)
        in putMsgS $ "fusion-plugin: found forbidden type classes in "
                   ++ getOccString (GET_NAME b)
                   ++ ": [" ++ DL.intercalate ", " names ++ "]"
reportInspectedClasses _ _ _ (Rec _) =
    error "reportInspectedClasses: expecting only NonRec binders"

-- Returns 0 on no violations and 1 otherwise.
reportCoreSize
    :: DynFlags -> ReportMode -> MAX_CORE_SIZE_FM -> CoreBind -> CoreM Int
reportCoreSize dflags reportMode sizeAnns (NonRec b rhs) =
    case Map.lookup (getOccString (GET_NAME b)) sizeAnns of
        Nothing -> return 0
        Just ann -> go ann
  where
    stats = exprStats rhs
    terms = cs_tm stats

    go (MaxCoreSize maxSize) = do
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> return ()
            _ ->
                putMsgS $ "fusion-plugin: Core size of "
                        ++ showWithUnique dflags b
                        ++ ": "
                        ++ showSDoc dflags (ppr stats)
        if terms > maxSize
        then do
            putMsgS $ "fusion-plugin: core size of "
                    ++ showWithUnique dflags b
                    ++ " (" ++ show terms
                    ++ " terms) exceeds the specified size ("
                    ++ show maxSize ++ " terms)."
            return 1
        else return 0
reportCoreSize _ _ _ (Rec _) =
    error "reportCoreSize: expecting only NonRec binders"

-- | Split a string on @\'-\'@ characters.
splitOnDash :: String -> [String]
splitOnDash s =
    case break (== '-') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitOnDash rest

-- | Recover the bare package name from a unit-id string. During a build the
-- unit id of the package being compiled looks like @my-pkg-0.2.8-inplace@ (or
-- with a hash instead of @inplace@); we strip the trailing version and any
-- component suffix, leaving just @my-pkg@. If no version-like component is
-- found the string is returned unchanged.
packageNameFromUnitId :: String -> String
packageNameFromUnitId uid =
    case break isVersion (splitOnDash uid) of
        (before, _ : _) | not (null before) -> DL.intercalate "-" before
        _ -> uid
  where
    isVersion x = not (null x) && all (\c -> isDigit c || c == '.') x

-- | The bare package name of the module currently being compiled.
modulePackageName :: Module -> String
modulePackageName =
    packageNameFromUnitId
#if MIN_VERSION_ghc(9,0,0)
        . unitString . moduleUnit
#else
        . unitIdString . moduleUnitId
#endif

-- | If the given top level bind's own binder carries a 'DumpCore' annotation,
-- write the Core of that binding to a file under the @fusion-plugin-output@
-- directory. No-op if the binder is not annotated.
reportDumpCore ::
    DynFlags -> String -> String -> DUMP_CORE_FM -> CoreBind -> CoreM ()
reportDumpCore dflags pkgName modName dumpAnns bind@(NonRec b _) =
    case Map.lookup (getOccString (GET_NAME b)) dumpAnns of
        Nothing -> return ()
        Just _ -> do
            let dir = "fusion-plugin-output" </> pkgName
                fileName =
                    modName ++ "." ++ getOccString (GET_NAME b) ++ ".dump-simpl"
                path = dir </> fileName
            liftIO $ do
                createDirectoryIfMissing True dir
                writeFile path (showSDoc dflags (ppr bind) ++ "\n")
            putMsgS $ "fusion-plugin: Dumped core of "
                    ++ showWithUnique dflags b ++ " to " ++ path
reportDumpCore _ _ _ _ (Rec _) =
    error "reportDumpCore: expecting only NonRec binders"

markInline :: Int -> ReportMode -> Bool -> ModGuts -> CoreM ModGuts
markInline pass reportMode transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- FMAP_SND getAnnotations deserializeWithData guts
    fuseTypesAnns <-
        getAnnotationsByStableName "FuseTypes" deserializeWithData guts
    noFuseTypesAnns <-
        getAnnotationsByStableName "NoFuseTypes" deserializeWithData guts
    if (anyUFM (any (== Fuse)) anns || not (Map.null fuseTypesAnns))
    then do
        r <- bindsOnlyPass
                (mapM
                    (transformBind dflags anns fuseTypesAnns noFuseTypesAnns))
                guts
        if dbgLevel > 0
        then dumpCore 0 (text ("Fusion-plugin-" ++ show pass)) r
        else return r
    else return guts
  where
    -- transformBind :: DynFlags -> UniqFM Unique [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind dflags anns0 fuseTypesAnns noFuseTypesAnns bind@(NonRec b _) = do
        -- Types named in a 'FuseTypes' annotation on this binding act as if
        -- they were 'Fuse'-annotated, but only while inlining inside it.
        -- Types named in a 'NoFuseTypes' annotation are stripped of their
        -- 'Fuse' status locally, overriding the above and any module-wide
        -- 'Fuse' annotation for this binding only.
        anns1 <- augmentFuseTypes anns0 fuseTypesAnns b
        anns <- removeFuseTypes anns1 noFuseTypesAnns b
        let patternMatches = letBndrsThatAreCases dflags anns bind
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

    transformBind dflags anns fuseTypesAnns noFuseTypesAnns (Rec bs) = do
        fmap Rec (mapM transformAsNonRec bs)
      where
        transformAsNonRec (b, expr) = do
            r <- transformBind dflags anns fuseTypesAnns noFuseTypesAnns
                    (NonRec b expr)
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

-------------------------------------------------------------------------------
-- Report unfused constructors
-------------------------------------------------------------------------------

-- | The set of top level binders that are reachable from an exported
-- binder, and therefore guaranteed to survive into the final program.
--
-- This plugin's final check runs as the very last 'CoreToDo', but GHC's
-- "CoreTidy" runs even after that, it changes the Core and there is no way to
-- run a hook after that in the plugin. We approximate CoreTidy's reachability
-- analysis here so we can skip unreachable bindings, seeding it from the same
-- 'isExportedId' flag CoreTidy itself relies on.
--
liveTopLevelBinders :: ModGuts -> VarSet
liveTopLevelBinders guts = go initial initial

    where

    flattenBind (NonRec b e) = [(b, e)]
    flattenBind (Rec bs) = bs

    topBinds = mg_binds guts >>= flattenBind
    topBndrSet = mkVarSet (map fst topBinds)

    adjacency :: VarEnv VarSet
    adjacency = mkVarEnv
        [ (b, intersectVarSet topBndrSet (exprFreeVars rhs))
        | (b, rhs) <- topBinds
        ]

    initial = mkVarSet (filter isExportedId (map fst topBinds))

    go frontier visited
        | isEmptyVarSet frontier = visited
        | otherwise =
            let next = unionVarSets
                     $ mapMaybe (lookupVarEnv adjacency)
                     $ nonDetEltsUniqSet frontier
                newVisited = unionVarSet visited next
                newFrontier = next `minusVarSet` visited
            in go newFrontier newVisited

-- | @runInspect@ controls whether the per-binding 'InspectTypes' annotations are
-- also processed. When @werror@ is set, the build is failed (after all
-- annotation violations in the module have been reported) if any
-- annotation-check violation ('InspectTypes', 'InspectTypeClasses' or
-- 'MaxCoreSize') was found.
fusionReport :: String -> ReportMode -> Bool -> Bool -> ModGuts -> CoreM ModGuts
fusionReport mesg reportMode runInspect werror guts = do
    inspectAnns <-
        if runInspect
        then getAnnotationsByStableName "InspectTypes" deserializeWithData guts
        else return Map.empty
    sizeAnns <-
        if runInspect
        then getAnnotationsByStableName "MaxCoreSize" deserializeWithData guts
        else return Map.empty
    dumpAnns <-
        if runInspect
        then getAnnotationsByStableName "DumpCore" deserializeWithData guts
        else return Map.empty
    classAnns <-
        if runInspect
        then getAnnotationsByStableName
                 "InspectTypeClasses" deserializeWithData guts
        else return Map.empty
    let anyInspect = runInspect && not (Map.null inspectAnns)
        anyInspectClasses = runInspect && not (Map.null classAnns)
        anyMaxCoreSize = runInspect && not (Map.null sizeAnns)
        anyDumpCore = runInspect && not (Map.null dumpAnns)
        anyReport =
            anyInspect || anyInspectClasses || anyMaxCoreSize || anyDumpCore
    case reportMode of
        ReportSilent | not anyReport -> return guts
        _ -> do
            when (reportMode /= ReportSilent) $
                putMsgS $ "fusion-plugin: " ++ mesg ++ "..."
            dflags <- getDynFlags
            anns <- FMAP_SND getAnnotations deserializeWithData guts
            let liveBndrs = liveTopLevelBinders guts
            let pkgName = modulePackageName (mg_module guts)
            let modName = moduleNameString (moduleName (mg_module guts))
            violations <-
                if anyUFM (any (== Fuse)) anns || anyReport
                then fmap sum
                        $ mapM
                            (transformBind
                                dflags anns inspectAnns classAnns sizeAnns
                                dumpAnns pkgName modName liveBndrs)
                        $ mg_binds guts
                else return 0
            when (werror && violations > 0) $ do
                putMsgS $ "fusion-plugin: " ++ show violations
                        ++ " annotation violation(s) reported in " ++ modName
                        ++ "; failing the build (werror)."
                liftIO $ throwGhcExceptionIO
                    (ProgramError
                        "fusion-plugin: annotation violations found (werror)")
            return guts

    where

    -- Returns the number of annotation-check violations reported for this
    -- binding (the module-wide unfused report below is not counted).
    transformBind
        :: DynFlags -> UNIQ_FM -> INSPECT_FM -> INSPECT_CLASSES_FM
        -> MAX_CORE_SIZE_FM -> DUMP_CORE_FM
        -> String -> String -> VarSet
        -> CoreBind -> CoreM Int
    transformBind dflags anns inspectAnns classAnns sizeAnns dumpAnns
            pkgName modName liveBndrs bind@(NonRec b _) = do
        n1 <- if runInspect
              then reportInspected dflags reportMode anns inspectAnns bind
              else return 0
        n2 <- if runInspect
              then reportInspectedClasses dflags reportMode classAnns bind
              else return 0
        n3 <- if runInspect
              then reportCoreSize dflags reportMode sizeAnns bind
              else return 0
        when runInspect $ reportDumpCore dflags pkgName modName dumpAnns bind
        when (b `elemVarSet` liveBndrs) $ do
            let results = filterNonAllocating
                        $ containsAnns dflags (isJust . lookupUFM anns) bind

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
            -- paths with one entry for each binder and giving one example of
            -- what it scrutinizes and/or constructs, for example:
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
                    showInfo b dflags reportMode "SCRUTINIZE"
                        uniqBinders patternMatches showDetailsCaseMatch
                    showInfo b dflags reportMode "CONSTRUCT"
                        uniqConstr constrs showDetailsConstr
        return (n1 + n2 + n3)

    transformBind dflags anns inspectAnns classAnns sizeAnns dumpAnns
            pkgName modName liveBndrs (Rec bs) =
        fmap sum
            $ mapM
                (\(b, expr) ->
                    transformBind
                        dflags anns inspectAnns classAnns sizeAnns dumpAnns
                        pkgName modName liveBndrs (NonRec b expr))
                bs

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

-- XXX Need to fix for GHC-9.6 and above
-- dump core not supported on 9.0.0, 9.0.0 does not export Logger
#if __GLASGOW_HASKELL__!=900 && !MIN_VERSION_ghc(9,6,0)
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
    dumpPassResult logger1 dflags print_unqual hdr (text "") binds rules
#else
dumpResult dflags print_unqual counter todo binds rules =
    dumpPassResult
        dflags print_unqual (_suffix ++ "dump-simpl") hdr (text "") binds rules
#endif

    where

    hdr = text "["
        GhcPlugins.<> int counter
        GhcPlugins.<> text "] "
        GhcPlugins.<> todo

    _suffix = printf "%02d" counter ++ "-"
        ++ (map (\x -> if isSpace x then '-' else x)
               $ filterOutLast isSpace
               $ takeWhile (/= '(')
               $ showSDoc dflags todo)
        ++ "."

#if MIN_VERSION_ghc(9,4,0)
    prefix = log_dump_prefix (logFlags logger) ++ _suffix
    logger1 = logger {logFlags = (logFlags logger) {log_dump_prefix = prefix}}
#elif MIN_VERSION_ghc(9,2,0)
    logger1 = logger
#endif
#endif

#if !MIN_VERSION_ghc(9,6,0)
dumpCore :: Int -> SDoc -> ModGuts -> CoreM ModGuts
dumpCore counter title guts = do
    dflags <- getDynFlags
    putMsgS $ "fusion-plugin: dumping core "
        ++ show counter ++ " " ++ showSDoc dflags title

#if MIN_VERSION_ghc(9,2,0)
    hscEnv <- getHscEnv
    let logger = hsc_logger hscEnv
    let print_unqual =
            mkPrintUnqualified (hsc_unit_env hscEnv) (mg_rdr_env guts)
    liftIO $ dumpResult logger dflags print_unqual counter
                title (mg_binds guts) (mg_rules guts)
#elif MIN_VERSION_ghc(9,0,0)
    putMsgS $ "fusion-plugin: dump-core not supported on GHC 9.0 "
#else
    let print_unqual = mkPrintUnqualified dflags (mg_rdr_env guts)
    liftIO $ dumpResult dflags print_unqual counter
                title (mg_binds guts) (mg_rules guts)
#endif
    return guts

dumpCorePass :: Int -> SDoc -> CoreToDo
dumpCorePass counter title =
    CoreDoPluginPass "Fusion plugin dump core" (dumpCore counter title)

_insertDumpCore :: [CoreToDo] -> [CoreToDo]
_insertDumpCore todos = dumpCorePass 0 (text "Initial ") : go 1 todos
  where
    go _ [] = []
    go counter (todo:rest) =
        todo : dumpCorePass counter (text "After " GhcPlugins.<> ppr todo)
             : go (counter + 1) rest
#else
dumpCore :: Int -> SDoc -> ModGuts -> CoreM ModGuts
dumpCore _counter _title guts = return guts

_insertDumpCore :: [CoreToDo] -> [CoreToDo]
_insertDumpCore = id
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
#if MIN_VERSION_ghc(9,6,0)
    go _ (todo@(CoreDoSimplify
        (SimplifyOpts
            { so_mode =
                (SimplMode
                    { sm_phase = Phase 0
                    , sm_names = ["main"]
                    }
                )
            }
        )):todos)
#elif MIN_VERSION_ghc(9,5,0)
    go _ (todo@(CoreDoSimplify (CoreDoSimplifyOpts _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            })):todos)
#else
    go _ (todo@(CoreDoSimplify _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            }):todos)
#endif
        = todo : ourTodos ++ go True todos
    go found (todo:todos) = todo : go found todos

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = do
    options <- liftIO $ parseOptions args
    dflags <- getDynFlags
    hscEnv <- getHscEnv
#if MIN_VERSION_ghc(9,14,0)
    let hpt = ue_hpt (hsc_unit_env hscEnv)
    home_pkg_rules <- liftIO $  concatHpt (md_rules . hm_details) hpt
    let hpt_rule_base = mkRuleBase home_pkg_rules
        simplify = fusionSimplify hpt_rule_base hscEnv dflags
#elif MIN_VERSION_ghc(9,6,0)
    m <- getModule
    let
        home_pkg_rules =
            hptRules
                hscEnv
                (moduleUnitId m)
                (GWIB
                    { gwib_mod = moduleName m
                    , gwib_isBoot = NotBoot
                    }
                )
        hpt_rule_base = mkRuleBase home_pkg_rules
        -- XXX GHC should export getHomeRuleBase
        -- hpt_rule_base <- getHomeRuleBase
        simplify = fusionSimplify hpt_rule_base hscEnv dflags
#else
    let simplify = fusionSimplify hscEnv dflags
#endif

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
        (if optionsDumpCore options
         then _insertDumpCore
         else id) $
        insertAfterSimplPhase0
            todos
            [ fusionMarkInline 1 ReportSilent True
            , simplify
            , fusionMarkInline 2 ReportSilent True
            , simplify
            , fusionMarkInline 3 ReportSilent True
            , simplify
            -- This lets us know what was left unfused after all the inlining
            -- and case-of-case transformations.
            , let mesg = "Check unfused (post inlining)"
              in CoreDoPluginPass mesg
                    (fusionReport mesg ReportSilent False False)
            ]
            (let mesg = "Check unfused (final)"
                 report =
                    fusionReport
                        mesg (optionsVerbosityLevel options) True
                        (optionsWError options)
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
