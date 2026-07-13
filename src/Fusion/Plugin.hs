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
import Control.Monad (when, unless)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Char (isDigit)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Word (Word8)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH.Syntax as TH

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

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Panic (throwGhcExceptionIO, GhcException(ProgramError))
#elif !MIN_VERSION_ghc(9,0,0)
import Panic (throwGhcExceptionIO, GhcException(ProgramError))
#endif

-- Core size reporting
#if MIN_VERSION_ghc(9,0,0)
import GHC.Core.Stats (exprStats, CoreStats(cs_tm))
#else
import CoreStats (exprStats, CoreStats(cs_tm))
#endif

import Fusion.Plugin.Types

-- $using
--
-- This plugin was primarily motivated by fusion issues discovered in
-- [streamly](https://github.com/composewell/streamly) but it can be used in
-- general.
--
-- You need to annotate your code to enable fusion or to report details about
-- fused types in a function. See the documentation of the
-- `fusion-plugin-types` package for available annotations. Also see the
-- `README` in this package for a guide to use the annotations and this plugin.
--
-- To use this plugin, add this package to your @build-depends@
-- and pass the following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin
-- @
--
-- GHC option to treat annotation-check violations as errors instead of
-- warnings:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:werror
-- @
--
-- GHC option to show more details about the checks and violations:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:verbose=1
-- @
--
-- Verbosity levels @2@, @3@, @4@ can be used for more verbose output.
--
-- To dump the core after each core to core transformation, pass the
-- following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin -fplugin-opt=Fusion.Plugin:dump-core
-- @
-- Output from each transformation is then printed in a different file.
--
-- Note: @dump-core@ does not work for GHC-9.0.x, 9.6.x and 9.8.x.
--
-- To record the optimized core size of every 'MaxCoreSize'-annotated binding
-- to a CSV file, pass the following:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-sizes
-- @
--
-- Each module writes a @\<module-name\>.core-sizes.csv@ file in the compiler's
-- dump directory (see GHC's @-dumpdir@ option), or in
-- @fusion-plugin-output\/\<package-name\>@ when no dump directory is set, with
-- one @binding-name,core-size@ row per annotated binding.
--
-- To dump the Core of every binding that carries a violation-causing
-- annotation ('InspectTypes', 'InspectTypeClasses' or 'MaxCoreSize'), pass
-- the following:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-if-annotated
-- @
--
-- Each such binding's Core is written to a
-- @\<module-name\>.\<binder-name\>.dump-simpl@ file in the same directory,
-- exactly as the 'Fusion.Plugin.Types.DumpCore' annotation would.
--
-- Similarly, @dump-core-if-violated@ dumps the Core of only those annotated
-- bindings for which a check actually reported a violation:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-if-violated
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
    , optionsDumpCoreSizes :: Bool
    , optionsDumpCoreIfAnnotated :: Bool
    , optionsDumpCoreIfViolated :: Bool
    , optionsVerbosityLevel :: ReportMode
    , optionsWError :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optionsDumpCore = False
    , optionsDumpCoreSizes = False
    , optionsDumpCoreIfAnnotated = False
    , optionsDumpCoreIfViolated = False
    , optionsVerbosityLevel = ReportSilent
    , optionsWError = False
    }

setDumpCore :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCore val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCore = val })

setDumpCoreSizes :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCoreSizes val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCoreSizes = val })

setDumpCoreIfAnnotated
    :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCoreIfAnnotated val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCoreIfAnnotated = val })

setDumpCoreIfViolated
    :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setDumpCoreIfViolated val = do
    (args, opts) <- get
    put (args, opts { optionsDumpCoreIfViolated = val })

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
shift :: Monad m => StateT ([String], Options) m (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

-- totally imperative style option parsing
parseOptions :: [CommandLineOption] -> IO Options
parseOptions args =
    flip evalStateT (args, defaultOptions)
        $ do parseLoop
             fmap snd get

    where

    parseOpt opt =
        case opt of
            "dump-core" -> setDumpCore True
            "dump-core-sizes" -> setDumpCoreSizes True
            "dump-core-if-annotated" -> setDumpCoreIfAnnotated True
            "dump-core-if-violated" -> setDumpCoreIfViolated True
            "werror" -> setWError True
            "verbose=1" -> setVerbosityLevel ReportWarn
            "verbose=2" -> setVerbosityLevel ReportVerbose
            "verbose=3" -> setVerbosityLevel ReportVerbose1
            "verbose=4" -> setVerbosityLevel ReportVerbose2
            str ->
                liftIO
                    $ throwGhcExceptionIO
                    $ ProgramError
                    $ "fusion-plugin: Unrecognized option - \"" ++ str ++ "\""

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
#define GET_NAME getName
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

hasNoInlineBinder :: CoreBndr -> Bool
hasNoInlineBinder bndr = isNoInlinePragma (inlinePragInfo (idInfo bndr))

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

    -- If the head of the application spine is a data constructor, record a hit
    -- for its type -- this recognizes a constructor applied to its fields
    -- (e.g. `Yield x y`), which checking a bare 'Var' node's own type cannot:
    -- the unapplied constructor 'Id' has a function type, not the constructed
    -- type, so that check only fires for nullary constructors.
    go parents e@(App _ _) =
        let (fun, args) = collectArgs e
            hit = case fun of
                Var i
                    | Just dcon <- isDataConId_maybe i
                    , isInteresting (GET_NAME (dataConTyCon dcon)) ->
                        [(parents, Constr i)]
                _ -> []
        in hit ++ go parents fun ++ concatMap (go parents) args
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

-- | Resolve the 'TyCon' a 'Constr' hit represents. When the 'Id' is itself a
-- data constructor (regardless of arity), resolve via its 'DataCon' rather
-- than the 'Id's own type: a non-nullary constructor's own type is a
-- function over its fields (e.g. @(:) :: a -> [a] -> [a]@), and even a
-- nullary constructor's is forall-quantified (e.g. @[] :: forall a. [a]@),
-- so neither is itself the constructed type that 'tyConAppTyConPicky_maybe'
-- can see. Falls back to the 'Id's own type for a bare boxed-use hit that is
-- not itself a constructor (e.g. a local binder of the annotated type).
constrTyCon :: Id -> Maybe TyCon
constrTyCon con =
    case isDataConId_maybe con of
        Just dcon -> Just (dataConTyCon dcon)
        Nothing -> tyConAppTyConPicky_maybe (varType con)

contextTyConName :: Context -> Maybe Name
contextTyConName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (GET_NAME $ dataConTyCon dcon)
contextTyConName (CaseAlt _) = Nothing
contextTyConName (Constr con) = GET_NAME <$> constrTyCon con

-- | Like 'contextTyConName' but yields the fully-qualified @Module.Type@ name
-- (via 'qualifiedTyConName') used in reports rather than the raw 'Name'.
contextQualifiedName :: Context -> Maybe String
contextQualifiedName (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    Just (qualifiedTyConName (dataConTyCon dcon))
contextQualifiedName (CaseAlt _) = Nothing
contextQualifiedName (Constr con) = qualifiedTyConName <$> constrTyCon con

-- | Drop any hit whose TyCon 'Name' is in the given exclusion list.
filterExcluded
    :: [Name] -> [([CoreBind], Context)] -> [([CoreBind], Context)]
filterExcluded excl =
    filter (\(_, ctx) -> maybe True (`notElem` excl) (contextTyConName ctx))

-- | True when the type occurs in a scrutinizing (pattern-match, i.e. @case@)
-- position -- a value being deconstructed. Note this is a /use/ of the value,
-- not an allocation: the box was allocated elsewhere (see 'isHeapAllocated').
isPatternMatch :: Context -> Bool
isPatternMatch (CaseAlt _) = True
isPatternMatch (Constr _)  = False

-- | True when the type occurs in a constructing (allocating) position -- a
-- value being built here.
isConstruction :: Context -> Bool
isConstruction (Constr _)  = True
isConstruction (CaseAlt _) = False

-- | True for TyCons whose values GHC never heap-allocates: unboxed
-- primitives (e.g. 'Int#', 'State#'), unboxed tuples/sums, and true
-- enumeration types (every data constructor nullary, e.g. '()', 'Bool')
-- which are compiled as statically shared singletons.
isNotHeapAllocatedTyCon :: TyCon -> Bool
isNotHeapAllocatedTyCon tycon =
    isPrimTyCon tycon
    || isUnboxedTupleTyCon tycon
    || isUnboxedSumTyCon tycon
    || isEnumerationTyCon tycon

-- | False for the cases that can never represent boxing: a case match or
-- construction of a non-allocating type (see 'isNotHeapAllocatedTyCon'), or a
-- bare reference to something of function type (e.g. a primop like \"+#\", or
-- a specialized worker) which is not a data construction at all.
--
-- This covers all usage including case scrutiny as well as construction.
isHeapAllocated :: Context -> Bool
isHeapAllocated (CaseAlt (ALT_CONSTR(DataAlt dcon,_,_))) =
    not (isNotHeapAllocatedTyCon (dataConTyCon dcon))
isHeapAllocated (CaseAlt _) = True
isHeapAllocated (Constr con) =
    case isDataConId_maybe con of
        -- A genuine data-constructor 'Id' is never itself the "bare
        -- reference to something of function type" this guards against
        -- (that's a primop/worker, never a 'DataCon'), so the 'isFunTy'
        -- exclusion below does not apply here -- it would wrongly drop
        -- every non-nullary constructor (e.g. @(:)@), whose own type is a
        -- function over its fields.
        Just dcon -> not (isNotHeapAllocatedTyCon (dataConTyCon dcon))
        Nothing ->
            not (isFunTy (varType con))
            && maybe True (not . isNotHeapAllocatedTyCon)
                     (tyConAppTyConPicky_maybe (varType con))

-- | Drop the cases that can never represent boxing (see 'isHeapAllocated').
keepHeapAllocatedOnly
    :: [([CoreBind], Context)] -> [([CoreBind], Context)]
keepHeapAllocatedOnly = filter (isHeapAllocated . snd)

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

-- | Build the "isInteresting" predicate, the exclusion list, and the position
-- filter for a given 'InspectTypes' directive. The position filter selects
-- which occurrences count: all positions by default, or only the scrutinizing
-- ('PermitPatternMatches') or only the constructing ('PermitAllocations')
-- position.
inspectPredicate
    :: UNIQ_FM -> InspectTypes -> CoreM (Name -> Bool, [Name], Context -> Bool)
inspectPredicate _ (ForbidBoxedUse thNames) = do
    names <- resolveTHNames thNames
    return (\n -> n `elem` names, [], const True)
inspectPredicate anns (ForbidFused thForbid thAllow) = do
    forbidden <- resolveTHNames thForbid
    allowed <- resolveTHNames thAllow
    return
        ( \n -> isJust (lookupUFM anns n) || n `elem` forbidden
        , allowed
        , const True
        )
inspectPredicate _ (PermitBoxedUse thAllow) = do
    allowed <- resolveTHNames thAllow
    return (const True, allowed, const True)
inspectPredicate _ (PermitPatternMatches thAllow) = do
    allowed <- resolveTHNames thAllow
    return (const True, allowed, isPatternMatch)
inspectPredicate _ (PermitAllocations thAllow) = do
    allowed <- resolveTHNames thAllow
    return (const True, allowed, isConstruction)

-- | For the "permit" (allowlist) directives, the directive's display name and
-- its allow list; 'Nothing' for the "forbid" directives, which have no
-- stale-entry warning.
permitAllowList :: InspectTypes -> Maybe (String, [TH.Name])
permitAllowList (PermitBoxedUse ns) = Just ("PermitBoxedUse", ns)
permitAllowList (PermitPatternMatches ns) = Just ("PermitPatternMatches", ns)
permitAllowList (PermitAllocations ns) = Just ("PermitAllocations", ns)
permitAllowList _ = Nothing

forbiddenLabel :: InspectTypes -> String
forbiddenLabel (PermitAllocations _) = "forbidden allocations"
forbiddenLabel (PermitPatternMatches _) = "forbidden pattern matches"
forbiddenLabel _ = "forbidden types"

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

-- | Show a 'Name' fully qualified as @Module.Name@ so that entities with the
-- same unqualified name defined in different modules (e.g. several @Step@
-- types) can be told apart in a report. Wired-in names with no defining
-- module are shown by their bare name.
qualifiedName :: Name -> String
qualifiedName name =
    case nameModule_maybe name of
        Just m ->
            moduleNameString (moduleName m) ++ "." ++ getOccString name
        Nothing -> getOccString name

-- | Show a 'TyCon' fully qualified as @Module.Name@ (see 'qualifiedName').
qualifiedTyConName :: TyCon -> String
qualifiedTyConName = qualifiedName . getName

showDetailsConstr
    :: DynFlags
    -> ReportMode
    -> ([CoreBind], Id)
    -> String
showDetailsConstr dflags reportMode (binds, con) =
    let t = constrTyCon con
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
    ppr (ForbidBoxedUse names) = text "ForbidBoxedUse" <+> text (show names)
    ppr (ForbidFused f a) =
        text "ForbidFused" <+> text (show f) <+> text (show a)
    ppr (PermitBoxedUse names) = text "PermitBoxedUse" <+> text (show names)
    ppr (PermitPatternMatches names) =
        text "PermitPatternMatches" <+> text (show names)
    ppr (PermitAllocations names) =
        text "PermitAllocations" <+> text (show names)

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

binderAnnKeys :: CoreBndr -> [String]
binderAnnKeys b = nm : [base | Just base <- [DL.stripPrefix "$w" nm]]

    where

    nm = getOccString (GET_NAME b)

-- | Look up a per-binder annotation, accounting for worker/wrapper splitting.
--
-- The annotations are keyed by the source-level binder name (e.g. @f@). The
-- strictness analyser, however, may split an annotated function @f@ into a
-- worker @$wf@ and a wrapper @f@; when @f@ is not exported the wrapper is
-- frequently inlined away at its (single) call site, leaving only @$wf@ in the
-- final Core. In that case the binder's occurrence name (@$wf@) would not be
-- found in the annotation map. So if the direct lookup fails and the binder is
-- a @$w@ worker, retry with the prefix stripped.
lookupBinderAnn :: CoreBndr -> Map.Map String a -> Maybe a
lookupBinderAnn b m = listToMaybe (mapMaybe (`Map.lookup` m) (binderAnnKeys b))

-------------------------------------------------------------------------------
-- Transitive closure of a binding
-------------------------------------------------------------------------------

-- | All 'Var' occurrences appearing anywhere in an expression. Like
-- 'classTyConsInBind' but collects term-level variable references instead of
-- type constructors.
exprVarOccs :: CoreExpr -> VarSet
exprVarOccs = goE

    where

    goE (Var v) = unitVarSet v
    goE (Lit _) = emptyUniqSet
    goE (App e1 e2) = goE e1 `unionUniqSets` goE e2
    goE (Lam _ e) = goE e
    goE (Let b e) = goB b `unionUniqSets` goE e
    goE (Case e _ _ alts) =
        goE e `unionUniqSets` unionManyUniqSets (map goAlt alts)
    goE (Cast e _) = goE e
    goE (Tick _ e) = goE e
    goE (Type _) = emptyUniqSet
    goE (Coercion _) = emptyUniqSet

    goAlt (ALT_CONSTR(_,_,e)) = goE e

    goB (NonRec _ e) = goE e
    goB (Rec bs) = unionManyUniqSets (map (goE . snd) bs)

-- | Return the binder itself plus the transitive closure of all /other top
-- level/ binders reachable from its RHS (following term-level references).
--
-- This reconstructs a whole function that the optimiser has scattered across
-- several top level bindings (a wrapper referencing a @$w@ worker, a worker
-- referencing a @$s$w@ specialisation, and so on). References to imported ids
-- or local binders are ignored (only names in this module's @binds@ are
-- followed).
binderClosure :: [(CoreBndr, CoreExpr)] -> CoreBndr -> [(CoreBndr, CoreExpr)]
binderClosure binds root = go [root] emptyVarSet []

    where

    topSet = mkVarSet (map fst binds)

    go [] _ acc = acc
    go (v : vs) seen acc
        | v `elemVarSet` seen = go vs seen acc
        | otherwise =
            case lookup v binds of
                Nothing -> go vs (extendVarSet seen v) acc
                Just e ->
                    let refs = nonDetEltsUniqSet
                                   (intersectVarSet (exprVarOccs e) topSet)
                    in go (refs ++ vs) (extendVarSet seen v) ((v, e) : acc)

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
    :: DynFlags -> ReportMode -> UNIQ_FM -> INSPECT_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM Int
reportInspected dflags reportMode anns inspectAnns allBinds (NonRec b _) =
    case lookupBinderAnn b inspectAnns of
        Nothing -> return 0
        Just ispec -> go ispec

    where

    go ispec = do
        (isInteresting, exclusion, inPosition) <- inspectPredicate anns ispec
        let allHits = filter (inPosition . snd)
                    $ keepHeapAllocatedOnly
                    $ concatMap
                        (\(v, e) ->
                            containsAnns dflags isInteresting (NonRec v e))
                        (binderClosure allBinds b)
            results = filterExcluded exclusion allHits
        warnStalePermitted ispec allHits
        if null results
        then return 0
        else do
            case reportMode of
                ReportSilent -> terse ispec results
                ReportWarn -> terse ispec results
                _ -> detailed ispec results
            return 1

    -- Warn about "permit" (allowlist) entries that never occur in the
    -- binding, so that stale types can be pruned from the permit list. The
    -- predicate for these directives is @const True@, so 'allHits' holds
    -- every (allocating) type occurrence in the relevant position(s).
    warnStalePermitted ispec allHits =
        case permitAllowList ispec of
            Nothing -> return ()
            Just (label, thAllow) -> do
                allowed <- resolveTHNames thAllow
                let present = mapMaybe (contextTyConName . snd) allHits
                    stale = filter (`notElem` present) allowed
                unless (null stale) $
                    putMsgS $ "fusion-plugin: "
                            ++ getOccString (GET_NAME b)
                            ++ ": redundant " ++ label
                            ++ " entries (safe to remove): ["
                            ++ DL.intercalate ", " (map qualifiedName stale)
                            ++ "]"

    terse ispec results =
        let names = DL.nub (mapMaybe (contextQualifiedName . snd) results)
        in putMsgS $ "fusion-plugin: "
                   ++ getOccString (GET_NAME b)
                   ++ ": found " ++ forbiddenLabel ispec ++ " ["
                   ++ DL.intercalate ", " names ++ "]"

    detailed ispec results = do
        putMsgS $ "fusion-plugin: "
                ++ showWithUnique dflags b
                ++ ": inspecting (" ++ showSDoc dflags (ppr ispec) ++ ")..."
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
reportInspected _ _ _ _ _ (Rec _) =
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
    -> [(CoreBndr, CoreExpr)]
    -> CoreBind
    -> CoreM Int
reportInspectedClasses dflags reportMode classAnns allBinds (NonRec b _) =
    case lookupBinderAnn b classAnns of
        Nothing -> return 0
        Just ispec -> go ispec

    where

    go ispec = do
        isInteresting <- inspectClassPredicate ispec
        let allClasses = concatMap
                         (\(v, e) -> classTyConsInBind (NonRec v e))
                         (binderClosure allBinds b)
            hits = filter (isInteresting . getName) allClasses
        warnStalePermitted ispec allClasses
        if null hits
        then return 0
        else do
            case reportMode of
                ReportSilent -> report ispec hits
                ReportWarn -> report ispec hits
                _ -> do
                    putMsgS $ "fusion-plugin: "
                            ++ showWithUnique dflags b
                            ++ ": inspecting (" ++ showSDoc dflags (ppr ispec) ++ ")..."
                    report ispec hits
            return 1

    report _ hits =
        let names = DL.nub (map qualifiedTyConName hits)
        in putMsgS $ "fusion-plugin: "
                   ++ getOccString (GET_NAME b)
                   ++ ": found forbidden type classes ["
                   ++ DL.intercalate ", " names ++ "]"

    -- Warn about 'PermitTypeClasses' entries that never occur in the binding,
    -- so that stale classes can be pruned from the permit list.
    warnStalePermitted (PermitTypeClasses thAllow) allClasses = do
        allowed <- resolveTHNames thAllow
        let present = map getName allClasses
            stale = filter (`notElem` present) allowed
        unless (null stale) $
            putMsgS $ "fusion-plugin: "
                    ++ getOccString (GET_NAME b)
                    ++ ": redundant PermitTypeClasses entries (safe to remove): ["
                    ++ DL.intercalate ", " (map qualifiedName stale) ++ "]"
    warnStalePermitted _ _ = return ()
reportInspectedClasses _ _ _ _ (Rec _) =
    error "reportInspectedClasses: expecting only NonRec binders"

fallbackDumpDir :: String -> FilePath
fallbackDumpDir pkgName = "fusion-plugin-output" </> pkgName

pluginDumpDir :: DynFlags -> String -> FilePath
pluginDumpDir dflags pkgName =
    fromMaybe (fallbackDumpDir pkgName) (dumpDir dflags)

-- | Path of the CSV file that 'reportCoreSize' appends core sizes to when the
-- @dump-core-sizes@ option is set: @\<module-name\>.core-sizes.csv@ under
-- 'pluginDumpDir'.
coreSizesFile :: DynFlags -> String -> String -> FilePath
coreSizesFile dflags pkgName modName =
    pluginDumpDir dflags pkgName </> (modName ++ ".core-sizes.csv")

-- Returns 0 on no violations and 1 otherwise.
reportCoreSize
    :: DynFlags -> ReportMode -> Bool -> String -> String -> MAX_CORE_SIZE_FM
    -> [(CoreBndr, CoreExpr)] -> CoreBind -> CoreM Int
reportCoreSize dflags reportMode dumpCoreSizes pkgName modName sizeAnns allBinds (NonRec b _) =
    case lookupBinderAnn b sizeAnns of
        Nothing -> return 0
        Just ann -> go ann

    where

    clSet = binderClosure allBinds b
    terms = sum (map (cs_tm . exprStats . snd) clSet)

    go (MaxCoreSize maxSize) = do
        case reportMode of
            ReportSilent -> return ()
            ReportWarn -> return ()
            _ ->
                putMsgS $ "fusion-plugin: "
                        ++ showWithUnique dflags b
                        ++ ": core size "
                        ++ show terms
                        ++ " terms (set of " ++ show (length clSet)
                        ++ " bindings)"
        -- Append a "binding-name,core-size" row (binder name without its
        -- unique suffix) to the per-module CSV file.
        when dumpCoreSizes $ liftIO $ do
            let path = coreSizesFile dflags pkgName modName
            createDirectoryIfMissing True (takeDirectory path)
            appendFile path (getOccString (GET_NAME b) ++ "," ++ show terms ++ "\n")
        if terms > maxSize
        then do
            putMsgS $ "fusion-plugin: "
                    ++ showWithUnique dflags b
                    ++ ": core size (" ++ show terms
                    ++ " terms) exceeds the specified size ("
                    ++ show maxSize ++ " terms)."
            return 1
        else return 0
reportCoreSize _ _ _ _ _ _ _ (Rec _) =
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

dumpBindCore
    :: DynFlags -> String -> String -> [(CoreBndr, CoreExpr)] -> CoreBndr
    -> CoreM ()
dumpBindCore dflags pkgName modName allBinds b = do
    let dir = pluginDumpDir dflags pkgName
        fileName =
            modName ++ "." ++ getOccString (GET_NAME b) ++ ".dump-simpl"
        path = dir </> fileName
        closure = binderClosure allBinds b
        doc = vcat (map (ppr . uncurry NonRec) closure)
    liftIO $ do
        createDirectoryIfMissing True dir
        writeFile path (showSDoc dflags doc ++ "\n")
    putMsgS $ "fusion-plugin: "
            ++ showWithUnique dflags b ++ ": dumped core to " ++ path

-- | If the given top level bind's own binder carries a 'DumpCore' annotation,
-- write the Core of that binding to a file (see 'dumpBindCore'). No-op if the
-- binder is not annotated.
reportDumpCore ::
    DynFlags -> String -> String -> DUMP_CORE_FM -> [(CoreBndr, CoreExpr)]
    -> CoreBind -> CoreM ()
reportDumpCore dflags pkgName modName dumpAnns allBinds (NonRec b _) =
    case lookupBinderAnn b dumpAnns of
        Nothing -> return ()
        Just _ -> dumpBindCore dflags pkgName modName allBinds b
reportDumpCore _ _ _ _ _ (Rec _) =
    error "reportDumpCore: expecting only NonRec binders"

dumpAllBindsCore :: DynFlags -> String -> String -> [CoreBind] -> CoreM ()
dumpAllBindsCore dflags pkgName modName binds = do
    let dir = pluginDumpDir dflags pkgName
        fileName = modName ++ ".dump-simpl"
        path = dir </> fileName
    liftIO $ do
        createDirectoryIfMissing True dir
        writeFile path (showSDoc dflags (vcat (map ppr binds)) ++ "\n")
    putMsgS $ "fusion-plugin: " ++ modName ++ ": dumped core to " ++ path

markInline :: Int -> ReportMode -> Bool -> ModGuts -> CoreM ModGuts
markInline pass reportMode transform guts = do
    putMsgS $ "fusion-plugin: Checking bindings to inline..."
    dflags <- getDynFlags
    anns <- FMAP_SND getAnnotations deserializeWithData guts
    fuseTypesAnns <-
        getAnnotationsByStableName "FuseTypes" deserializeWithData guts
    noFuseTypesAnns <-
        getAnnotationsByStableName "NoFuseTypes" deserializeWithData guts
    let pkgName = modulePackageName (mg_module guts)
        modName = moduleNameString (moduleName (mg_module guts))
        modBinds = flattenBinds (mg_binds guts)
    if (anyUFM (any (== Fuse)) anns || not (Map.null fuseTypesAnns))
    then do
        r <- bindsOnlyPass
                (mapM
                    (transformBind
                        dflags anns fuseTypesAnns noFuseTypesAnns
                        pkgName modName modBinds))
                guts
        if dbgLevel > 0
        then dumpCore 0 (text ("Fusion-plugin-" ++ show pass)) r
        else return r
    else return guts
  where
    -- transformBind :: DynFlags -> UniqFM Unique [Fuse] -> CoreBind -> CoreM CoreBind
    transformBind
            dflags anns0 fuseTypesAnns noFuseTypesAnns
            pkgName modName modBinds bind@(NonRec b _) = do
        -- Types named in a 'FuseTypes' annotation on this binding act as if
        -- they were 'Fuse'-annotated, but only while inlining inside it.
        -- Types named in a 'NoFuseTypes' annotation are stripped of their
        -- 'Fuse' status locally, overriding the above and any module-wide
        -- 'Fuse' annotation for this binding only.
        anns1 <- augmentFuseTypes anns0 fuseTypesAnns b
        anns <- removeFuseTypes anns1 noFuseTypesAnns b
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
        let blocked = blockedPat ++ blockedConstr
            blockedBinders = DL.nub $ map (getNonRecBinder . head . fst) blocked
        mapM_
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
            dflags anns fuseTypesAnns noFuseTypesAnns
            pkgName modName modBinds (Rec bs) = do
        fmap Rec (mapM transformAsNonRec bs)
      where
        transformAsNonRec (b, expr) = do
            r <- transformBind
                    dflags anns fuseTypesAnns noFuseTypesAnns
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

-- binderAnnKeys is shared with lookupBinderAnn and this function so that both
-- are consistent with each other.
failOnUnmatchedAnns
    :: DynFlags -> String -> String
    -> [CoreBind] -> [(CoreBndr, CoreExpr)]
    -> INSPECT_FM -> INSPECT_CLASSES_FM -> MAX_CORE_SIZE_FM -> DUMP_CORE_FM
    -> CoreM ()
failOnUnmatchedAnns dflags pkgName modName allTopBinds allBinds
        inspectAnns classAnns sizeAnns dumpAnns = do
    let candidateKeys = DL.nub (concatMap (binderAnnKeys . fst) allBinds)
        unmatched =
            filter (`notElem` candidateKeys) $ DL.nub
                ( Map.keys inspectAnns ++ Map.keys classAnns
               ++ Map.keys sizeAnns ++ Map.keys dumpAnns )
    mapM_
        (\k ->
            putMsgS $ "fusion-plugin: " ++ k
                ++ ": annotated but no such binding in " ++ modName
                ++ " (inlined away -- try NOINLINE or NoFuseTypes on it).")
        unmatched
    when (not (null unmatched)) $ do
        -- Dump whatever Core we do have before failing: the annotated binding
        -- is gone, so there is no single 'CoreBind' left to hand to
        -- 'dumpBindCore', therefore a 'DumpCore' annotation will also not
        -- work, do a whole-module dump so we can still diagnose the problem.
        dumpAllBindsCore dflags pkgName modName allTopBinds
        liftIO $ throwGhcExceptionIO
            (ProgramError
                ("fusion-plugin: " ++ show (length unmatched)
                  ++ " inspection annotation(s) matched no binding in "
                  ++ modName ++ "; failing the build."))

-- | @runInspect@ controls whether the per-binding 'InspectTypes' annotations are
-- also processed. When @werror@ is set, the build is failed (after all
-- annotation violations in the module have been reported) if any
-- annotation-check violation ('InspectTypes', 'InspectTypeClasses' or
-- 'MaxCoreSize') was found.
fusionReport
    :: String -> ReportMode -> Bool -> Bool -> Bool -> Bool -> Bool -> ModGuts
    -> CoreM ModGuts
fusionReport
        mesg reportMode runInspect werror dumpCoreSizes dumpCoreIfAnnotated
        dumpCoreIfViolated guts = do
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
            let allBinds = flattenBinds (mg_binds guts)
            -- Truncate the per-module core-sizes CSV once, before any binding
            -- is processed, so 'reportCoreSize' can append fresh rows to it
            -- without carrying over stale entries from a previous build.
            when (dumpCoreSizes && anyMaxCoreSize) $ liftIO $ do
                let path = coreSizesFile dflags pkgName modName
                createDirectoryIfMissing True (takeDirectory path)
                writeFile path ""
            violations <-
                if anyUFM (any (== Fuse)) anns || anyReport
                then fmap sum
                        $ mapM
                            (transformBind
                                dflags anns inspectAnns classAnns sizeAnns
                                dumpAnns pkgName modName liveBndrs allBinds)
                        $ mg_binds guts
                else return 0
            -- Fail on any inspection annotation whose target has no
            -- corresponding binding in the final core.
            failOnUnmatchedAnns
                dflags pkgName modName (mg_binds guts) allBinds
                inspectAnns classAnns sizeAnns dumpAnns
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
        -> String -> String -> VarSet -> [(CoreBndr, CoreExpr)]
        -> CoreBind -> CoreM Int
    transformBind dflags anns inspectAnns classAnns sizeAnns dumpAnns
            pkgName modName liveBndrs allBinds bind@(NonRec b _) = do
        n1 <- if runInspect
              then reportInspected dflags reportMode anns inspectAnns allBinds bind
              else return 0
        n2 <- if runInspect
              then reportInspectedClasses
                       dflags reportMode classAnns allBinds bind
              else return 0
        n3 <- if runInspect
              then reportCoreSize
                       dflags reportMode dumpCoreSizes pkgName modName sizeAnns
                       allBinds bind
              else return 0
        when runInspect $
            reportDumpCore dflags pkgName modName dumpAnns allBinds bind
        -- Auto-dump the Core of this binding (see 'dumpBindCore') when either:
        --   * 'dump-core-if-annotated' is set and the binding carries a
        --     violation-causing annotation ('InspectTypes', 'InspectTypeClasses'
        --     or 'MaxCoreSize'), or
        --   * 'dump-core-if-violated' is set and one of those checks actually
        --     reported a violation for this binding (n1 + n2 + n3 > 0).
        let hasViolationAnn =
                   isJust (lookupBinderAnn b inspectAnns)
                || isJust (lookupBinderAnn b classAnns)
                || isJust (lookupBinderAnn b sizeAnns)
            shouldDump =
                   (dumpCoreIfAnnotated && hasViolationAnn)
                || (dumpCoreIfViolated && n1 + n2 + n3 > 0)
        when (runInspect && shouldDump) $
            dumpBindCore dflags pkgName modName allBinds b
        when (b `elemVarSet` liveBndrs) $ do
            let results = keepHeapAllocatedOnly
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
                    let allBnds = map fst patternMatches ++ map fst constrs
                    when (not $ null allBnds) $ do
                        putMsgS "Unfused bindings:"
                        putMsgS
                            $ DL.unlines
                            $ DL.nub
                            $ map (listPath dflags) allBnds
                _ -> do
                    showInfo b dflags reportMode "SCRUTINIZE"
                        uniqBinders patternMatches showDetailsCaseMatch
                    showInfo b dflags reportMode "CONSTRUCT"
                        uniqConstr constrs showDetailsConstr
        return (n1 + n2 + n3)

    transformBind dflags anns inspectAnns classAnns sizeAnns dumpAnns
            pkgName modName liveBndrs allBinds (Rec bs) =
        fmap sum
            $ mapM
                (\(b, expr) ->
                    transformBind
                        dflags anns inspectAnns classAnns sizeAnns dumpAnns
                        pkgName modName liveBndrs allBinds (NonRec b expr))
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
                    (fusionReport mesg ReportSilent False False False False False)
            ]
            (let mesg = "Check unfused (final)"
                 report =
                    fusionReport
                        mesg (optionsVerbosityLevel options) True
                        (optionsWError options) (optionsDumpCoreSizes options)
                        (optionsDumpCoreIfAnnotated options)
                        (optionsDumpCoreIfViolated options)
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
