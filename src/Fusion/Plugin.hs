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
import Control.Monad (when, unless, forM_, void)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import qualified Data.Map.Strict as Map

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Opt.Simplify.Env (SimplMode(..))
import GHC.Core.Opt.Simplify (SimplifyOpts(..))
#endif

#if MIN_VERSION_ghc(9,14,0)
import GHC.Unit.Env (ue_hpt)
import GHC.Unit.Home.ModInfo (HomeModInfo(..))
import GHC.Unit.Home.PackageTable (concatHpt)
import GHC.Unit.Module.ModDetails (ModDetails(..))
#endif
#endif

-- Implicit imports
#if MIN_VERSION_ghc(9,0,0)
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

import Fusion.Plugin.Types

import Fusion.Plugin.Common
    ( Options(..)
    , ReportMode(..)
    , dumpCore
    , dumpCoreSuffix
    , getAnnotationsByStableName
    , lookupBinderAnn
    , modulePackageName
    , pluginDumpDir
    , pluginDumpStem
    , subsumedBySameName
    , writeBindCore
    )
import Fusion.Plugin.Fuse
    ( fusionMarkInline
    , fusionSimplify
    )
import Fusion.Plugin.Inspect
    ( fusionReport
    )

#define DUMP_CORE_PASSES_FM Map.Map String DumpCorePasses

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
-- GHC option to make every @Forbid...@ annotation
-- ('Fusion.Plugin.Types.ForbidPatternMatches',
-- 'Fusion.Plugin.Types.ForbidAllocations') additionally forbid all
-- 'Fusion.Plugin.Types.Fuse' annotated types, using them as a baseline. The
-- types named in the annotation are then forbidden on top of the fused types:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:forbid-fused
-- @
--
-- To dump the core after each core to core transformation, pass the
-- following to your ghc-options:
--
-- @
-- ghc-options: -O2 -fplugin=Fusion.Plugin -fplugin-opt=Fusion.Plugin:dump-core
-- @
-- Output from each transformation is then printed in a different file.
--
-- Note: @dump-core@ does not work for GHC-9.0.x.
--
-- To record the optimized core size of every binding that carries a
-- violation-causing annotation ('InspectPatternMatches', 'InspectAllocations',
-- 'InspectTypeClasses' or 'MaxCoreSize') to a CSV file, pass the following:
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-sizes
-- @
--
-- Each module writes a @\<module-name\>.core-sizes.csv@ file in the compiler's
-- dump directory (see GHC's @-dumpdir@ option), or in
-- @fusion-plugin-output\/\<package-name\>@ when no dump directory is set. The
-- file starts with a @name,core-size@ header row followed by one row
-- per annotated binding.
--
-- By default the CSV is truncated on each compilation so it reflects only the
-- latest build. Pass @csv-append@ to instead keep the existing file and append
-- each run's header and rows to it, accumulating the results of successive
-- builds in one file (useful for comparing core sizes across changes):
--
-- @
-- ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-sizes -fplugin-opt=Fusion.Plugin:csv-append
-- @
--
-- To dump the Core of every binding that carries a violation-causing
-- annotation ('InspectPatternMatches', 'InspectAllocations',
-- 'InspectTypeClasses' or 'MaxCoreSize'), pass the following:
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

defaultOptions :: Options
defaultOptions = Options
    { optionsDumpCore = False
    , optionsDumpCoreSizes = False
    , optionsDumpCoreIfAnnotated = False
    , optionsDumpCoreIfViolated = False
    , optionsVerbosityLevel = ReportSilent
    , optionsWError = False
    , optionsCsvAppend = False
    , optionsForbidFused = False
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

setCsvAppend :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setCsvAppend val = do
    (args, opts) <- get
    put (args, opts { optionsCsvAppend = val })

setForbidFused :: Monad m => Bool -> StateT ([CommandLineOption], Options) m ()
setForbidFused val = do
    (args, opts) <- get
    put (args, opts { optionsForbidFused = val })

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
            "csv-append" -> setCsvAppend True
            "forbid-fused" -> setForbidFused True
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

dumpBindCorePass
    :: DynFlags -> Int -> SDoc -> String -> String
    -> [(CoreBndr, CoreExpr)] -> CoreBndr -> CoreM ()
dumpBindCorePass dflags counter title pkgName modName allBinds b =
    void $ writeBindCore dflags pkgName modName
        ("." ++ dumpCoreSuffix dflags counter title ++ "dump-simpl") allBinds b

dumpCoreLocationMsg :: DynFlags -> String -> String -> String
dumpCoreLocationMsg dflags pkgName modName
    | not (gopt Opt_DumpToFile dflags) =
        "stdout (pass -ddump-to-file to write dump files instead)"
    | otherwise = pluginDumpStem dflags pkgName modName ++ "*"

dumpCoreLocationPass :: CoreToDo
dumpCoreLocationPass =
    CoreDoPluginPass "report dump-core location" $ \guts -> do
        dflags <- getDynFlags
        let pkgName = modulePackageName (mg_module guts)
            modName = moduleNameString (moduleName (mg_module guts))
        putMsgS $ "fusion-plugin: dumped core to "
            ++ dumpCoreLocationMsg dflags pkgName modName
        return guts

-------------------------------------------------------------------------------
-- Per-binding per-pass core dump (the 'DumpCorePasses' annotation)
-------------------------------------------------------------------------------

dumpCorePassesBinds
    :: IORef (Maybe (DUMP_CORE_PASSES_FM)) -> Int -> SDoc -> ModGuts
    -> CoreM ModGuts
dumpCorePassesBinds ref counter title guts = do
    cached <- liftIO $ readIORef ref
    dumpAnns <- case cached of
        Just anns -> return anns
        Nothing -> do
            anns <-
                getAnnotationsByStableName
                    "DumpCorePasses" deserializeWithData guts
            liftIO $ writeIORef ref (Just anns)
            return anns
    unless (Map.null dumpAnns) $ do
        dflags <- getDynFlags
        let pkgName = modulePackageName (mg_module guts)
            modName = moduleNameString (moduleName (mg_module guts))
            allBinds = flattenBinds (mg_binds guts)
        mapM_
            (\(b, _) ->
                case lookupBinderAnn b dumpAnns of
                    Just _ | not (subsumedBySameName allBinds b) ->
                        dumpBindCorePass
                            dflags counter title pkgName modName allBinds b
                    _ -> return ())
            allBinds
    return guts

dumpCorePassesLocationPass :: IORef (Maybe (DUMP_CORE_PASSES_FM)) -> CoreToDo
dumpCorePassesLocationPass ref =
    CoreDoPluginPass "report DumpCorePasses location" $ \guts -> do
        dflags <- getDynFlags
        cached <- liftIO $ readIORef ref
        let dumpAnns = fromMaybe Map.empty cached
            pkgName = modulePackageName (mg_module guts)
            modName = moduleNameString (moduleName (mg_module guts))
            dir = pluginDumpDir dflags pkgName
        forM_ (Map.keys dumpAnns) $ \name ->
            putMsgS $ "fusion-plugin: dumped per-pass core for " ++ name
                ++ " to " ++ (dir </> (modName ++ "." ++ name ++ ".*.dump-simpl"))
        return guts

insertDumpPasses
    :: Bool -> Bool -> IORef (Maybe (DUMP_CORE_PASSES_FM)) -> [CoreToDo]
    -> [CoreToDo]
insertDumpPasses verbose dumpWhole ref todos =
    dumpPass 0 (text "Initial ")
        : go 1 todos
        ++ [dumpCoreLocationPass | dumpWhole]
        ++ [dumpCorePassesLocationPass ref]

    where

    dumpPass counter title =
        CoreDoPluginPass "fusion-plugin dump core" $ \guts -> do
            when dumpWhole $ void $ dumpCore verbose counter title guts
            dumpCorePassesBinds ref counter title guts

    go _ [] = []
    go counter (todo:rest) =
        todo
            : dumpPass counter (text "After " GhcPlugins.<> ppr todo)
            : go (counter + 1) rest

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

isFusionPluginMarker :: CoreToDo -> Bool
isFusionPluginMarker (CoreDoPluginPass "installed" _) = True
isFusionPluginMarker _ = False

fusionPluginMarker :: CoreToDo
fusionPluginMarker = CoreDoPluginPass "installed" return

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos
    | any isFusionPluginMarker todos = do
        putMsgS $ "fusion-plugin: warning: plugin is already installed for "
            ++ "this module (check for a duplicate -fplugin=Fusion.Plugin "
            ++ "in ghc-options), skipping the duplicate installation"
        return todos
    | otherwise = do
        options <- liftIO $ parseOptions args
        dflags <- getDynFlags
        hscEnv <- getHscEnv
        dumpPassesRef <- liftIO $ newIORef Nothing
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
            insertDumpPasses
                (optionsVerbosityLevel options /= ReportSilent)
                (optionsDumpCore options) dumpPassesRef $
            insertAfterSimplPhase0
                todos
                [ fusionPluginMarker
                , fusionMarkInline 1 ReportSilent True
                , simplify
                , fusionMarkInline 2 ReportSilent True
                , simplify
                , fusionMarkInline 3 ReportSilent True
                , simplify
                -- This lets us know what was left unfused after all the
                -- inlining and case-of-case transformations.
                , let mesg = "Check unfused (post inlining)"
                  in CoreDoPluginPass mesg
                        (fusionReport mesg ReportSilent False options)
                ]
                (let mesg = "Check unfused (final)"
                     report =
                        fusionReport
                            mesg (optionsVerbosityLevel options) True options
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
