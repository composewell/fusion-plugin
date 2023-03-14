-- |
-- Module      : Fusion.Plugin.Ghc930
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Fusion.Plugin.Ghc930
    ( coreToDo
    , dumpCore
    , isPhase0MainTodo
    , defaultPurePlugin
    )
where

-- Imports for specific compiler versions
import Data.Char (isSpace)
import Text.Printf (printf)
import GHC.Core.Ppr (pprCoreBindingsWithSize, pprRules)
import GHC.Types.Name.Ppr (mkPrintUnqualified)
import GHC.Utils.Logger (Logger)
import GHC.Utils.Logger (putDumpFile, logFlags, LogFlags(..))

-- Implicit imports

import GHC.Plugins
import qualified GHC.Plugins as GhcPlugins

coreToDo :: HscEnv -> DynFlags -> CoreToDo
coreToDo _hsc_env dflags =
    let mode =
            SimplMode
            { sm_phase = InitialPhase
            , sm_names = ["Fusion Plugin Inlining"]
            , sm_dflags = dflags
            , sm_rules = gopt Opt_EnableRewriteRules dflags
            , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
            , sm_inline = True
            , sm_case_case = True
            , sm_uf_opts = unfoldingOpts dflags
            , sm_pre_inline = gopt Opt_SimplPreInlining dflags
            , sm_logger = hsc_logger _hsc_env
            , sm_cast_swizzle = True
            }
    in CoreDoSimplify
        (maxSimplIterations dflags) mode

-------------------------------------------------------------------------------
-- Dump core passes
-------------------------------------------------------------------------------

dumpPassResult ::
      Logger
   -> DynFlags
   -> PrintUnqualified
   -> SDoc                  -- Header
   -> SDoc                  -- Extra info to appear after header
   -> CoreProgram -> [CoreRule]
   -> IO ()
dumpPassResult logger dflags unqual hdr extra_info binds rules = do
    let flags = logFlags logger
    let getDumpAction = putDumpFile
    (getDumpAction logger)
        flags dump_style Opt_D_dump_simpl title undefined dump_doc

    where

    title = showSDoc dflags hdr

    dump_style = mkDumpStyle unqual

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
    :: Logger
    -> DynFlags
    -> PrintUnqualified
    -> Int
    -> SDoc
    -> CoreProgram
    -> [CoreRule]
    -> IO ()
dumpResult logger dflags print_unqual counter todo binds rules =
    dumpPassResult logger1 dflags print_unqual hdr (text "") binds rules

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

    logger1 = logger

dumpCore :: Int -> SDoc -> ModGuts -> CoreM ()
dumpCore counter title guts = do
    dflags <- getDynFlags
    hscEnv <- getHscEnv
    let logger = hsc_logger hscEnv
    let print_unqual =
            mkPrintUnqualified (hsc_unit_env hscEnv) (mg_rdr_env guts)
    liftIO $ dumpResult logger dflags print_unqual counter
                title (mg_binds guts) (mg_rules guts)

isPhase0MainTodo :: CoreToDo :: Bool
isPhase0MainTodo (CoreDoSimplify _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            }) = True
isPhase0MainTodo _ = False

defaultPurePlugin :: Plugin
defaultPurePlugin = defaultPlugin
    { pluginRecompile = purePlugin
    }
