-- |
-- Module      : Fusion.Plugin.Ghc900
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Fusion.Plugin.Ghc900
    ( coreToDo
    , dumpCore
    , isPhase0MainTodo
    , defaultPurePlugin
    )
where

import GHC.Plugins

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
            }
    in CoreDoSimplify
        (maxSimplIterations dflags) mode

dumpCore :: Int -> SDoc -> ModGuts -> CoreM ()
dumpCore _ _ _ = do
    putMsgS $ "fusion-plugin: dump-core not supported on GHC 9.0 "

isPhase0MainTodo :: CoreToDo -> Bool
isPhase0MainTodo (CoreDoSimplify _ SimplMode
            { sm_phase = Phase 0
            , sm_names = ["main"]
            }) = True
isPhase0MainTodo _ = False

defaultPurePlugin :: Plugin
defaultPurePlugin = defaultPlugin
    { pluginRecompile = purePlugin
    }
