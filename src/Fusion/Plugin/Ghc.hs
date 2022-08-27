-- |
-- Module      : Fusion.Plugin.Ghc
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : Apache-2.0
-- Maintainer  : ps@pranaysashank.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

#if MIN_VERSION_ghc(8,6,0)
module Fusion.Plugin.Ghc
    ( coreToDo
    , dumpCore
    , isPhase0MainTodo
    , defaultPurePlugin
    )
where

#if MIN_VERSION_ghc(9,5,0)
import Fusion.Plugin.GhcHead
#elif MIN_VERSION_ghc(9,4,0)
import Fusion.Plugin.Ghc940
#elif MIN_VERSION_ghc(9,3,0)
import Fusion.Plugin.Ghc930
#elif MIN_VERSION_ghc(9,2,2)
import Fusion.Plugin.Ghc922
#elif MIN_VERSION_ghc(9,2,0)
import Fusion.Plugin.Ghc920
#elif MIN_VERSION_ghc(9,0,0)
import Fusion.Plugin.Ghc900
#elif MIN_VERSION_ghc(8,6,0)
import Fusion.Plugin.Ghc860
#endif
#else
module Fusion.Plugin.Ghc
    ( defaultPurePlugin
    )
where

import GhcPlugins

defaultPurePlugin :: Plugin
defaultPurePlugin = defaultPlugin

#endif
