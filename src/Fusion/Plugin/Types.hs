-- |
-- Module      : Fusion.Plugin.Types
-- Copyright   : (c) 2019 Pranay Sashank
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@gmail.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE DeriveDataTypeable #-}

module Fusion.Plugin.Types
    ( ForceCaseCase (..)
    )
where

import Data.Data


-- | 'ForceCaseCase' is used to inform the plugin to aggressively
-- inline join points that perform a case match on a constructor. Use
-- this annotation on the constructor when it is statically known that
-- the elimination of the constructor would provide a significant
-- performance boost.
data ForceCaseCase =
    ForceCaseCase
    deriving (Data)
