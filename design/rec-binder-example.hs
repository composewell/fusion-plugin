module Main (main) where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import Streamly.Internal.Data.Time.Units

main = do
      S.drain
    $ S.classifySessionsOf 3 (const (return False)) (fmap Right FL.drain)
    $ S.map (\(ts,(k,a)) -> (k, a, ts))
    $ S.timestamped
    $ (,) <$> S.enumerateFromTo (1 :: Int) 1000 <*> S.enumerateFromTo 1 (100 :: Int)
