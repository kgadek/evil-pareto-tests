{-# LANGUAGE ScopedTypeVariables #-}

module Evil.RandUtils (
    RandomSeed,
    genSeed
  ) where

-- base
import Control.Monad.ST
import Data.Word
-- vector
import qualified Data.Vector as V
-- mwc-random
import qualified System.Random.MWC as R



type RandomSeed = V.Vector Word32


genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)