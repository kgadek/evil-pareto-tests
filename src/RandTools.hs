{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandTools where


-- base
import Data.Foldable
import Data.Word
import Control.Monad.ST

-- primitive
import Control.Monad.Primitive (PrimState,PrimMonad)

-- mwc-random
import qualified System.Random.MWC as R

-- containers
import qualified Data.Sequence as Seq

-- vector
import qualified Data.Vector as V



-- http://stackoverflow.com/a/13782206/547223
sample :: (PrimMonad m) => [a] -> Int -> R.Gen (PrimState m) -> m [a]
sample ys size = go 0 (l - 1) (Seq.fromList ys)
  where
    l = length ys
    go !n !i xs g | n >= size = return $! (toList . Seq.drop (l - size)) xs
                  | otherwise = do
                      j <- R.uniformR (0, i) g
                      let toI  = xs `Seq.index` j
                          toJ  = xs `Seq.index` i
                          next = (Seq.update i toI . Seq.update j toJ) xs
                      go (n + 1) (i - 1) next g
{-# INLINE sample #-}


type RandomSeed = V.Vector Word32


genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)
