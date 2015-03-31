{-# LANGUAGE BangPatterns #-}

module RandTools (
    sample
  ) where


-- base
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Word
import Data.List
import Data.Ord
import Text.Printf
import Data.Foldable

-- transformers
import Control.Monad.Trans.Class

-- primitive
import Control.Monad.Primitive (PrimState,PrimMonad)

-- mwc-random
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as RD

-- vector
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

-- statistics
import qualified Statistics.Sample               as Stat
import qualified Statistics.Types                as StatTy
import qualified Statistics.Resampling           as StatRe
import qualified Statistics.Resampling.Bootstrap as StatReBoo

-- containers
import qualified Data.Sequence as Seq

--import Control.Monad.Primitive
--import Data.Foldable (toList)
--import System.Random.MWC



-- http://stackoverflow.com/a/13782206/547223
sample :: PrimMonad m => [a] -> Int -> R.Gen (PrimState m) -> m [a]
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
