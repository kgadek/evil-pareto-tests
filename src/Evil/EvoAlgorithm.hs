{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evil.EvoAlgorithm (
    EvoAlgorithm(..)
  ) where


-- evil
import Evil.Individual
import Evil.Spaces
-- primitive
import Control.Monad.Primitive
-- mwc-random
import qualified System.Random.MWC as R


class (Individual indiv x y) => EvoAlgorithm ea params indiv x y | ea x -> params where
    initialize :: (PrimMonad m) => params -> R.Gen (PrimState m) -> m ea
    nextGen    :: (PrimMonad m) => ea -> m (Maybe ea)