{-# LANGUAGE MultiParamTypeClasses #-}

module Evil.Spaces (
    Domain(..), CoDomain(..)
  ) where


-- mwc-random
import qualified System.Random.MWC as R
-- primitive
import Control.Monad.Primitive



class Domain x where
    mkDomain :: (PrimMonad m) => R.Gen (PrimState m) -> m x

class CoDomain x y where
    fitness :: x -> y
