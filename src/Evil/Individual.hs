{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evil.Individual (
    IsFitnessEvaluated(..),
    Individual(..)
  ) where


-- evil
import Evil.Spaces
import Evil.PPrintable
-- mwc-random
import qualified System.Random.MWC as R
-- pretty
import qualified Text.PrettyPrint as PP
-- primitive
import Control.Monad.Primitive



data IsFitnessEvaluated = WithFit | NoFit

class (Domain x, CoDomain x y) => Individual (a :: * -> * -> IsFitnessEvaluated -> *) x y where
    newIndividual :: (PrimMonad m) => R.Gen (PrimState m) -> m (a x y NoFit)
    mkIndividual  :: x -> a x y NoFit
    fitnessify    :: a x y NoFit -> a x y WithFit
    getFitness    :: a x y WithFit -> y
    getIndividual :: a x y fe -> x


instance (Domain x, PPrintable x, Individual a x y) => PPrintable (a x y NoFit) where
    pprint = pprint . getIndividual

instance (Domain x, PPrintable x, PPrintable y, Individual a x y) => PPrintable (a x y WithFit) where
    pprint x = px PP.<> PP.char '↣' PP.<> py
      where px = pprint $ getIndividual x
            py = pprint $ getFitness x

instance (Domain x, PPrintable x, Individual a x y) => Show (a x y NoFit) where
    show = PP.render . pprint
instance (Domain x, PPrintable x, PPrintable y, Individual a x y) => Show (a x y WithFit) where
    show = PP.render . pprint
