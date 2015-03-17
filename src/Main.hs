{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
--import Data.Monoid
import Data.Word
--import Data.Proxy

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import qualified System.Random.MWC as R
import qualified Data.Vector as V


--------------------------------------------------------------------------------
-- CLASSES
--------------------------------------------------------------------------------
class PPrintable a where
    pprint :: a -> Doc
--------------------------------------------------------------------------------
class Domain x where
    mkDomain :: (PrimMonad m) => R.Gen (PrimState m) -> m x
--------------------------------------------------------------------------------
class CoDomain x y where
    fitness :: x -> y
--------------------------------------------------------------------------------
data IsFitnessEvaluated = WithFit | NoFit
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
class (Domain x) => Individual (a :: * -> * -> IsFitnessEvaluated -> *) x y where
    newIndividual :: (PrimMonad m) => R.Gen (PrimState m) -> m (a x y NoFit)
    mkIndividual  ::                  x -> a x y NoFit
    fitnessify    ::                  a x y NoFit -> a x y WithFit
    getIndividual :: forall fe_ .     a x y fe_ -> x
    getFitness    ::                  a x y WithFit -> y
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
instance PPrintable ()    where pprint = const $ PP.text "()"
instance PPrintable Float where pprint = PP.float
--------------------------------------------------------------------------------
instance (Domain x, PPrintable x, Individual a x y)               => PPrintable (a x y NoFit) where
    pprint = pprint . getIndividual
instance (Domain x, PPrintable x, PPrintable y, Individual a x y) => PPrintable (a x y WithFit) where
    pprint x = px PP.<> PP.char '↣' PP.<> py
      where px = pprint $ getIndividual x
            py = pprint $ getFitness x
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
instance (Domain x, PPrintable x, Individual a x y)               => Show (a x y NoFit) where
    show = PP.render . pprint
instance (Domain x, PPrintable x, PPrintable y, Individual a x y) => Show (a x y WithFit) where
    show = PP.render . pprint
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- DOMAIN
--------------------------------------------------------------------------------
newtype DomainFloat = DomainFloat Float
--------------------------------------------------------------------------------
instance Domain DomainFloat where
    mkDomain gen = R.uniformR (-5,5) gen >>= return . DomainFloat
instance PPrintable DomainFloat where
    pprint (DomainFloat x) = pprint x
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- CoDOMAIN
--------------------------------------------------------------------------------
newtype CoDomainFloat = CoDomainFloat Float
--------------------------------------------------------------------------------
instance CoDomain DomainFloat CoDomainFloat where
    fitness (DomainFloat x) = CoDomainFloat ((x-1)*(x+2))
instance PPrintable CoDomainFloat where
    pprint (CoDomainFloat x) = pprint x
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Individual
--------------------------------------------------------------------------------
data IndividualContainer x y (fe :: IsFitnessEvaluated) where
    IndividualContainerFit   :: x -> y -> IndividualContainer x y WithFit
    IndividualContainerNofit :: x ->      IndividualContainer x y NoFit
--------------------------------------------------------------------------------
instance (Domain x, CoDomain x y) => Individual IndividualContainer x y where
    newIndividual gen = mkDomain gen >>= return . IndividualContainerNofit
    fitnessify (IndividualContainerNofit x) = IndividualContainerFit x (fitness x)

    mkIndividual x = IndividualContainerNofit x

    getIndividual (IndividualContainerFit x _) = x
    getIndividual (IndividualContainerNofit x) = x
    getFitness (IndividualContainerFit _ y) = y
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Random stuff
--------------------------------------------------------------------------------
type RandomSeed = V.Vector Word32
--------------------------------------------------------------------------------
genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)
--------------------------------------------------------------------------------
generatePopulation :: (Domain x, Individual a x y) => Int -> R.GenST s -> ST s (V.Vector (a x y NoFit))
generatePopulation size gen = V.fromList <$> replicateM size (newIndividual gen)
----------------------------------------------------------------------------------
iteration :: (Domain x, Individual a x y) => RandomSeed -> ST s (V.Vector (a x y NoFit), V.Vector (a x y NoFit))
iteration seed = do
    gen <- R.initialize seed
    initial_population1 <- generatePopulation population_size gen
    initial_population2 <- generatePopulation population_size gen
    return (initial_population1, initial_population2)
  where
    population_size = 20
--------------------------------------------------------------------------------


--mutate :: IndividualContainer Domain a -> IndividualContainer Domain ()
--mutate (IndividualContainer (x,_)) = IndividualContainer . (,()) $ x + 0.15

--crossover :: IndividualContainer Domain a -> IndividualContainer Domain a -> [IndividualContainer Domain ()]
--crossover (IndividualContainer (x, _)) (IndividualContainer (y, _)) = [IndividualContainer (x + y / 2, ())]
--crossover x y = _
--  where xx = getIndividual x
--        yy = getIndividual y

type VInds a = V.Vector (IndividualContainer DomainFloat CoDomainFloat a)

main :: IO ()
main = do
    --let init = initialize
    --    init' = fitnessify <$> init
    --    select1 = init !! 3
    --    select2 = init !! 30
    --    (cross1:_) = crossover select1 select2
    --    mutate1 = mutate cross1
    --    --newPop = concatMap (fmap mutate . crossover )
    --print $ init
    --print $ init'
    --print $ select1
    --print $ select2
    --print $ cross1
    --print $ mutate1

    seed <- genSeed
    let (res :: (VInds NoFit, VInds NoFit)) = runST $ iteration seed
    print $ fmap (fmap fitnessify )res


    putStrLn "OHAI"

