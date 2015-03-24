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

-- base
import Control.Applicative
import Control.Monad
import Control.Monad.ST
-- mwc-random
import qualified System.Random.MWC as R
-- vector
import qualified Data.Vector as V
-- evil
import Evil.EvoAlgorithm
import Evil.Individual
import Evil.Spaces
import Evil.PPrintable
import Evil.RandUtils



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


type VInds a = V.Vector (IndividualContainer DomainFloat CoDomainFloat a)

main :: IO ()
main = do
    seed <- genSeed
    let (res :: (VInds NoFit, VInds NoFit)) = runST $ iteration seed
    print $ fmap (fmap fitnessify )res
    putStrLn "OHAI"
