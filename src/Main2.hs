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
import Data.Monoid
import Data.Word
import Data.Proxy

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import qualified System.Random.MWC as R
import qualified Data.Vector as V


--------------------------------------------------------------------------------
-- TOOLS: PPrintable

class PPrintable a where
    pprint :: a -> Doc

instance PPrintable () where pprint = const $ PP.text "()"
instance PPrintable Float where pprint = PP.float


--------------------------------------------------------------------------------
-- EA CLASSES

class Individual (a :: * -> * -> IsFitnessEvaluated -> *) x y where
    newIndividual :: (PrimMonad m) => R.Gen (PrimState m) -> m (a x y No)

    mkIndividual :: x -> a x y No

    fitnessify :: a x y No -> a x y Yes

    getIndividual :: forall fe_ .  a x y fe_ -> x
    getFitness :: a x y Yes -> y


data IsFitnessEvaluated = Yes | No


type Domain   = Float
type CoDomain = Float


data IndividualBox x y (fe :: IsFitnessEvaluated) where
    IndividualBoxFit   :: x -> y -> IndividualBox x y Yes
    IndividualBoxNofit :: x -> IndividualBox x y No


instance Individual IndividualBox Domain CoDomain where
    newIndividual gen = R.uniformR (-5,5) gen >>= return . IndividualBoxNofit
    fitnessify (IndividualBoxNofit x) = IndividualBoxFit x ((x-1)*(x+2))

    mkIndividual x = IndividualBoxNofit x

    getIndividual (IndividualBoxFit x _) = x
    getIndividual (IndividualBoxNofit x) = x
    getFitness (IndividualBoxFit _ y) = y


instance (PPrintable x, Individual a x y)               => PPrintable (a x y No) where
    pprint = pprint . getIndividual

instance (PPrintable x, PPrintable y, Individual a x y) => PPrintable (a x y Yes) where
    pprint x = px PP.<> PP.char '↣' PP.<> py
      where px = pprint $ getIndividual x
            py = pprint $ getFitness x

instance (PPrintable x, Individual a x y)               => Show (a x y No) where
    show = PP.render . pprint

instance (PPrintable x, PPrintable y, Individual a x y) => Show (a x y Yes) where
    show = PP.render . pprint


--------------------------------------------------------------------------------
-- RANDOM

type RandomSeed = V.Vector Word32

genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)

generatePopulation :: (Individual a x y) => Int -> R.GenST s -> ST s (V.Vector (a x y No))
generatePopulation size gen = V.fromList <$> replicateM size (newIndividual gen)


----------------------------------------------------------------------------------
---- EA OPERATORS

iteration :: (Individual a x y) => RandomSeed -> ST s (V.Vector (a x y No), V.Vector (a x y No))
iteration seed = do
    gen <- R.initialize seed
    initial_population1 <- generatePopulation population_size gen
    initial_population2 <- generatePopulation population_size gen
    return (initial_population1, initial_population2)
  where
    population_size = 20


--------------------------------------------------------------------------------


--mutate :: IndividualBox Domain a -> IndividualBox Domain ()
--mutate (IndividualBox (x,_)) = IndividualBox . (,()) $ x + 0.15

--crossover :: IndividualBox Domain a -> IndividualBox Domain a -> [IndividualBox Domain ()]
--crossover (IndividualBox (x, _)) (IndividualBox (y, _)) = [IndividualBox (x + y / 2, ())]

type VInds a = V.Vector (IndividualBox Domain CoDomain a)

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
    let (res :: (VInds No, VInds No)) = runST $ iteration seed
    print $ fmap (fmap fitnessify )res


    putStrLn "OHAI"

