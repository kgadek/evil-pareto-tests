{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Monoid
import Data.Word

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

class Individual (a :: * -> * -> *) x y where
    newIndividual :: (PrimMonad m) => R.Gen (PrimState m) -> m (a x y)


--------------------------------------------------------------------------------
-- EA INSTANCE

type Domain   = Float
type CoDomain = Float


newtype IndividualBox x y = IndividualBox (x,y)

instance Individual IndividualBox Domain () where
    newIndividual gen = R.uniformR (-5,5) gen >>= return . IndividualBox . (,())

instance Individual IndividualBox Domain CoDomain where
    newIndividual gen = newIndividual gen >>= return . fitnessify

instance (PPrintable x, PPrintable y) => Show (IndividualBox x y) where
    show = PP.render . pprint

instance (PPrintable x, PPrintable y) => PPrintable (IndividualBox x y) where
    pprint (IndividualBox (x,y)) = pprint x PP.<> PP.char '↣' PP.<> pprint y


fitness :: Domain -> CoDomain
fitness x = (x-1)*(x+2)


--------------------------------------------------------------------------------
-- RANDOM

type RandomSeed = V.Vector Word32

genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)

generatePopulation :: (Individual a x y) => Int -> R.GenST s -> ST s (V.Vector (a x y))
generatePopulation size gen = V.fromList <$> replicateM size (newIndividual gen)


--------------------------------------------------------------------------------
-- EA OPERATORS

iteration :: (Individual a x y) => RandomSeed -> ST s (V.Vector (a x y), V.Vector (a x y))
iteration seed = do
    gen <- R.initialize seed
    initial_population1 <- generatePopulation population_size gen
    initial_population2 <- generatePopulation population_size gen
    return (initial_population1, initial_population2)
  where
    population_size = 20


--------------------------------------------------------------------------------


fitnessify :: IndividualBox Domain () -> IndividualBox Domain CoDomain
fitnessify (IndividualBox (x, ())) = IndividualBox (x, fitness x)


initialize :: [IndividualBox Domain ()]
initialize = IndividualBox . (,()) <$> [-5,-4.75..5] -- TODO [kgdk] 8 mar 2015: randomize

mutate :: IndividualBox Domain a -> IndividualBox Domain ()
mutate (IndividualBox (x,_)) = IndividualBox . (,()) $ x + 0.15

crossover :: IndividualBox Domain a -> IndividualBox Domain a -> [IndividualBox Domain ()]
crossover (IndividualBox (x, _)) (IndividualBox (y, _)) = [IndividualBox (x + y / 2, ())]

main = do
    let init = initialize
        init' = fitnessify <$> init
        select1 = init !! 3
        select2 = init !! 30
        (cross1:_) = crossover select1 select2
        mutate1 = mutate cross1
        --newPop = concatMap (fmap mutate . crossover )
    print $ init
    print $ init'
    print $ select1
    print $ select2
    print $ cross1
    print $ mutate1

    seed <- genSeed
    --print seed
    --let x = runST $ test seed
    --print x
    let (res :: (V.Vector (IndividualBox Domain CoDomain), V.Vector (IndividualBox Domain CoDomain))) = runST $ iteration seed
    print res


    putStrLn "OHAI"