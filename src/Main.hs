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
{-# LANGUAGE UndecidableInstances #-}
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

--class PPrintable a where
--    pprint :: a -> Doc

--instance PPrintable () where pprint = const $ PP.text "()"
--instance PPrintable Float where pprint = PP.float


--------------------------------------------------------------------------------
-- EA CLASSES

class Individual (a :: * -> * -> IsFitnessEvaluated -> *) x y where
    --data Foo (b :: IsFitnessEvaluated) :: *
    --data BoxY a x y :: *
    --data BoxN a x y :: *

    --newIndividual :: (PrimMonad m) => R.Gen (PrimState m) -> m (Box a x y No)

    mkIndividual :: x -> a x y No

    --fitnessify :: BoxN a x y -> BoxY a x y

    --getIndividualFoo :: BoxN a x y -> x
    --getFitness :: BoxY a x y -> y


data IsFitnessEvaluated = Yes | No


type Domain   = Float
type CoDomain = Int


--newtype IndividualBox x y (fe :: IsFitnessEvaluated) = IndividualBox (x,y)
data IndividualBox x y (fe :: IsFitnessEvaluated) where
    IndividualBoxFit   :: x -> y -> IndividualBox x y Yes
    IndividualBoxNofit :: x -> IndividualBox x y No

instance Individual IndividualBox Domain CoDomain where
    --data Foo No  = FooIBN (IndividualBox Domain () No)
    --data Foo Yes = FooIBY (IndividualBox Domain () Yes)
    --data BoxY IndividualBox Domain CoDomain = IndividualBox Domain CoDomain Yes
    --data BoxN IndividualBox Domain CoDomain = IndividualBox Domain ()       No

    --newIndividual gen = R.uniformR (-5,5) gen >>= return . IndividualBox . (,())
    --fitnessify (IndividualBox (x,())) = IndividualBox (x, 123)

    --mkIndividual :: Domain -> Box IndividualBox Domain CoDomain No
    mkIndividual x = IndividualBoxNofit x

    --getIndividualFoo (IndividualBox (x, _)) = x
    --getFitness (IndividualBox (_, y)) = y



--test2 :: Box IndividualBox Domain CoDomain No
--test2 :: Foo No
test2 :: IndividualBox Domain CoDomain No
test2 = mkIndividual (4 :: Domain)


--test :: Domain
--test = getIndividualFoo x
--  where x = mkIndividual (5 :: Domain) :: IndividualBox Domain CoDomain No

--instance (PPrintable x, Individual IndividualBox x y No) => Show (IndividualBox x y No) where
--    show = PP.render . pprint . getIndividual

--instance (PPrintable x, PPrintable y, Individual IndividualBox x y Yes) => Show (IndividualBox x y Yes) where
--    show = PP.render . pprint . getFitness

--test :: IndividualBox Domain CoDomain No
--test = mkIndividual 4 :: Individual IndividualBox Domain CoDomain No => IndividualBox Domain CoDomain No


--instance (PPrintable x, PPrintable x, PPrintable y) => Show (IndividualBox x y Yes) where
--    show x = "YE"

--instance (Individual a x y Yes, PPrintable x) => Show (Box a x y Yes) where
--    show = PP.render . pprint . getIndividual

--instance (Individual a x y Yes, PPrintable x, PPrintable y) => Show (a x y Yes) where
--    show x = PP.render (pprint (getIndividual x) PP.<> PP.char '↣' PP.<> pprint (getFitness x))



--instance (PPrintable x, PPrintable y) => Show (IndividualBox x y Yes) where
--    show = PP.render . pprint

--instance (PPrintable x, PPrintable y) => Show (IndividualBox x y No) where
--    show = PP.render . pprint

--instance (PPrintable x, PPrintable y) => PPrintable (IndividualBox x y Yes) where
--    pprint (IndividualBox (x,y)) = pprint x PP.<> PP.char '↣' PP.<> pprint y

--instance (PPrintable x, PPrintable y) => PPrintable (IndividualBox x y No) where
--    pprint (IndividualBox (x,y)) = pprint x PP.<> PP.text "↣?"


--------------------------------------------------------------------------------
-- RANDOM

--type RandomSeed = V.Vector Word32

--genSeed :: IO RandomSeed
--genSeed = R.withSystemRandom aux
--  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)

--generatePopulation :: (Individual a x y No) => Int -> R.GenST s -> ST s (V.Vector (a x y))
--generatePopulation size gen = V.fromList <$> replicateM size (newIndividual gen)


----------------------------------------------------------------------------------
---- EA OPERATORS

--iteration :: (Individual a x y Yes) => RandomSeed -> ST s (V.Vector (a x y), V.Vector (a x y))
--iteration seed = do
--    gen <- R.initialize seed
--    initial_population1 <- generatePopulation population_size gen
--    initial_population2 <- generatePopulation population_size gen
--    return (initial_population1, initial_population2)
--  where
--    population_size = 20


--------------------------------------------------------------------------------


--fitnessify :: IndividualBox Domain () -> IndividualBox Domain CoDomain
--fitnessify (IndividualBox (x, ())) = IndividualBox (x, fitness x)


--initialize :: [IndividualBox Domain ()]
--initialize = IndividualBox . (,()) <$> [-5,-4.75..5] -- TODO [kgdk] 8 mar 2015: randomize

--mutate :: IndividualBox Domain a -> IndividualBox Domain ()
--mutate (IndividualBox (x,_)) = IndividualBox . (,()) $ x + 0.15

--crossover :: IndividualBox Domain a -> IndividualBox Domain a -> [IndividualBox Domain ()]
--crossover (IndividualBox (x, _)) (IndividualBox (y, _)) = [IndividualBox (x + y / 2, ())]

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

    --seed <- genSeed
    ----print seed
    ----let x = runST $ test seed
    ----print x
    --let (res :: (V.Vector (IndividualBox Domain CoDomain), V.Vector (IndividualBox Domain CoDomain))) = runST $ iteration seed
    --print res


    putStrLn "OHAI"

