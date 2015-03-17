--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeFamilies #-}
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
type Representation a = V.Vector a
type PopulationSize = Int
type Domain = Float
type Codomain = Float
--------------------------------------------------------------------------------
data IsFitnessEvaluated = WithFit | NoFit
data Individual (fe :: IsFitnessEvaluated) where
    IndividualFit   :: Domain  -> Codomain -> Individual WithFit
    IndividualNofit :: Domain  ->             Individual NoFit
--------------------------------------------------------------------------------



mkPopulation :: PopulationSize -> R.GenST s -> ST s (Representation (Individual NoFit))
mkPopulation = undefined

iteration :: Representation (Individual NoFit) -> R.GenST s -> ST s (Representation (Individual NoFit))
iteration = undefined


--------------------------------------------------------------------------------
data PartialOrder = Superior
                  | Inferior
                  | Incomparable

class Poset a where
    partialOrder :: a -> a -> PartialOrder
--------------------------------------------------------------------------------


-- representation
-- funkcja fitness
-- similarity
-- generacja
-- mutacja
-- selekcja
-- crossover

--class GenCodomain codomain where
--    compareCod :: codomain -> codomain -> ParetoCompare

--class GenDomain domain codomain where
--    isSimilar  :: domain -> domain -> Bool
--    fitness    :: domain -> codomain
--    crossover  :: domain -> domain -> domain
--    mutate     :: domain -> ST s domain
--    generate   :: ST s domain

--class GenRepr repr domain codomain | repr -> domain codomain where
    --select     :: repr -> ST s domain





--newtype Codomain3D = Codomain3D { _getCodomain3D :: (Int,Int,Int) }

--instance GenCodomain Codomain3D where
--  compareCod (Codomain3D (x1,y1,z1)) (Codomain3D (x2,y2,z2))
--      | x1 <= x2 && y1 <= y2 && z1 <= z2 = Inferior
--      | x1 >= x2 && y1 >= y2 && z1 >= z2 = Superior
--      | otherwise                        = Incomparable





main = do
    putStrLn "NO SIEMA"
