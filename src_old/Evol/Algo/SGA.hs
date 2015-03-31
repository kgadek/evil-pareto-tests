module Evol.Algo.SGA (
  ) where


-- evil
import Evil.EvoAlgorithm
import Evil.Individual
import Evil.Spaces
import Evil.PPrintable
import Evil.RandUtils
-- vector
import qualified Data.Vector as V


newtype SGA = SGA

instance EvoAlgorithm SGA ()  where
    initialize () gen = undefined
    nextGen = undefined
