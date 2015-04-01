{-# LANGUAGE ScopedTypeVariables #-}

module EA.SGAHello where

-- base
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Ord
import Data.Function
import Data.Char

-- primitive
import Control.Monad.Primitive (PrimMonad)

-- mwc-random
import qualified System.Random.MWC as R


-- vector
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- evil
import EA
import RandMonadT
import RandTools


type Individual = String
type CoDomain   = Int


target :: Individual
target = "Hello, world"


fitness :: Individual -> (Individual, CoDomain)
fitness x = (x, fitness_x)
  where fitness_x = sum $ zipWith (\a b -> abs $ ((-) `on` ord) a b) x target


type SGA_Hello = EA Individual CoDomain (Vector Individual)



mkSGA :: (MonadIO m, PrimMonad m) => Int -> RandMonadT m (SGA_Hello m)
mkSGA popSize = do
    pop <- V.replicateM popSize $ 
        replicateM wordlen (liftR (R.uniformR (32,127)) >>= return.chr)
    return $ EA { nextStep = stepSGA pop, getStats = pop}
  where
    wordlen = length target


stepSGA :: (PrimMonad m) => Vector Individual -> RandMonadT m (SGA_Hello m)
stepSGA is = do
    newPop <- V.replicateM popSize $ do
        x  <- selectSGA 2 is
        y  <- selectSGA 2 is
        xy <- crossoverSGA x y
        mutateSGA xy
    return EA { nextStep = stepSGA newPop
              , getStats = newPop
              }
  where popSize = V.length is


selectSGA :: (PrimMonad m) => Int -> Vector Individual -> RandMonadT m Individual
selectSGA probe is = do
    ps <- liftR $ sample (V.toList is) probe
    return . head . map fst . sortBy (comparing snd) . map fitness $ ps


crossoverSGA :: (PrimMonad m) => Individual -> Individual -> RandMonadT m Individual
crossoverSGA x y = do
    tmp <- liftR $ R.uniformR (0,11)
    let (xa,_) = splitAt tmp x
        (_,yb) = splitAt tmp y
    return $ xa ++ yb


mutateSGA :: (PrimMonad m) => Individual -> RandMonadT m Individual
mutateSGA i = do
    (coin :: Float) <- liftR $ R.uniformR (0,100)
    if coin > 95.0
      then do
        pos <- liftR $ R.uniformR (0,10)
        new <- liftR $ R.uniformR (32,127)
        let (ia,(_:ib)) = splitAt pos i
        return $ ia ++ [chr new] ++ ib
      else return i
