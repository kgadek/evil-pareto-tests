{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Ord
import Text.Printf
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
import RandMonadT
import RandTools



type Individual = String
type CoDomain   = Int


populationSize, eaSteps :: Int
populationSize = 2048
eaSteps        =  100


fitness :: Individual -> (Individual, CoDomain)
fitness x = (x, fitness_x)
  where fitness_x = sum $ zipWith (\a b -> abs $ ((-) `on` ord) a b) x "hello world"


mkSGA :: (MonadIO m, PrimMonad m) => Int -> RandMonadT m (Vector Individual)
mkSGA popSize = do
    V.replicateM popSize $ replicateM 11 (liftR (R.uniformR (32,127)) >>= return.chr)


stepSGA :: (MonadIO m, PrimMonad m) => Vector Individual -> RandMonadT m (Vector Individual)
stepSGA is = do
    newPop <- V.replicateM popSize $ do
        x  <- selectSGA 2 is
        y  <- selectSGA 2 is
        xy <- crossoverSGA x y
        mutateSGA xy
    return newPop
  where popSize = V.length is


selectSGA :: (MonadIO m, PrimMonad m) => Int -> Vector Individual -> RandMonadT m Individual
selectSGA probe is = do
    ps <- liftR $ sample (V.toList is) probe
    return . head . map fst . sortBy (comparing snd) . map fitness $ ps


crossoverSGA :: (MonadIO m, PrimMonad m) => Individual -> Individual -> RandMonadT m Individual
crossoverSGA x y = do
    tmp <- liftR $ R.uniformR (0,11)
    let (xa,_) = splitAt tmp x
        (_,yb) = splitAt tmp y
    return $ xa ++ yb


mutateSGA :: (MonadIO m, PrimMonad m) => Individual -> RandMonadT m Individual
mutateSGA i = do
    (coin :: Float) <- liftR $ R.uniformR (0,100)
    if coin > 95.0
      then do
        pos <- liftR $ R.uniformR (0,10)
        new <- liftR $ R.uniformR (32,127)
        let (ia,(_:ib)) = splitAt pos i
        return $ ia ++ [chr new] ++ ib
      else return i

main :: IO ()
main = do
    rseed <- genSeed
    rgen  <- R.initialize rseed

    (resG, _) <- flip runRandMonadT rgen $ do
        sga   <- mkSGA populationSize
        stats sga
        foldM eaStep sga ([1..eaSteps])
    return ()

  where
    eaStep s i = do
      let ls = fromIntegral $ V.length s
          ls10 = floor $ (0.10 :: Double) * ls
          ls90 = V.length s - ls10
          ordr = V.fromList . map fst . sortBy (comparing snd) . V.toList . V.map fitness
      res <- stepSGA s
      let resS = ordr res
          sS   = ordr s
          res' = V.take ls90 resS V.++ V.take ls10 sS
      stats res'
      return res'

    stats p = liftIO $
      (putStrLn . intercalate ", " . map form . sortBy (comparing snd) . map fitness . V.toList) p

    form (x,f) = printf "%s↦%d" x f