{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- base
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Word
import Data.List
import Data.Ord
import Text.Printf
import Data.Function
import Data.Char

-- transformers
import Control.Monad.Trans.Class

-- primitive
import Control.Monad.Primitive (PrimState,PrimMonad)

-- mwc-random
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as RD

-- vector
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- statistics
import qualified Statistics.Sample               as Stat
import qualified Statistics.Types                as StatTy
import qualified Statistics.Resampling           as StatRe
import qualified Statistics.Resampling.Bootstrap as StatReBoo

--
import RandMonadT
import RandTools



type RandomSeed = V.Vector Word32

genSeed :: IO RandomSeed
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 256 :: ST s (V.Vector Word32)



type Individual = String
type CoDomain = Int



fitness :: Individual -> (Individual, CoDomain)
fitness x = (x, go x)
  where go x = sum $ zipWith (\a b -> abs $ ((-) `on` ord) a b) x "hello world"


mkSGA :: (MonadIO m, PrimMonad m) => Int -> RandMonadT m (Vector Individual)
mkSGA popSize = do
    V.replicateM popSize $ replicateM 11 (liftR (R.uniformR (32,127)) >>= return.chr)
    

trim = id
--trim x | x >  5.0  =  5.0
--       | x < -5.0  = -5.0
--       | otherwise = x

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
    let res = head . map fst . sortBy (comparing snd) . map fitness $ ps
    --liftIO $ printf "probe = %v\n  res = %v\n" (show ps) (show $ fitness res)
    return res

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
    putStrLn "AAA"

    rseed <- genSeed
    rgen  <- R.initialize rseed -- $ V.fromList [5,1,9]

    (resG, _) <- flip runRandMonadT rgen $ do
        sga   <- mkSGA 2048
        --stats sga (0 :: Int)
        foldM aux sga ([1..100] :: [Int])

    return ()

  where
    aux s i = do
      let ls = fromIntegral $ V.length s
          ls10 = floor $ (0.10 :: Double) * ls
          ls90 = V.length s - ls10
          ordr = V.fromList . map fst . sortBy (comparing snd) . V.toList . V.map fitness
      res <- stepSGA s
      let resS = ordr res
          sS   = ordr s
          res' = V.take ls90 resS V.++ V.take ls10 sS
      --stats res' i
      return res'

    stats p i = do
          --xxx <- liftR (\r -> liftIO $ StatRe.resample r [StatTy.Mean] 100 p)
          --let mean   = Stat.mean p
              --stdDev = Stat.stdDev p
              --bMean  = StatReBoo.bootstrapBCA 0.95 p [StatTy.Mean] xxx
          --liftIO $ putStrLn ("=====" ++ show i ++ "=====")
          --liftIO $ printf   "• mean:   %.3f\n• bMean:  %s\n• stdDev: %.3f\n" mean (show bMean) stdDev
          liftIO $ (putStrLn . intercalate ", " . map form . sortBy (comparing snd) . map fitness . V.toList) p

    form (x,f) = printf "%s↦%d" x f