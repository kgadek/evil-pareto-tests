{-# LANGUAGE ExistentialQuantification #-}

module Main where

---- base
--import Control.Applicative
--import Control.Monad
--import Control.Monad.ST
---- mwc-random
--import qualified System.Random.MWC as R
---- vector
--import qualified Data.Vector as V
---- evil
--import Evil.EvoAlgorithm
--import Evil.Individual
--import Evil.Spaces
--import Evil.PPrintable
--import Evil.RandUtils


data EA c = EA {
     container :: c,
     nextGen   :: EA c -> (EA c, String),
     getInd    :: c -> Int
  }

mkEA :: EA [Int]
mkEA = EA { container = [1,2,3]
          , nextGen   = next
          , getInd    = head
          }
  where next ea = (ea{container = map (*2) $ container ea}, "DUPA")

main :: IO ()
main = do
    --let ea = EA {container = [1,2,3], nextGen = aux, getInd = [1,2,3] }
    let x = mkEA
    --let (y, _) = nextGen x
    putStrLn "OHAI"
    print $ pack [1,1,1,2,3]
    --print y
  --where aux xs = (EA {container = map (*20) xs, nextGen = aux, getInd = (head $ map (*20) xs)}, "dupa")


encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xss@(x:xs) = (length a, x) : encode b
  where (a,b) = span (==x) xss

pack :: Eq a => [a] -> [[a]]
pack xss@(x:xs) = a : pack b
  where (a,b) = span (==x) xss