{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

test :: (V.Vector Word32) -> ST s Int
test seed = do
    R.initialize seed --   ST s (R.Gen (PrimState (ST s)))
                      -- ≡ ST s (R.GenST s)
    return 123

genSeed :: IO (V.Vector Word32)
genSeed = R.withSystemRandom aux
  where aux (gen::R.GenST s) = R.uniformVector gen 255 :: ST s (V.Vector Word32)



--------------------------------------------------------------------------------
-- TOOLS
--------------------------------------------------------------------------------

class PPrintable a where
    pprint :: a -> Doc

instance PPrintable () where pprint = const $ PP.text "()"

--------------------------------------------------------------------------------



type Domain   = Float
type CoDomain = Float

instance PPrintable Float where pprint = PP.float
--instance PPrintable Domain    where pprint = PP.float
--instance PPrintable CoDomain  where pprint = PP.float


newtype Individual a = Individual (Domain,a)

instance (PPrintable codomain) => Show (Individual codomain) where
    show = PP.render . pprint
instance (PPrintable codomain) => PPrintable (Individual codomain) where
    pprint (Individual (x,y)) = PP.float x PP.<> PP.char '↣' PP.<> pprint y


--------------------------------------------------------------------------------


fitness :: Domain -> CoDomain
fitness x = (x-1)*(x+2)

fitnessify :: Individual () -> Individual CoDomain
fitnessify (Individual (x, ())) = Individual (x, fitness x)


initialize :: [Individual ()]
initialize = Individual . (,()) <$> [-5,-4.75..5] -- TODO [kgdk] 8 mar 2015: randomize

mutate :: Individual a -> Individual ()
mutate (Individual (x,_)) = Individual . (,()) $ x + 0.15

crossover :: Individual a -> Individual a -> [Individual ()]
crossover (Individual (x, _)) (Individual (y, _)) = [Individual (x + y / 2, ())]

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
    print seed
    let x = runST $ test seed
    print x


    putStrLn "OHAI"