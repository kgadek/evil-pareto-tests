module Main where

-- mwc-random
import qualified System.Random.MWC as R

-- evil
import RandMonadT
import RandTools
import EA
import EA.SGAHello






populationSize, eaSteps :: Int
populationSize = 2048
eaSteps        =  100



main :: IO ()
main = do
    rseed <- genSeed
    rgen  <- R.initialize rseed

    (result, _) <- flip runRandMonadT rgen $ do
        initial <- mkSGA populationSize
        final   <- reFeedM eaSteps nextStep initial
        return $ getStats final
    print result


reFeedM :: (Monad m) => Int -> (a -> m a) -> a -> m a
reFeedM n act v | n > 0      = act v >>= reFeedM (n-1) act
                | otherwise  = return v
