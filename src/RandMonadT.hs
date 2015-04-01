{-# LANGUAGE TupleSections #-}

module RandMonadT where


-- base
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad

-- transformers
import Control.Monad.Trans.Class

-- primitive
import Control.Monad.Primitive (PrimState)

-- mwc-random
import qualified System.Random.MWC as R



newtype RandMonadT m a = RandMonadT { runRandMonadT :: R.Gen (PrimState m) -> m (a, R.Gen (PrimState m)) }


instance (Monad m) => Functor (RandMonadT m) where
    fmap f a = RandMonadT $ \r -> do
        (x, r') <- runRandMonadT a r
        return (f x, r')

instance (Monad m) => Applicative (RandMonadT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (RandMonadT m) where
  return x = RandMonadT $ \r -> return (x, r)
  x >>= fm = RandMonadT $ \r ->
    do  (a, r') <- runRandMonadT x r
        runRandMonadT (fm a) r'

instance MonadTrans RandMonadT where
  lift x = RandMonadT $ \r -> liftM (,r) x

instance (MonadIO m) => MonadIO (RandMonadT m) where
  liftIO = lift . liftIO



liftR :: (Monad m) => (R.Gen (PrimState m) -> m a) -> RandMonadT m a
liftR f = RandMonadT $ \r -> f r >>= return.(,r)
