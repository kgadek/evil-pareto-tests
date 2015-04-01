{-# LANGUAGE RankNTypes #-}

module EA where


import RandMonadT



data EA domain codomain stats m = forall a. EA {
    nextStep :: RandMonadT m (EA domain codomain stats m)
  , getStats :: stats
}
