{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Random.Instances () where

import           Control.Monad.Error.Class (MonadError(throwError, catchError))
import           Control.Monad.Random (RandT, liftRandT, runRandT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(..))

instance (MonadError e m) => MonadError e (RandT g m) where
  throwError e = lift (throwError e)
  {-# INLINE throwError #-}
  catchError m f = liftRandT (runStateT (catchError (StateT (runRandT m)) (StateT . runRandT . f)))
  {-# INLINE catchError #-}
