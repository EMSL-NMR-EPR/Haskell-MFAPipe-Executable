{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Random.Logger.Instances () where

import           Control.Monad.Random (RandT)
import           Control.Monad.Random.Instances ()
import           Control.Monad.Trans.Class (lift)
import           System.Log.Data (MonadRecord(appendRecord))

instance (Monad m, MonadRecord d m) => MonadRecord d (RandT g m) where
  appendRecord x = lift (appendRecord x)
  {-# INLINE appendRecord #-}
