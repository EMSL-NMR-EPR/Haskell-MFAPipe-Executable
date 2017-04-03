{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Instances
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan instances of 'MonadError'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Log.Logger.Instances () where

import           Control.Monad.Error.Class (MonadError(..))
import           System.Log.Logger.Base
import           System.Log.Logger.Drop
import           System.Log.Logger.Handler
import           System.Log.Logger.Priority
import           System.Log.Logger.Thread
import           System.Log.Logger.Writer

instance (MonadError e m) => MonadError e (BaseLoggerT l m) where
  throwError e = BaseLoggerT (throwError e)
  {-# INLINE throwError #-}
  catchError (BaseLoggerT m) f = BaseLoggerT (catchError m (runRawBaseLoggerT . f))
  {-# INLINE catchError #-}

instance (MonadError e m) => MonadError e (DropLoggerT m) where
  throwError e = DropLoggerT (throwError e)
  {-# INLINE throwError #-}
  catchError (DropLoggerT m) f = DropLoggerT (catchError m (runDropLoggerT . f))
  {-# INLINE catchError #-}

instance (MonadError e m) => MonadError e (HandlerLoggerT m) where
  throwError e = HandlerLoggerT (throwError e)
  {-# INLINE throwError #-}
  catchError (HandlerLoggerT m) f = HandlerLoggerT (catchError m (fromHandlerLogger . f))
  {-# INLINE catchError #-}

instance (MonadError e m) => MonadError e (PriorityLoggerT m) where
  throwError e = PriorityLoggerT (throwError e)
  {-# INLINE throwError #-}
  catchError (PriorityLoggerT m) f = PriorityLoggerT (catchError m (fromPriorityLoggerT . f))
  {-# INLINE catchError #-}

instance (MonadError e m) => MonadError e (ThreadedLoggerT' d r m) where
  throwError e = ThreadedLoggerT' (throwError e)
  {-# INLINE throwError #-}
  catchError (ThreadedLoggerT' m) f = ThreadedLoggerT' (catchError m (fromThreadedLoggerT . f))
  {-# INLINE catchError #-}

instance (MonadError e m) => MonadError e (WriterLoggerT m) where
  throwError e = WriterLoggerT (throwError e)
  {-# INLINE throwError #-}
  catchError (WriterLoggerT m) f = WriterLoggerT (catchError m (fromWriterLoggerT . f))
  {-# INLINE catchError #-}
