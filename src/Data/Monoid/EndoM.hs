-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.EndoM
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Endomorphism as Kleisli arrow.
--
-----------------------------------------------------------------------------

module Data.Monoid.EndoM where

import           Control.Monad ((>=>))

-- | Monoid under Kleisli composition.
newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance (Monad m) => Monoid (EndoM m a) where
  mempty = EndoM return
  {-# INLINE mempty #-}
  (EndoM f) `mappend` (EndoM g) = EndoM (f >=> g)
  {-# INLINE mappend #-}
