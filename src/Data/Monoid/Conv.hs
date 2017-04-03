{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Conv
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- One-dimensional convolution.
--
-----------------------------------------------------------------------------

module Data.Monoid.Conv where

import           Foreign.Storable (Storable())
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import qualified Numeric.LinearAlgebra.HMatrix

-- | Monoid under one-dimensional convolution.
newtype Conv e = Conv { getConv :: Vector e }
  deriving (Read, Show)

deriving instance (Storable e, Eq e) => Eq (Conv e)
deriving instance (Storable e, Ord e) => Ord (Conv e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (Conv e) where
  mempty = Conv (Numeric.LinearAlgebra.HMatrix.konst 1 1)
  {-# INLINE mempty #-}
  (Conv vL) `mappend` (Conv vR) = Conv (vL `Numeric.LinearAlgebra.HMatrix.conv` vR)
  {-# INLINE mappend #-}

conv :: (Foldable f, Container Vector e, Num (Vector e), Product e) => f (Vector e) -> Vector e
conv = getConv . foldMap Conv
