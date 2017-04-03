{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.CartProduct
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Cartesian product.
--
-----------------------------------------------------------------------------

module Data.Monoid.CartProduct where

import           Foreign.Storable (Storable())
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import qualified Numeric.LinearAlgebra.HMatrix

-- | Monoid under Cartesian product.
newtype CartProduct e = CartProduct { getCartProduct :: Vector e }
  deriving (Read, Show)

deriving instance (Storable e, Eq e) => Eq (CartProduct e)
deriving instance (Storable e, Ord e) => Ord (CartProduct e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (CartProduct e) where
  mempty = CartProduct (Numeric.LinearAlgebra.HMatrix.konst 1 1)
  {-# INLINE mempty #-}
  (CartProduct vL) `mappend` (CartProduct vR) = CartProduct (Numeric.LinearAlgebra.HMatrix.flatten (vL `Numeric.LinearAlgebra.HMatrix.outer` vR))
  {-# INLINE mappend #-}

cartProduct :: (Foldable f, Container Vector e, Num (Vector e), Product e) => f (Vector e) -> Vector e
cartProduct = getCartProduct . foldMap CartProduct
