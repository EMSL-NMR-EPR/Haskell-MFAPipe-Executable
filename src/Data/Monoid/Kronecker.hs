{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Kronecker
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Kronecker product.
--
-----------------------------------------------------------------------------

module Data.Monoid.Kronecker where

import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix

-- | Monoid under Kronecker product.
newtype Kronecker e = Kronecker { getKronecker :: Matrix e }
  deriving (Read, Show)

deriving instance (Container Vector e, Num e) => Eq (Kronecker e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (Kronecker e) where
  mempty = Kronecker (Numeric.LinearAlgebra.HMatrix.konst 1 (1, 1))
  {-# INLINE mempty #-}
  (Kronecker mL) `mappend` (Kronecker mR) = Kronecker (mL `Numeric.LinearAlgebra.HMatrix.kronecker` mR)
  {-# INLINE mappend #-}

kronecker :: (Foldable f, Container Vector e, Num (Vector e), Product e) => f (Matrix e) -> Matrix e
kronecker = getKronecker . foldMap Kronecker
