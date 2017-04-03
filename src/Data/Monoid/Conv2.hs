{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Conv2
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Two-dimensional convolution.
--
-----------------------------------------------------------------------------

module Data.Monoid.Conv2 where

import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix

-- | Monoid under two-dimensional convolution.
newtype Conv2 e = Conv2 { getConv2 :: Matrix e }
  deriving (Read, Show)

deriving instance (Container Vector e, Num e) => Eq (Conv2 e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (Conv2 e) where
  mempty = Conv2 (Numeric.LinearAlgebra.HMatrix.konst 1 (1, 1))
  {-# INLINE mempty #-}
  (Conv2 mL) `mappend` (Conv2 mR) = Conv2 (mL `Numeric.LinearAlgebra.HMatrix.conv2` mR)
  {-# INLINE mappend #-}

conv2 :: (Foldable f, Container Vector e, Num (Vector e), Product e) => f (Matrix e) -> Matrix e
conv2 = getConv2 . foldMap Conv2
