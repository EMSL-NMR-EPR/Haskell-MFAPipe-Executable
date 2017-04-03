{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.IsotopomerFractionVector
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of isotopomer fraction
-- vectors.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.IsotopomerFractionVector
( -- * IsotopomerFractionVector type
  IsotopomerFractionVector(..)
) where

import qualified Control.Lens
import           Data.Functor.Classes.Monoid1 (Monoid1(..))
import           Data.Monoid.CartProduct (CartProduct(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import           Numeric.LinearAlgebra.HMatrix.Lens (AsVector(..))

-- | A isotopomer fraction vector.
newtype IsotopomerFractionVector e = IsotopomerFractionVector { getIsotopomerFractionVector :: CartProduct e }
  deriving (Eq, Ord, Read, Show)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (IsotopomerFractionVector e) where
  mempty = IsotopomerFractionVector mempty
  {-# INLINE mempty #-}
  (IsotopomerFractionVector mL) `mappend` (IsotopomerFractionVector mR) = IsotopomerFractionVector (mL `mappend` mR)
  {-# INLINE mappend #-}

instance Monoid1 IsotopomerFractionVector where
  type Monoid1Ty IsotopomerFractionVector e = (Container Vector e, Num (Vector e), Product e)
  mempty1 = mempty
  {-# INLINE mempty1 #-}
  mappend1 = mappend1
  {-# INLINE mappend1 #-}
  mconcat1 = mconcat1
  {-# INLINE mconcat1 #-}

instance AsVector IsotopomerFractionVector where
  _Vector = Control.Lens.iso getIsotopomerFractionVector IsotopomerFractionVector . _Vector
