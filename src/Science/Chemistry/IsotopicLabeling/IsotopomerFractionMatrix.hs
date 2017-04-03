{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.IsotopomerFractionMatrix
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of isotopomer fraction
-- matrices.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.IsotopomerFractionMatrix
( -- * IsotopomerFractionMatrix type
  IsotopomerFractionMatrix(..)
) where

import qualified Control.Lens
import           Data.Functor.Classes.Monoid1 (Monoid1(..))
import           Data.Monoid.Kronecker (Kronecker(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import           Numeric.LinearAlgebra.HMatrix.Lens (AsMatrix(..))

-- | A isotopomer fraction matrix.
newtype IsotopomerFractionMatrix e = IsotopomerFractionMatrix { getIsotopomerFractionMatrix :: Kronecker e }
  deriving (Read, Show)

deriving instance (Container Vector e, Num e) => Eq (IsotopomerFractionMatrix e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (IsotopomerFractionMatrix e) where
  mempty = IsotopomerFractionMatrix mempty
  {-# INLINE mempty #-}
  (IsotopomerFractionMatrix mL) `mappend` (IsotopomerFractionMatrix mR) = IsotopomerFractionMatrix (mL `mappend` mR)
  {-# INLINE mappend #-}

instance Monoid1 IsotopomerFractionMatrix where
  type Monoid1Ty IsotopomerFractionMatrix e = (Container Vector e, Num (Vector e), Product e)
  mempty1 = mempty
  {-# INLINE mempty1 #-}
  mappend1 = mappend1
  {-# INLINE mappend1 #-}
  mconcat1 = mconcat1
  {-# INLINE mconcat1 #-}

instance AsMatrix IsotopomerFractionMatrix where
  _Matrix = Control.Lens.iso getIsotopomerFractionMatrix IsotopomerFractionMatrix . _Matrix
