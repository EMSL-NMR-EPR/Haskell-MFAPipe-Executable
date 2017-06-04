{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
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

instance AsMatrix IsotopomerFractionMatrix where
  _Matrix = Control.Lens.iso getIsotopomerFractionMatrix IsotopomerFractionMatrix . _Matrix
