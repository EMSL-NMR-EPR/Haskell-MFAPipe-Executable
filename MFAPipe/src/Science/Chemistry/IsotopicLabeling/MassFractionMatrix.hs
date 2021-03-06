{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.MassFractionMatrix
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of mass fraction
-- matrices.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.MassFractionMatrix
( -- * MassFractionMatrix type
  MassFractionMatrix(..)
) where

import qualified Control.Lens
import           Data.Monoid.Conv2 (Conv2(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import           Numeric.LinearAlgebra.HMatrix.Lens (AsMatrix(..))

-- | A mass fraction matrix.
newtype MassFractionMatrix e = MassFractionMatrix { getMassFractionMatrix :: Conv2 e }
  deriving (Read, Show)

deriving instance (Container Vector e, Num e) => Eq (MassFractionMatrix e)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (MassFractionMatrix e) where
  mempty = MassFractionMatrix mempty
  {-# INLINE mempty #-}
  (MassFractionMatrix mL) `mappend` (MassFractionMatrix mR) = MassFractionMatrix (mL `mappend` mR)
  {-# INLINE mappend #-}

instance AsMatrix MassFractionMatrix where
  _Matrix = Control.Lens.iso getMassFractionMatrix MassFractionMatrix . _Matrix
