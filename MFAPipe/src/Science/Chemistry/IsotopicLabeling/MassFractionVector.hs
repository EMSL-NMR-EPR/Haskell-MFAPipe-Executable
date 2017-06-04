{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.MassFractionVector
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of mass fraction
-- vectors.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.MassFractionVector
( -- * MassFractionVector type
  MassFractionVector(..)
) where

import qualified Control.Lens
import           Data.Monoid.Conv (Conv(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), Product(), Vector)
import           Numeric.LinearAlgebra.HMatrix.Lens (AsVector(..))

-- | A mass fraction vector.
newtype MassFractionVector e = MassFractionVector { getMassFractionVector :: Conv e }
  deriving (Eq, Ord, Read, Show)

instance (Container Vector e, Num (Vector e), Product e) => Monoid (MassFractionVector e) where
  mempty = MassFractionVector mempty
  {-# INLINE mempty #-}
  (MassFractionVector mL) `mappend` (MassFractionVector mR) = MassFractionVector (mL `mappend` mR)
  {-# INLINE mappend #-}

instance AsVector MassFractionVector where
  _Vector = Control.Lens.iso getMassFractionVector MassFractionVector . _Vector
