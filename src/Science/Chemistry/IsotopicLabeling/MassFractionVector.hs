{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.MassFractionVector
-- Copyright   :  2016 Pacific Northwest National Laboratory
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
import           Data.Functor.Classes.Monoid1 (Monoid1(..))
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

instance Monoid1 MassFractionVector where
  type Monoid1Ty MassFractionVector e = (Container Vector e, Num (Vector e), Product e)
  mempty1 = mempty
  {-# INLINE mempty1 #-}
  mappend1 = mappend1
  {-# INLINE mappend1 #-}
  mconcat1 = mconcat1
  {-# INLINE mconcat1 #-}

instance AsVector MassFractionVector where
  _Vector = Control.Lens.iso getMassFractionVector MassFractionVector . _Vector
