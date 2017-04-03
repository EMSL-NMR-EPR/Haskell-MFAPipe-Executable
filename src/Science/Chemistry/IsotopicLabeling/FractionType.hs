-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.FractionType
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of fraction-specific types.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.FractionType
( -- * FractionType type
  FractionType(..)
) where

-- | The type of fraction types.
data FractionType
  = IsotopomerFractionTy
  | MassFractionTy
  | ProductTy
  | SumTy
  deriving (Eq, Ord, Read, Show)
