-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.MassFraction
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of mass fractions.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.MassFraction
( -- * MassFraction type
  MassFraction(..) , getMassFraction
  -- * Utilities
, massFractions
) where

import           Data.Csv (ToField(toField))
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<>))
import qualified Text.PrettyPrint.Leijen.Text

-- | A mass fraction (identifier).
data MassFraction = MassFraction {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Read, Show)

getMassFraction :: MassFraction -> Int
getMassFraction (MassFraction _base x) = x
{-# INLINE getMassFraction #-}

instance Pretty MassFraction where
  pretty x = Text.PrettyPrint.Leijen.Text.char 'M' <> Text.PrettyPrint.Leijen.Text.char '+' <> Text.PrettyPrint.Leijen.Text.int (getMassFraction x)

instance ToField MassFraction where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | @massFractions base n@ is the list of 'MassFraction's of @n@ atoms with
-- respect to @base@.
massFractions
  :: Int
  -- ^ base
  -> Int
  -- ^ number of atoms
  -> [MassFraction]
massFractions base n = map (MassFraction base) $ enumFromThenTo 0 1 ((base - 1) * n)
