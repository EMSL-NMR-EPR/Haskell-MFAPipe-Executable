{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.Types
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the majority of types that need to appear in user
-- signatures or in documentation when talking about chemical reactions, atom
-- transition reactions, stoichiometric models, etc.  The remaining types are
-- distributed across various modules in the hierarchy.
--
-----------------------------------------------------------------------------

module Science.Chemistry.Types
( AtomIx
, AtomIxSet(..)
, AtomKeyList(..)
, MixingProbability(..)
, StoichiometricCoefficient(..)
, StoichiometricIx
) where

import           Data.Csv (ToField(toField))
import           Data.IntSet (IntSet)
import qualified Data.IntSet
import qualified Data.List
import           Data.Monoid (Product(..), Sum(..))
import qualified Data.Ratio
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text

-- | The index of an atom of an instance of a chemical species in the context of an atom transition reaction.
type AtomIx = Int

-- | A set of indices of atoms of an instance of a chemical species in the context of an atom transitions reaction.
newtype AtomIxSet = AtomIxSet { getAtomIxSet :: IntSet }
  deriving (Eq, Ord, Read, Show)

instance Pretty AtomIxSet where
  pretty = Text.PrettyPrint.Leijen.Text.braces . Text.PrettyPrint.Leijen.Text.hcat . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char ',') . map Text.PrettyPrint.Leijen.Text.int . Data.IntSet.toAscList . getAtomIxSet

instance ToField AtomIxSet where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | A list of keys for atoms of an instance of a chemical species in the context of a chemical reaction.
newtype AtomKeyList a = AtomKeyList { getAtomKeyList :: [a] }
  deriving (Eq, Ord, Read, Show, Monoid)

instance Functor AtomKeyList where
  fmap f = AtomKeyList . map f . getAtomKeyList
  {-# INLINE fmap #-}

instance Foldable AtomKeyList where
  foldMap f = foldMap f . getAtomKeyList
  {-# INLINE foldMap #-}

instance Traversable AtomKeyList where
  traverse f = fmap AtomKeyList . traverse f . getAtomKeyList
  {-# INLINE traverse #-}

instance (Pretty a) => Pretty (AtomKeyList a) where
  pretty = Text.PrettyPrint.Leijen.Text.braces . Text.PrettyPrint.Leijen.Text.hcat . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char ',') . map pretty . getAtomKeyList

instance (Pretty a) => ToField (AtomKeyList a) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | The probability of an atom transition.
newtype MixingProbability = MixingProbability { getMixingProbability :: Product Rational }
  deriving (Eq, Ord, Read, Show, Monoid, Num)

instance Pretty MixingProbability where
  pretty (MixingProbability (Product x))
    | Data.Ratio.denominator x == 1 = Text.PrettyPrint.Leijen.Text.integer (Data.Ratio.numerator x)
    | otherwise = Text.PrettyPrint.Leijen.Text.double (fromRational x)

-- | The coefficient factor for the stoichiometry of an instance of a chemical species in the context of a chemical reaction.
newtype StoichiometricCoefficient = StoichiometricCoefficient { getStoichiometricCoefficient :: Sum Rational }
  deriving (Eq, Ord, Read, Show, Monoid, Num)

instance Pretty StoichiometricCoefficient where
  pretty (StoichiometricCoefficient (Sum x))
    | Data.Ratio.denominator x == 1 = Text.PrettyPrint.Leijen.Text.integer (Data.Ratio.numerator x)
    | otherwise = Text.PrettyPrint.Leijen.Text.double (fromRational x)

-- | The index of an instance of a chemical species in the context of a chemical reaction.
type StoichiometricIx = Int
