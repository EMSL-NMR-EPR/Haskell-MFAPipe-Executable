{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.EMU
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of Elementary Metabolite
-- Units (EMU).
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.EMU
( -- * EMU type
  EMU(..)
) where

import           Data.Bifunctor (first)
import           Data.Csv (ToField(toField))
import           Data.Function (on)
import qualified Data.IntSet
import qualified Data.List
import           Data.Proxy (Proxy(..))
import           Science.Chemistry.EMU.Factorized.Class
import           Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.IsotopicLabeling.FractionMatrix
import           Science.Chemistry.IsotopicLabeling.FractionType
import           Science.Chemistry.IsotopicLabeling.FractionVector
import           Science.Chemistry.IsotopicLabeling.Isotopomer
import           Science.Chemistry.IsotopicLabeling.MassFraction
import           Science.Chemistry.Types
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<>))
import qualified Text.PrettyPrint.Leijen.Text

-- | An Elementary Metabolite Unit (EMU), where:
--
-- * @k@ is the type of chemical species.
--
-- An EMU is a list of pairs, where each pair is a chemical species and a list
-- of sets of atom indices.  This representation enables use cases where the
-- stoichiometric coefficient of a given chemical species is @'/=' 1@.
--
-- For example, assuming two isotopic labeling states per atom, the
-- representation of the EMU @(A123 x B1 x B1)@ is:
--
-- > EMU 2 [('A', AtomIxSet (fromList [1,2,3])), ('B', AtomIxSet (fromList [1])), ('B', AtomIxSet (fromList [1]))]
--
data EMU k = EMU {-# UNPACK #-} !Int [(k, AtomIxSet)]
  deriving (Eq, Ord, Read, Show)

instance Functor EMU where
  fmap f (EMU base m) = EMU base (g m)
    where
      g = map (first f)
      {-# INLINE g #-}
  {-# INLINE fmap #-}

instance Monoid (EMU k) where
  mempty = EMU 1 mempty
  {-# INLINE mempty #-}
  (EMU baseL mL) `mappend` (EMU baseR mR) = EMU (baseL `max` baseR) (mL `mappend` mR)
  {-# INLINE mappend #-}

instance Factorized (EMU k) where
  factors (EMU base m) = do
    (k, xs) <- m
    return (EMU base [(k, xs)])

instance HasSize (EMU k) where
  size (EMU _ m) = sum (map (Data.IntSet.size . getAtomIxSet . snd) m)

instance (Pretty k) => Pretty (EMU k) where
  pretty (EMU _ m) =
    case Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char 'x') (map (uncurry (\k ixs -> pretty k <> Text.PrettyPrint.Leijen.Text.char '#' <> pretty ixs)) m) of
      [] -> Text.PrettyPrint.Leijen.Text.int 1
      [doc] -> doc
      docs -> Text.PrettyPrint.Leijen.Text.parens (Text.PrettyPrint.Leijen.Text.hsep docs)

instance (Pretty k) => ToField (EMU k) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

instance FromFractionMatrixPtr 'IsotopomerFractionTy (EMU k) where
  fromFractionMatrixPtr (FractionMatrixPtr Proxy k@(EMU base _m) (ixL, ixR)) = ((,) `on` (!!) (isotopomers base (size k))) ixL ixR

instance FromFractionMatrixPtr 'MassFractionTy (EMU k) where
  fromFractionMatrixPtr (FractionMatrixPtr Proxy (EMU base _m) (ixL, ixR)) = ((,) `on` MassFraction base) ixL ixR

instance FromFractionVectorPtr 'IsotopomerFractionTy (EMU k) where
  fromFractionVectorPtr (FractionVectorPtr Proxy k@(EMU base _m) ix) = isotopomers base (size k) !! ix

instance FromFractionVectorPtr 'MassFractionTy (EMU k) where
  fromFractionVectorPtr (FractionVectorPtr Proxy (EMU base _m) ix) = MassFraction base ix
