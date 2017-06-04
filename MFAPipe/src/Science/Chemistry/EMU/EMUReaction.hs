{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.EMUReaction
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of Elementary Metabolite
-- Units (EMU) reactions.
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.EMUReaction
( -- * EMUReaction type
  EMUReaction(..)
) where

import qualified Control.Lens
import           Data.Csv (ToField(toField))
import           Data.Graph.Inductive.Graph.Lens (AsEdge(..))
import           Data.Monoid (Product(..), Sum(..))
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.Stoichiometry
import           Science.Chemistry.Types
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<>), (<+>))
import qualified Text.PrettyPrint.Leijen.Text
import qualified Text.Printf

-- | An Elementary Metabolite Units (EMU) reaction, where:
--
-- * @k@ is the type of chemical species.
--
data EMUReaction k = EMUReaction MixingProbability (EMU k) (EMU k)
  deriving (Eq, Ord, Read, Show)

instance Functor EMUReaction where
  fmap f (EMUReaction p ls rs) = EMUReaction p (fmap f ls) (fmap f rs)
  {-# INLINE fmap #-}

instance Monoid (EMUReaction k) where
  mempty = EMUReaction mempty mempty mempty
  {-# INLINE mempty #-}
  (EMUReaction pL lsL rsL) `mappend` (EMUReaction pR lsR rsR) = EMUReaction (pL `mappend` pR) (lsL `mappend` lsR) (rsL `mappend` rsR)
  {-# INLINE mappend #-}

instance (Ord i, Ord k) => AsEdge (i, EMUReaction k) where
  type NodeLabel (i, EMUReaction k) = EMU k
  type EdgeLabel (i, EMUReaction k) = (Sum MixingProbability, i)
  _Edge = Control.Lens.iso to from
    where
      to (ix, EMUReaction p ls rs) = (rs, ls, (Sum p, ix))
      {-# INLINE to #-}
      from (rs, ls, (Sum p, ix)) = (ix, EMUReaction p ls rs)
      {-# INLINE from #-}

instance HasSize (EMUReaction k) where
  size (EMUReaction _ ls rs)
    | sizeL == sizeR = sizeL
    | otherwise = error (Text.Printf.printf "'EMUReaction' is not size balanced (%d /= %d)" sizeL sizeR)
    where
      sizeL, sizeR :: Int
      sizeL = size ls
      sizeR = size rs

instance (Ord k) => HasStoichiometry (EMU (MetaboliteVar k)) (EMUReaction (MetaboliteVar k)) where
  assertStoichiometryM assertStoichiometricCoefficient0 assertChemicalSpecies0 = fromEMUReaction
    where
      fromEMUReaction (EMUReaction p ls rs) = do
        -- Coerce mixing probability.
        let new_p = StoichiometricCoefficient (Sum (getProduct (getMixingProbability p)))
        -- Reagents have negative stoichiometric coefficients.
        fromEMU ls (negate new_p)
        -- Products have positive stoichiometric coefficients.
        fromEMU rs new_p
      fromEMU k x = do
        -- Assert stoichiometric coefficient.
        assertStoichiometricCoefficient0 k x
        -- Assert chemical species.
        assert k
      -- The 'assert' function asserts a chemical species.
      assert x0@(EMU _ kxss)
        -- If every EMU is 'Intracellular', then assert an intermediate.
        | all (isIntracellular . fst) kxss = assertChemicalSpecies0 (Left x0)
        -- If every EMU is 'Extracellular', then assert a reagent/product.
        | all (isExtracellular . fst) kxss = assertChemicalSpecies0 (Right x0)
        -- Otherwise, raise an error.
        | otherwise = error "'EMU' is mixture of 'Intracellular' and 'Extracellular' 'MetaboliteVar's"

instance (Pretty k) => Pretty (EMUReaction k) where
  pretty (EMUReaction p ls rs) = pretty ls <+> Text.PrettyPrint.Leijen.Text.char '-' <> pretty p <> Text.PrettyPrint.Leijen.Text.char '-' <> Text.PrettyPrint.Leijen.Text.char '>' <+> pretty rs

instance (Pretty k) => ToField (EMUReaction k) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty
