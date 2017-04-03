{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.Reaction
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of chemical reactions
-- and atom transition reactions, along with functions for chemical reaction
-- normalization and construction of atom transition reactions.
-----------------------------------------------------------------------------

module Science.Chemistry.Reaction
( -- * Chemical Reactions
  Reaction(..)
, MFAReactionPart , MFAReactionSide
, FBAReactionPart , FBAReactionSide
, NormReaction(..)
, NormMFAReactionPart , NormMFAReactionSide
  -- * Atom Transition Reactions
, AtomTransitionReaction(..)
, AtomTransitionReactionPart , AtomTransitionReactionSide
  -- * Working with chemical reactions
  -- ** Normalization
, toNormReactionList
  -- ** Atom transitions
, toAtomTransitionReaction
) where

import           Control.Applicative (liftA2)
import           Control.Monad (foldM, guard)
import           Data.Bifunctor (Bifunctor(bimap, first, second))
import           Data.Csv (ToField(toField))
import           Data.Function (on)
import qualified Data.List
import qualified Data.Map.Strict
import           Data.Monoid (Product(..), Sum(..))
import qualified Data.Text.Lazy
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.Stoichiometry
import           Science.Chemistry.Types
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), Doc, (<>), (<+>))
import qualified Text.PrettyPrint.Leijen.Text

-- | A chemical reaction, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
data Reaction k a
  = MFAReaction (MFAReactionSide k a) (MFAReactionSide k a)
  | FBAReaction (FBAReactionSide k a) (FBAReactionSide k a)
  deriving (Eq, Ord, Read, Show)

type MFAReactionPart k a = (k, [(MixingProbability, AtomKeyList a)])

type MFAReactionSide k a = [MFAReactionPart k a]

type FBAReactionPart k a = (k, StoichiometricCoefficient)

type FBAReactionSide k a = [FBAReactionPart k a]

instance Bifunctor Reaction where
  bimap f g (MFAReaction ls rs) = MFAReaction (h ls) (h rs)
    where
      h = map (bimap f (map (second (fmap g))))
      {-# INLINE h #-}
  bimap f _ (FBAReaction ls rs) = FBAReaction (h ls) (h rs)
    where
      h = map (first f)
      {-# INLINE h #-}
  {-# INLINE bimap #-}

instance Functor (Reaction k) where
  fmap = second
  {-# INLINE fmap #-}

instance (Ord k) => HasStoichiometry (MetaboliteVar k) (Reaction (MetaboliteVar k) a) where
  assertStoichiometryM assertStoichiometricCoefficient0 assertChemicalSpecies0 = fromReaction
    where
      -- The 'fromReaction' function asserts the stoichiometry of a 'MFAReaction' or 'FBAReaction'.
      fromReaction (MFAReaction ls rs) = do
        -- Reagents have negative stoichiometric coefficients.
        fromMFAReactionWith negate ls
        -- Products have positive stoichiometric coefficients.
        fromMFAReactionWith id rs
      fromReaction (FBAReaction ls rs) = do
        -- Reagents have negative stoichiometric coefficients.
        fromFBAReactionWith negate ls
        -- Products have positive stoichiometric coefficients.
        fromFBAReactionWith id rs
      -- The 'fromMFAReactionWith' function asserts the stoichiometry of a 'MFAReaction', using the specified helper function.
      fromMFAReactionWith = flip foldM () . cataWith
        where
          cataWith f () (k, xs) = do
            -- Assert stoichiometric coefficient (after coercion of 'Product' and 'Sum' newtypes).
            --
            -- An MFA reaction may have multiple instances of a given chemical species; accounting for mixing.
            -- Use the 'foldMap' function to 'Sum' multiple instances.
            if null xs
              then do
                assertStoichiometricCoefficient0 k (f 1)
              else do
                assertStoichiometricCoefficient0 k (foldMap (f . StoichiometricCoefficient . Sum . getProduct . getMixingProbability . fst) xs)
            -- Assert chemical species.
            assert k
      -- The 'fromFBAReactionWith' function asserts the stoichiometry of a 'FBAReaction', using the specified helper function.
      fromFBAReactionWith = flip foldM () . cataWith
        where
          cataWith f () (k, x) = do
            -- Assert stoichiometric coefficient.
            assertStoichiometricCoefficient0 k (f x)
            -- Assert chemical species.
            assert k
      -- The 'assert' function asserts a chemical species.
      --
      -- If the metabolite variable is 'Intracellular', then assert an intermediate.
      --
      -- If the metabolite variable is 'Extracellular', then assert a reagent/product.
      assert k
        | isIntracellular k = assertChemicalSpecies0 (Left k)
        | isExtracellular k = assertChemicalSpecies0 (Right k)
        | otherwise = error "'MetaboliteVar' is neither 'Intracellular' nor 'Extracellular'"

instance (Pretty k, Pretty a) => Pretty (Reaction k a) where
  pretty (MFAReaction ls rs) = prettyMFAReactionSide ls <+> Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "->") <+> prettyMFAReactionSide rs
    where
      prettyMFAReactionSide :: (Pretty k, Pretty a) => MFAReactionSide k a -> Doc
      prettyMFAReactionSide = Text.PrettyPrint.Leijen.Text.hsep . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char '+') . map prettyMFAReactionPart
      prettyMFAReactionPart :: (Pretty k, Pretty a) => MFAReactionPart k a -> Doc
      prettyMFAReactionPart (k, xs) = prettyFBAReactionPart (k, 1) <+> prettyMixingList xs
      prettyMixingList :: (Pretty a) => [(MixingProbability, AtomKeyList a)] -> Doc
      prettyMixingList = Text.PrettyPrint.Leijen.Text.parens . Text.PrettyPrint.Leijen.Text.hsep . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char '+') . map (uncurry (\p xs -> pretty p <+> pretty xs))
  pretty (FBAReaction ls rs) = prettyFBAReactionSide ls <+> Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "->") <+> prettyFBAReactionSide rs

prettyFBAReactionSide :: (Pretty k) => FBAReactionSide k a -> Doc
prettyFBAReactionSide = Text.PrettyPrint.Leijen.Text.hsep . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char '+') . map prettyFBAReactionPart

prettyFBAReactionPart :: (Pretty k) => FBAReactionPart k a -> Doc
prettyFBAReactionPart (k, c) = pretty c <+> pretty k

instance (Pretty k, Pretty a) => ToField (Reaction k a) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | A normalized chemical reaction, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
data NormReaction k a
  = NormMFAReaction MixingProbability (NormMFAReactionSide k a) (NormMFAReactionSide k a)
  | NormFBAReaction (FBAReactionSide k a) (FBAReactionSide k a)
  deriving (Eq, Ord, Read, Show)

type NormMFAReactionPart k a = ((k, StoichiometricIx), AtomKeyList a)

type NormMFAReactionSide k a = [NormMFAReactionPart k a]

instance Bifunctor NormReaction where
  bimap f g (NormMFAReaction p ls rs) = NormMFAReaction p (h ls) (h rs)
    where
      h = map (bimap (first f) (fmap g))
      {-# INLINE h #-}
  bimap f _ (NormFBAReaction ls rs) = NormFBAReaction (h ls) (h rs)
    where
      h = map (first f)
      {-# INLINE h #-}
  {-# INLINE bimap #-}

instance Functor (NormReaction k) where
  fmap = second
  {-# INLINE fmap #-}

instance (Ord k) => HasStoichiometry (MetaboliteVar k) (NormReaction (MetaboliteVar k) a) where
  assertStoichiometryM assertStoichiometricCoefficient0 assertChemicalSpecies0 = fromNormReaction
    where
      -- The 'fromNormReaction' function asserts the stoichiometry of a 'NormMFAReaction' or 'NormFBAReaction'.
      fromNormReaction (NormMFAReaction p ls rs) = do
        -- Construct helper function to assert stoichiometric coefficient (after coercion of 'Product' and 'Sum' newtypes).
        let f = (*) . StoichiometricCoefficient . Sum . getProduct . getMixingProbability $ p
        -- Reagents have negative stoichiometric coefficients.
        fromNormMFAReactionWith (f . negate) ls
        -- Products have positive stoichiometric coefficients.
        fromNormMFAReactionWith f rs
      fromNormReaction (NormFBAReaction ls rs) = do
        -- Reagents have negative stoichiometric coefficients.
        fromNormFBAReactionWith negate ls
        -- Products have positive stoichiometric coefficients.
        fromNormFBAReactionWith id rs
      fromNormMFAReactionWith = flip foldM () . cataWith
        where
          cataWith f () ((k, _), _) = do
            -- Assert stoichiometric coefficient.
            --
            -- By definition, before application of the helper function, the stoichiometric coefficient is unity.
            assertStoichiometricCoefficient0 k (f 1)
            -- Assert chemical species.
            assert k
      fromNormFBAReactionWith = flip foldM () . cataWith
        where
          cataWith f () (k, x) = do
            -- Assert stoichiometric coefficient.
            assertStoichiometricCoefficient0 k (f x)
            -- Assert chemical species.
            assert k

      -- The 'assert' function asserts a chemical species.
      --
      -- If the metabolite variable is 'Intracellular', then assert an intermediate.
      --
      -- If the metabolite variable is 'Extracellular', then assert a reagent/product.
      assert k
        | isIntracellular k = assertChemicalSpecies0 (Left k)
        | isExtracellular k = assertChemicalSpecies0 (Right k)
        | otherwise = error "'MetaboliteVar' is neither 'Intracellular' nor 'Extracellular'"

instance (Pretty k, Pretty a) => Pretty (NormReaction k a) where
  pretty (NormMFAReaction p ls rs) = prettyNormMFAReactionSide ls <+> Text.PrettyPrint.Leijen.Text.char '-' <> pretty p <> Text.PrettyPrint.Leijen.Text.char '-' <> Text.PrettyPrint.Leijen.Text.char '>' <+> prettyNormMFAReactionSide rs
    where
      prettyNormMFAReactionSide :: (Pretty k, Pretty a) => NormMFAReactionSide k a -> Doc
      prettyNormMFAReactionSide = Text.PrettyPrint.Leijen.Text.hsep . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char '+') . map prettyNormMFAReactionPart
      prettyNormMFAReactionPart :: (Pretty k, Pretty a) => NormMFAReactionPart k a -> Doc
      prettyNormMFAReactionPart ((k, _ix), xs) = prettyFBAReactionPart (k, 1) <+> pretty xs
  pretty (NormFBAReaction ls rs) = prettyFBAReactionSide ls <+> Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "->") <+> prettyFBAReactionSide rs

instance (Pretty k, Pretty a) => ToField (NormReaction k a) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | An atom transition reaction, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
data AtomTransitionReaction k a
  = AtomTransitionReaction MixingProbability (AtomTransitionReactionSide k a)
  deriving (Eq, Ord, Read, Show)

type AtomTransitionReactionPart k a = ((k, StoichiometricIx), [((AtomIx, a), ((k, StoichiometricIx), (AtomIx, a)))])

type AtomTransitionReactionSide k a = [AtomTransitionReactionPart k a]

instance Bifunctor AtomTransitionReaction where
  bimap f g (AtomTransitionReaction p xs) = AtomTransitionReaction p (h xs)
    where
      h = map (bimap (first f) (map (bimap (second g) (bimap (first f) (second g)))))
      {-# INLINE h #-}
  {-# INLINE bimap #-}

instance Functor (AtomTransitionReaction k) where
  fmap = second
  {-# INLINE fmap #-}

-- | /O(?)/.
-- @toNormReactionList rxn@ is the list of chemical reactions obtained by normalizing @rxn@.
--
-- If @rxn@ is a 'MFAReaction' then each chemical species is associated with a 'StoichiometricIx' and the 'MixingProbability' is distributed.
--
-- If @rxn@ is a 'FBAReaction' then, simply, 'StoichiometricCoefficient's for duplicate chemical species are summed.
--
-- Note: This function does not guarantee that the stoichiometry of the result is balanced.
toNormReactionList :: (Ord k) => Reaction k a -> [NormReaction k a]
toNormReactionList (MFAReaction ls0 rs0) = mkNormReactionList ls0 rs0
  where
    -- | Associate each chemical species with a stoichiometric index, and distribute the mixing probabilities.
    mkNormReactionList :: (Ord k) => [(k, [(MixingProbability, AtomKeyList a)])] -> [(k, [(MixingProbability, AtomKeyList a)])] -> [NormReaction k a]
    mkNormReactionList = liftA2 (\(pL, ls) (pR, rs) -> NormMFAReaction (pL `mappend` pR) ls rs) `on` normA . assocWithDefault (toEnum 0)
      where
        -- | Distribute mixing probabilities.
        -- normA :: (Ord k, Monoid m) => [(k, [(m, a)])] -> [(m, [(k, a)])]
        normA = map sequenceA . sequenceA . map (uncurry (map . second . (,)) . second (\xs -> if null xs then [(1, AtomKeyList [])] else xs))
        -- | Associate each chemical species with a stoichiometric index.
        assocWithDefault :: (Ord k, Enum v) => v -> [(k, a)] -> [((k, v), a)]
        assocWithDefault v0 = snd . Data.List.mapAccumL (\m (k, x) -> liftA2 (,) (\v -> Data.Map.Strict.insert k v m) (\v -> ((k, v), x)) (succ (Data.Map.Strict.findWithDefault v0 k m))) Data.Map.Strict.empty
toNormReactionList (FBAReaction ls0 rs0) = [mkNormReaction ls0 rs0]
  where
    -- | 'FBAReaction' is already in normal form, so we just have to sum the stoichiometric coefficients for each chemical species.
    mkNormReaction :: (Ord k) => [(k, StoichiometricCoefficient)] -> [(k, StoichiometricCoefficient)] -> NormReaction k a
    mkNormReaction = NormFBAReaction `on` Data.Map.Strict.toAscList . Data.Map.Strict.fromListWith mappend

-- | /O(?)/.
-- @toAtomTransitionReaction rxn@ is the atom transition reaction that corresponds to @rxn@.
--
-- If @rxn@ is a 'NormMFAReaction' then the atoms of each product are equated to the atoms of each reagent, and assigned an 'AtomIx'.
--
-- If @rxn@ is a 'NormFBAReaction' then, by definition, there are no atom transitions.
--
-- Note: This function does not guarantee that the stoichiometry of the result is balanced.
toAtomTransitionReaction :: (Eq a) => NormReaction k a -> AtomTransitionReaction k a
toAtomTransitionReaction (NormMFAReaction p ls0 rs0) = AtomTransitionReaction p (mkAtomTransitionList ls0 rs0)
  where
    -- | For each product, identify the atoms of each reagent.
    mkAtomTransitionList :: (Eq a) => [(k, AtomKeyList a)] -> [(k, AtomKeyList a)] -> [(k, [((AtomIx, a), (k, (AtomIx, a)))])]
    mkAtomTransitionList ls rs = do
      (kR, atomsR) <- rs
      return $ (,) kR $ do
        atomR <- zip (enumFrom (toEnum 1)) (getAtomKeyList atomsR)
        (kL, atomsL) <- ls
        atomL <- zip (enumFrom (toEnum 1)) (getAtomKeyList atomsL)
        guard (snd atomR == snd atomL)
        return (atomR, (kL, atomL))
toAtomTransitionReaction (NormFBAReaction _ _) = AtomTransitionReaction mempty []
