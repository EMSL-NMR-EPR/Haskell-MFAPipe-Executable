{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.EMUReactionNetwork
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of Elementary Metabolite
-- Units (EMU) reaction networks.
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.EMUReactionNetwork
( -- * EMUReactionNetwork type
  EMUReactionNetwork(..)
  -- * Conversion
  -- ** Chemical reaction networks
, toEMUReactionNetwork
  -- ** Stoichiometric models
, toEMUStoichiometricModel
) where

import           Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.Graph.Inductive.Graph.Lens
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.Text.Lazy
import qualified Data.List
import qualified Data.Map.Strict
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.EMUReaction
import           Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.Reaction
import           Science.Chemistry.ReactionNetwork
import           Science.Chemistry.Stoichiometry
import           Science.Chemistry.Types
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), Doc, (<>), (<+>), (<$$>))
import qualified Text.PrettyPrint.Leijen.Text

-- | An Elementary Metabolite Units (EMU) reaction network, where:
--
-- * @i@ is the type of chemical reaction indices; and,
--
-- * @k@ is the type of chemical species.
--
newtype EMUReactionNetwork i k = EMUReactionNetwork { getEMUReactionNetwork :: IntMap [(i, EMUReaction k)] }
  deriving (Eq, Ord, Read, Show)

instance Bifunctor EMUReactionNetwork where
  bimap f g = EMUReactionNetwork . h . getEMUReactionNetwork
    where
      h = Data.IntMap.Strict.map (map (bimap f (fmap g)))
      {-# INLINE h #-}
  {-# INLINE bimap #-}

instance Functor (EMUReactionNetwork i) where
  fmap = second
  {-# INLINE fmap #-}

instance Monoid (EMUReactionNetwork i k) where
  mempty = EMUReactionNetwork Data.IntMap.Strict.empty
  {-# INLINE mempty #-}
  (EMUReactionNetwork mL) `mappend` (EMUReactionNetwork mR) = EMUReactionNetwork (Data.IntMap.Strict.unionWith mappend mL mR)
  {-# INLINE mappend #-}

instance (Pretty i, Pretty k) => Pretty (EMUReactionNetwork i k) where
  pretty = Text.PrettyPrint.Leijen.Text.vsep . Data.List.intersperse Text.PrettyPrint.Leijen.Text.softbreak . map (uncurry (\n xs -> (Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "EMU size") <+> Text.PrettyPrint.Leijen.Text.int n <> Text.PrettyPrint.Leijen.Text.colon) <$$> goList xs)) . Data.IntMap.Strict.toAscList . getEMUReactionNetwork
    where
      goList :: (Pretty i, Pretty k) => [(i, EMUReaction k)] -> Doc
      goList = Text.PrettyPrint.Leijen.Text.vsep . map (uncurry go)
      go :: (Pretty i, Pretty k) => i -> EMUReaction k -> Doc
      go ix x = pretty ix <+> Text.PrettyPrint.Leijen.Text.equals <+> pretty x

-- | /O(?)/.
-- @toEMUReactionNetwork base s@ is the Elementary Metabolite Units (EMU) reaction
-- network for @s@ with respect to @base@.
--
-- Note: This function does not guarantee that the stoichiometry of the result
-- is balanced.
toEMUReactionNetwork :: (Ord i, Ord k, Eq a) => Int -> ReactionNetwork i k a -> EMUReactionNetwork i k
toEMUReactionNetwork base = mkEMUReactionNetwork . concatMap (Data.Graph.Inductive.Graph.Lens.mconcatEdgeLabels . uncurry (\ix -> concatMap (concatMap (map ((,) ix))) . fromReaction)) . Data.Map.Strict.toAscList . getReactionNetwork
  where
    fromReaction :: (Ord k, Eq a) => Reaction k a -> [[[EMUReaction k]]]
    fromReaction = map (fromAtomTransitionReaction . toAtomTransitionReaction) . toNormReactionList
      where
        fromAtomTransitionReaction :: (Ord k) => AtomTransitionReaction k a -> [[EMUReaction k]]
        fromAtomTransitionReaction (AtomTransitionReaction p xs) = map (map (uncurry (EMUReaction p) . bimap mkEMU mkEMU . unzip) . tail . Data.List.subsequences . uncurry (\kR -> map (\((xR, _), ((kL, (xL, _)))) -> ((kL, xL), (kR, xR))))) xs
          where
            mkEMU :: (Ord k) => [((k, StoichiometricIx), AtomIx)] -> EMU k
            mkEMU = EMU base . concatMap strength . Data.Map.Strict.toAscList . Data.Map.Strict.map (Data.Map.Strict.elems . Data.Map.Strict.map AtomIxSet) . foldr (\((k, v), x) -> Data.Map.Strict.alter (Just . Data.Map.Strict.alter (Just . Data.IntSet.insert x . maybe Data.IntSet.empty id) v . maybe Data.Map.Strict.empty id) k) Data.Map.Strict.empty
    mkEMUReactionNetwork :: (Foldable f, Ord k) => f (i, EMUReaction k) -> EMUReactionNetwork i k
    mkEMUReactionNetwork = EMUReactionNetwork . partitionBySizeWith snd

toEMUStoichiometricModel :: (Ord i, HasStoichiometry (EMU k) (EMUReaction k), MutableStoichiometricModel i k (Sparse i k)) => EMUReactionNetwork i k -> IntMap (Dense i (EMU k) Double)
toEMUStoichiometricModel = Data.IntMap.Strict.map (sparseToDense . flip mapAccumStoichiometricModel mempty) . getEMUReactionNetwork

-- | Strengthen a 'Functor'.
--
-- See also
--
-- * \"Deriving Strength from Laziness\"
--   by E. Kmett,
--   /The Comonad Reader/ (2008), online at
--   <http://comonad.com/reader/2008/deriving-strength-from-laziness/>.
strength :: (Functor f) => (a, f b) -> f (a, b)
strength (a, fb) = fmap ((,) a) fb
