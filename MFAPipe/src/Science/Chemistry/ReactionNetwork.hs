{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.ReactionNetwork
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of chemical reaction
-- networks, along with functions for conversion to stoichiometric models.
--
-----------------------------------------------------------------------------

module Science.Chemistry.ReactionNetwork
( -- * ReactionNetwork type
  ReactionNetwork(..)
  -- * Traversal
  -- ** Map
, ixmapReactionNetwork , ixmapReactionNetworkWith
  -- * Conversion
  -- ** Stoichiometric models
, toStoichiometricModel
) where

import           Data.Bifunctor (Bifunctor(bimap, second))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Numeric.LinearAlgebra.HMatrix (Container(), Vector)
import           Science.Chemistry.Reaction
import           Science.Chemistry.Stoichiometry
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<+>))
import qualified Text.PrettyPrint.Leijen.Text

-- | A chemical reaction network, where:
--
-- * @i@ is the type of chemical reaction indices;
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
newtype ReactionNetwork i k a = ReactionNetwork { getReactionNetwork :: Map i (Reaction k a) }
  deriving (Eq, Ord, Read, Show)

-- | /O(n*log n)/.
-- @ixmapReactionNetwork f s@ is the chemical reaction network obtained by applying @f@ to each index in @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct indices to the same new index.  In this case the value at the greatest of the original indices is retained.
ixmapReactionNetwork :: (Ord i, Ord i') => (i -> i') -> ReactionNetwork i k a -> ReactionNetwork i' k a
ixmapReactionNetwork f = ReactionNetwork . Data.Map.Strict.mapKeys f . getReactionNetwork

-- | /O(n*log n)/.
-- @ixmapReactionNetworkWith c f s@ is the chemical reaction network obtained by applying @f@ to each index in @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct indices to the same new index.  In this case the associated chemical reactions will be combined using @c@.
ixmapReactionNetworkWith :: (Ord i, Ord i') => (Reaction k a -> Reaction k a -> Reaction k a) -> (i -> i') -> ReactionNetwork i k a -> ReactionNetwork i' k a
ixmapReactionNetworkWith c f = ReactionNetwork . Data.Map.Strict.mapKeysWith c f . getReactionNetwork

instance Bifunctor (ReactionNetwork i) where
  bimap f g = ReactionNetwork . Data.Map.Strict.map (bimap f g) . getReactionNetwork
  {-# INLINE bimap #-}

instance Functor (ReactionNetwork i k) where
  fmap = second
  {-# INLINE fmap #-}

instance (Ord i) => Monoid (ReactionNetwork i k a) where
  mempty = ReactionNetwork mempty
  {-# INLINE mempty #-}
  (ReactionNetwork mL) `mappend` (ReactionNetwork mR) = ReactionNetwork (mL `mappend` mR)
  {-# INLINE mappend #-}

instance (Pretty i, Pretty k, Pretty a) => Pretty (ReactionNetwork i k a) where
  pretty = Text.PrettyPrint.Leijen.Text.vsep . map (uncurry (\ix x -> pretty ix <+> Text.PrettyPrint.Leijen.Text.equals <+> pretty x)) . Data.Map.Strict.toAscList . getReactionNetwork

-- | /O(?)/.
-- @toStoichiometricModel s@ is the 'Dense' stoichiometric model obtained by asserting the stoichiometry of each chemical reaction in @s@.
--
-- Note: This function does not guarantee that the stoichiometry of the result is balanced.
toStoichiometricModel :: (Ord i, HasStoichiometry k (Reaction k a), MutableStoichiometricModel i k (Sparse i k), Container Vector e, Fractional e) => ReactionNetwork i k a -> Dense i k e
toStoichiometricModel = sparseToDense . flip mapAccumStoichiometricModel mempty . Data.Map.Strict.toAscList . getReactionNetwork
