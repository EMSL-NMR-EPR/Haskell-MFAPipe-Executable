{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.EMUGraph
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of Elementary Metabolite
-- Units (EMU) graphs.
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.EMUGraph
( -- * EMUGraph type
  EMUGraph(..)
  -- * Lenses
, _EMUGraph
) where

import           Control.Lens (Iso')
import qualified Control.Lens
import           Data.Bifunctor (Bifunctor(bimap, second))
import           Data.Graph.Inductive.Graph (Graph(), OrdGr)
import qualified Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph.Extras.Node
import           Data.Graph.Inductive.Graph.Instances ()
import           Data.Graph.Inductive.Graph.Lens (AsEdge(..), _Graph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.DFS.EdgeSmoothing (Smoothable())
import qualified Data.Graph.Inductive.Query.DFS.EdgeSmoothing
import qualified Data.Graph.Inductive.Query.DFS.Subgraph
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import           Data.Monoid (Sum(..))
import           Data.Set (Set)
import qualified Data.Set
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.EMUReaction
import           Science.Chemistry.EMU.EMUReactionNetwork
import           Science.Chemistry.EMU.Factorized.Class
import           Science.Chemistry.EMU.HasSize.Class
import           Science.Chemistry.EMU.Optimized.Class

-- | An Elementary Metabolite Units (EMU) graph, where:
--
-- * @i@ is the type of chemical reaction indices; and,
--
-- * @k@ is the type of chemical species.
--
newtype EMUGraph i k = EMUGraph { getEMUGraph :: IntMap (OrdGr Gr (NodeLabel (i, EMUReaction k)) (EdgeLabel (i, EMUReaction k))) }
  deriving (Eq, Ord, Read, Show)

instance Bifunctor EMUGraph where
  bimap f g = EMUGraph . h . getEMUGraph
    where
      h = Data.IntMap.Strict.map (Data.Graph.Inductive.Graph.nemap (fmap g) (second f))
      {-# INLINE h #-}
  {-# INLINE bimap #-}

instance Functor (EMUGraph i) where
  fmap = second
  {-# INLINE fmap #-}

instance (Smoothable i, Eq i, Ord k) => Optimized (EMU k) (EMUGraph i k) where
  optimize = Control.Lens.under (Control.Lens.iso EMUGraph getEMUGraph) . (snd .) . reachableSubgraphs
    where
      -- reachableSubgraphs :: (Foldable f, DynGraph gr, Factorized a, Ord a, Size a, Eq b) => f a -> IntMap (gr a b) -> (IntMap (Set a), IntMap (gr a b))
      reachableSubgraphs ks0 = Data.IntMap.Strict.foldrWithKey cataWithKey (mkAccWith Data.Set.insert ks0 Data.IntMap.Strict.empty, Data.IntMap.Strict.empty)
        where
          -- cataWithKey :: (DynGraph gr, Factorized a, Ord a, Size a, Eq b) => Int -> gr a b -> (IntMap (Set a), IntMap (gr a b)) -> (IntMap (Set a), IntMap (gr a b))
          cataWithKey n gr0 ~acc@(ks_acc, grs_acc) = case Data.IntMap.Strict.lookup n ks_acc of
            Nothing -> acc
            Just ks ->
              let
                gr1 = Data.Graph.Inductive.Query.DFS.Subgraph.subgraph ks gr0
                gr2 = Data.Graph.Inductive.Query.DFS.EdgeSmoothing.smoothEdgesWhen (\(Sum mL, xL) (Sum mR, xR) -> (,) (Sum (mL `mappend` mR)) <$> Data.Graph.Inductive.Query.DFS.EdgeSmoothing.smoothMaybe xL xR) (`Data.Set.notMember` ks) gr1
              in
                (mkAccWith Data.Set.insert (leafNodeLabsFor gr2) (mkAccWith Data.Set.delete ks ks_acc), Data.IntMap.Strict.insert n gr2 grs_acc)
          leafNodeLabsFor :: (Graph gr, Factorized a) => gr a b -> [a]
          leafNodeLabsFor gr = concatMap (factors . snd) (filter (Data.Graph.Inductive.Graph.Extras.Node.isSink gr . fst) (Data.Graph.Inductive.Graph.labNodes gr))
          mkAccWith :: (Foldable f, Ord a, HasSize a) => (a -> Set a -> Set a) -> f a -> IntMap (Set a) -> IntMap (Set a)
          mkAccWith f0 = flip (foldr (cataWith f0 <*> size))
            where
              cataWith :: (a -> Set a -> Set a) -> a -> Int -> IntMap (Set a) -> IntMap (Set a)
              cataWith f k = Data.IntMap.Strict.alter ((\ks -> if Data.Set.null ks then Nothing else Just ks) . f k . maybe Data.Set.empty id)

-- | An 'Iso'' between Elementary Metabolite Units (EMU) reaction networks and
-- graphs.
_EMUGraph :: (Ord i, Ord k) => Iso' (EMUReactionNetwork i k) (EMUGraph i k)
_EMUGraph = Control.Lens.iso getEMUReactionNetwork EMUReactionNetwork . Control.Lens.mapping _Graph . Control.Lens.iso EMUGraph getEMUGraph
