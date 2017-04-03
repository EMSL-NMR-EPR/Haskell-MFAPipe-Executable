{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Graph.Lens
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for labeled, inductive graphs.
-----------------------------------------------------------------------------

module Data.Graph.Inductive.Graph.Lens
( -- * AsEdge class
  AsEdge(..)
, _Graph
  -- * Optimization
, mconcatEdgeLabels
) where

import           Control.Applicative (liftA2, liftA3)
import           Control.Lens (Iso')
import qualified Control.Lens
import qualified Control.Monad.State.Class
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as Control.Monad.Trans.State
import           Data.Graph.Inductive.Graph (Graph(), LEdge, LNode, Node)
import qualified Data.Graph.Inductive.Graph
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Tuple
import qualified GHC.Exts

-- | The 'AsEdge' class is used for types that can be converted to and from labeled edges.
class (Ord (NodeLabel a), Ord (EdgeLabel a)) => AsEdge a where
  {-# MINIMAL _Edge #-}

  -- | Type of node labels.
  type NodeLabel a :: *

  -- | Type of edge labels.
  type EdgeLabel a :: *

  -- | An isomorphism between a scalar and a labeled edge.
  _Edge :: Iso' a (NodeLabel a, NodeLabel a, EdgeLabel a)

instance (Ord a, Ord b) => AsEdge (a, a, b) where
  type NodeLabel (a, a, b) = a
  type EdgeLabel (a, a, b) = b
  _Edge = id
  {-# INLINE _Edge #-}

-- | An isomorphism between a list and a labeled, inductive graph.
_Graph :: (AsEdge a, Graph gr) => Iso' [a] (gr (NodeLabel a) (EdgeLabel a))
_Graph = Control.Lens.mapping _Edge . Control.Lens.iso toGraph fromGraph
  where
    -- | @toGraph xs@ converts @xs@ to an inductive graph.
    toGraph xs =
      let
        -- Initialize an empty node map.
        nodeMap0 = Data.Map.Strict.empty
        -- Map each element of the input to a labeled edge from left to right, and collect the results, along with the modified node map.
        (edges, nodeMap) = Control.Monad.Trans.State.runState (mapM toLEdgeM xs) nodeMap0
        -- Construct a list of labeled nodes.
        nodes = map Data.Tuple.swap (Data.Map.Strict.toAscList nodeMap)
      in
        -- Construct an inductive graph.
        Data.Graph.Inductive.Graph.mkGraph nodes edges
    -- | @fromGraph gr@ converts @gr@ to a list.
    fromGraph = liftA2 Data.Maybe.mapMaybe fromLEdgeMaybe Data.Graph.Inductive.Graph.labEdges

-- | Convert an edge-like thing into a 'LEdge'.
toLEdgeM :: (Monad m, Ord a) => (a, a, b) -> StateT (Map a Node) m (LEdge b)
toLEdgeM (aL, aR, b) = liftA2 (\(nodeL, _) (nodeR, _) -> (nodeL, nodeR, b)) (toLNodeM aL) (toLNodeM aR)

-- | Convert a node-like thing into a 'LNode'.
toLNodeM :: (Monad m, Ord a) => a -> StateT (Map a Node) m (LNode a)
toLNodeM = (fmap . flip (,)) <*> (Control.Monad.State.Class.state . liftA2 (flip (liftA3 maybe) (flip (,))) (\k m -> let x = Data.Map.Strict.size m in (x, Data.Map.Strict.insert k x m)) Data.Map.Strict.lookup)

-- | Convert a 'LEdge' into an edge-like thing.
fromLEdgeMaybe :: (Graph gr) => gr a b -> LEdge b -> Maybe (a, a, b)
fromLEdgeMaybe gr (nodeL, nodeR, b) = liftA2 (\aL aR -> (aL, aR, b)) (Data.Graph.Inductive.Graph.lab gr nodeL) (Data.Graph.Inductive.Graph.lab gr nodeR)

-- | Assume that each edge label is a tuple of a 'Monoid' and a /true/ edge label, and 'mappend' any duplicates. Subject to list fusion.
mconcatEdgeLabels :: (AsEdge a, EdgeLabel a ~ (m, b), Monoid m, Ord b) => [a] -> [a]
mconcatEdgeLabels = Control.Lens.over (Control.Lens.mapping _Edge) f
  where
    f :: (Monoid m, Ord a, Ord b) => [(a, a, (m, b))] -> [(a, a, (m, b))]
    f xs = GHC.Exts.build (\cons nil -> Data.Map.Strict.foldrWithKey (\(aL, aR, b) m -> cons (aL, aR, (m, b))) nil (foldr (\(aL, aR, (m, b)) -> Data.Map.Strict.alter (Just . mappend m . maybe mempty id) (aL, aR, b)) Data.Map.Strict.empty xs))
