-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Query.DFS.Subgraph
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports an implementation of the subgraph algorithm.
-----------------------------------------------------------------------------

module Data.Graph.Inductive.Query.DFS.Subgraph
( -- * Operations
  -- ** Subgraphs
  subgraph
) where

import           Control.Monad (ap, join)
import           Data.Graph.Inductive.Graph (DynGraph())
import qualified Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.NodeMap
import qualified Data.Graph.Inductive.Query.DFS
import           Data.Set (Set)
import qualified Data.Set

-- | Returns the subgraph induced by the supplied node labels.
--
-- Note: This function delegates to 'Data.Graph.Inductive.Query.DFS.reachable'.
subgraph :: (DynGraph gr, Ord a) => Set a -> gr a b -> gr a b
subgraph = join . (Data.Graph.Inductive.Graph.subgraph .) . ap (\gr -> Data.Set.toList . Data.Set.unions . map (\node -> Data.Set.fromList (Data.Graph.Inductive.Query.DFS.reachable node gr))) . flip (\gr -> map (fst . Data.Graph.Inductive.NodeMap.mkNode_ (Data.Graph.Inductive.NodeMap.fromGraph gr)) . Data.Set.toAscList)
