-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Graph.Extras.Node
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports assorted functions and types that relate to nodes in
-- inductive graphs.
-----------------------------------------------------------------------------

module Data.Graph.Inductive.Graph.Extras.Node
( -- * Predicates
  isIsolated , isSource , isSink , isIntermediate , isLeaf
  -- * NodeType type
, NodeType(..) , getNodeType
) where

import           Control.Applicative (liftA2)
import           Data.Graph.Inductive.Graph (Graph(), Node)
import qualified Data.Graph.Inductive.Graph

-- | Return 'True' if the given 'Node' has no incoming or outgoing edges, 'False' otherwise.
isIsolated :: (Graph gr) => gr a b -> Node -> Bool
isIsolated gr node = Data.Graph.Inductive.Graph.deg gr node == 0
{-# INLINE isIsolated #-}

-- | Return 'True' if the given 'Node' has no incoming edges, 'False' otherwise.
isSource :: (Graph gr) => gr a b -> Node -> Bool
isSource gr node = Data.Graph.Inductive.Graph.indeg gr node == 0
{-# INLINE isSource #-}

-- | Return 'True' if the given 'Node' has no outgoing edges, 'False' otherwise.
isSink :: (Graph gr) => gr a b -> Node -> Bool
isSink gr node = Data.Graph.Inductive.Graph.outdeg gr node == 0
{-# INLINE isSink #-}

-- | Return 'True' if the given 'Node' has both incoming and outgoing edges, 'False' otherwise.
isIntermediate :: (Graph gr) => gr a b -> Node -> Bool
isIntermediate gr node = (Data.Graph.Inductive.Graph.indeg gr node > 0) && (Data.Graph.Inductive.Graph.outdeg gr node > 0)

-- | Return 'True' if the given 'Node' has exactly one incoming or outgoing edge, 'False' otherwise.
isLeaf :: (Graph gr) => gr a b -> Node -> Bool
isLeaf gr node = Data.Graph.Inductive.Graph.deg gr node == 1
{-# INLINE isLeaf #-}

-- | Type of node types.
data NodeType
  = Isolated
  -- ^ Neither incoming nor outgoing edges.
  | Source
  -- ^ Outgoing, but no incoming, edges.
  | Sink
  -- ^ Incoming, but no outgoing, edges.
  | Intermediate
  -- ^ Both incoming and outgoing edges.
  deriving (Eq, Ord, Read, Show)

-- | Return the 'NodeType' for the given 'Node'.
getNodeType :: (Graph gr) => gr a b -> Node -> NodeType
getNodeType gr node = case liftA2 (liftA2 (,)) Data.Graph.Inductive.Graph.indeg Data.Graph.Inductive.Graph.outdeg gr node of
  (0, 0) -> Isolated
  (0, _) -> Source
  (_, 0) -> Sink
  _      -> Intermediate
{-# INLINE getNodeType #-}
