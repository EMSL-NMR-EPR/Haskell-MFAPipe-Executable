-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Graph.Extras.Node.Strict
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

module Data.Graph.Inductive.Graph.Extras.Node.Strict
( module Data.Graph.Inductive.Graph.Extras.Node
, partitionNodes
, partitionLabNodes
) where

import           Data.Graph.Inductive.Graph (Graph(), LNode, Node)
import qualified Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Graph.Extras.Node
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict

-- | Partition nodes by type.
partitionNodes :: (Graph gr) => gr a b -> Map NodeType [Node]
partitionNodes = partitionUsing Data.Graph.Inductive.Graph.nodes id
{-# INLINE partitionNodes #-}

-- | Partition labeled nodes by type.
partitionLabNodes :: (Graph gr) => gr a b -> Map NodeType [LNode a]
partitionLabNodes = partitionUsing Data.Graph.Inductive.Graph.labNodes fst
{-# INLINE partitionLabNodes #-}

-- ----------------------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------------------

partitionUsing :: (Functor f, Foldable f, Graph gr) => (gr a b -> f c) -> (c -> Node) -> gr a b -> Map NodeType [c]
partitionUsing elems toNode gr =
  let
    xs0 = elems gr
    xs1 = fmap (getNodeType gr . toNode >>= (,)) xs0
  in
    foldr (uncurry (flip (\x -> Data.Map.Strict.alter (Just . (:) x . maybe [] id)))) Data.Map.Strict.empty xs1
