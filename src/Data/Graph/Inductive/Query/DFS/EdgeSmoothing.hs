-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Query.DFS.EdgeSmoothing
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports an implementation of the edge smoothing algorithm.
--
-- See also
--
--  * \"Homeomorphism (graph theory)\",
--    from Wikipedia, the free encyclopedia, online at
--    <https://en.wikipedia.org/wiki/Homeomorphism_(graph_theory)>.
--
-----------------------------------------------------------------------------

module Data.Graph.Inductive.Query.DFS.EdgeSmoothing
( -- * Smoothable class
  Smoothable(..)
  -- * Operations
  -- ** Edge smoothing with a predicate
, smoothEdgesWhen , smoothEdgesWhen'
) where

import           Control.Applicative (liftA2)
import qualified Data.Foldable
import           Data.Graph.Inductive.Graph (DynGraph(), LEdge)
import qualified Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph.Extras.Node
import qualified Data.Graph.Inductive.Query.DFS

-- | The 'Smoothable' class is used for types that are edge labels and can be smoothed.
class Smoothable a where
  {-# MINIMAL smoothMaybe #-}
  
  -- | @smoothMaybe x y@ is the smoothed edge label.
  --
  -- If @'Just' z@, then the pair of 'LEdge's is smoothed. If 'Nothing', then the pair of 'LEdge's is not smoothed.
  smoothMaybe :: a -> a -> Maybe a

-- | Edge smoothing.
smoothEdgesWhen
  :: (DynGraph gr, Eq b)
  => (b -> b -> Maybe b)
  -- ^ Predicate and constructor for 'LEdge' labels. If 'Nothing', then the pair of 'LEdge's is not smoothed.
  -> (a -> Bool)
  -- ^ Predicate for 'Data.Graph.Inductive.Graph.LNode' labels. If 'False', then the 'Data.Graph.Inductive.Graph.LNode' is skipped.
  -> gr a b
  -> gr a b
smoothEdgesWhen c p =
  Data.Graph.Inductive.Query.DFS.topsort >>= go
    where
      go [] acc =
        let
          -- Gather all isolated nodes.
          nodes = liftA2 filter Data.Graph.Inductive.Graph.Extras.Node.isIsolated Data.Graph.Inductive.Graph.nodes acc
        in
          -- Delete all isolated nodes.
          Data.Foldable.foldr Data.Graph.Inductive.Graph.delNode acc nodes
      go (n:ns) acc = case Data.Graph.Inductive.Graph.lab acc n of
        Nothing ->
          -- If the current node is unlabeled, then continue with the next node.
          go ns acc
        Just m
          | p m ->
              let
                -- If the current node is labeled, and the label satisfies the predicate, then construct a new graph with smoothed edges.
                f = case (Data.Graph.Inductive.Graph.inn acc n, Data.Graph.Inductive.Graph.out acc n) of
                  (inLEdge:[], outLEdge:[]) ->
                    -- An edge is smoothed if and only if the current node has exactly one incoming and outgoing edge.
                    smoothEdgeWith c inLEdge outLEdge
                  _ ->
                    -- Otherwise, do nothing.
                    id
              in
                go ns (f acc)
          | otherwise ->
              go ns acc

-- | Edge smoothing using 'smoothMaybe'.
smoothEdgesWhen'
  :: (Smoothable b, DynGraph gr, Eq b)
  => (a -> Bool)
  -- ^ Predicate for 'Data.Graph.Inductive.Graph.LNode' labels. If 'False', then the 'Data.Graph.Inductive.Graph.LNode' is skipped.
  -> gr a b
  -> gr a b
smoothEdgesWhen' = smoothEdgesWhen smoothMaybe

-- | Smooth a pair of labeled edges.
smoothEdgeWith :: (DynGraph gr, Eq b) => (b -> b -> Maybe b) -> LEdge b -> LEdge b -> gr a b -> gr a b
smoothEdgeWith c inLEdge@(pre, _, inEdgeLab) outLEdge@(_, suc, outEdgeLab) = maybe id (\x -> Data.Graph.Inductive.Graph.insEdge (pre, suc, x) . Data.Graph.Inductive.Graph.delLEdge inLEdge . Data.Graph.Inductive.Graph.delLEdge outLEdge) (c outEdgeLab inEdgeLab)
