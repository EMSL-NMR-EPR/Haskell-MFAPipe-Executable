-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Inductive.Graph.Instances
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan instances of 'Graph' and 'DynGraph'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Graph.Inductive.Graph.Instances () where

import           Data.Graph.Inductive.Graph (DynGraph(..), Graph(..), OrdGr(..))

instance (Graph gr) => Graph (OrdGr gr) where
  empty = OrdGr empty
  {-# INLINE empty #-}
  isEmpty = isEmpty . unOrdGr
  {-# INLINE isEmpty #-}
  match a = fmap OrdGr . match a . unOrdGr
  {-# INLINE match #-}
  mkGraph as bs = OrdGr (mkGraph as bs)
  {-# INLINE mkGraph #-}
  labNodes = labNodes . unOrdGr
  {-# INLINE labNodes #-}
  matchAny = fmap OrdGr . matchAny . unOrdGr
  {-# INLINE matchAny #-}
  noNodes = noNodes . unOrdGr
  {-# INLINE noNodes #-}
  nodeRange = nodeRange . unOrdGr
  {-# INLINE nodeRange #-}
  labEdges = labEdges . unOrdGr
  {-# INLINE labEdges #-}

instance (DynGraph gr) => DynGraph (OrdGr gr) where
  (&) ctx = OrdGr . (&) ctx . unOrdGr
  {-# INLINE (&) #-}
