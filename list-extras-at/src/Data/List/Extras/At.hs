-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Extras.At
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports assorted functions that relate to lists.
-----------------------------------------------------------------------------

module Data.List.Extras.At
( -- * Searching lists
  -- ** Searching by index
  partitionAt
  -- * Special lists
  -- ** \"Set\" operations
, deleteAt
  -- ** Ordered lists
, insertAt
) where

-- | The 'partitionAt' function takes a list of indices and a list, and returns the pair of lists of elements which are and arn't at the specified indices.
--
-- Note: Elements are returned in their original order.
partitionAt :: [Int] -> [a] -> ([a], [a])
partitionAt = go 0 ([], [])
  where
    go :: Int -> ([a], [a]) -> [Int] -> [a] -> ([a], [a])
    go _ (asL, asR) [] as = (reverse asL, reverse asR ++ as)
    go _ (asL, asR) _ [] = (reverse asL, reverse asR)
    go ~n0 (asL, asR) ns0@(n:ns) (a:as)
      | n0 == n = go (n0 + 1) (a:asL, asR) ns as
      | otherwise = go (n0 + 1) (asL, a:asR) ns0 as
    {-# INLINE go #-}
{-# INLINE partitionAt #-}

-- | The 'deleteAt' function removes the elements of the list at the specified indices.
deleteAt :: [Int] -> [a] -> [a]
deleteAt = go 0
  where
    go :: Int -> [Int] -> [a] -> [a]
    go _ [] as = as
    go _ _ [] = []
    go ~n0 ns0@(n:ns) (a:as)
      | n0 == n = go (n0 + 1) ns as
      | otherwise = a : go (n0 + 1) ns0 as
    {-# INLINE go #-}
{-# INLINE deleteAt #-}

-- | The 'insertAt' function takes a list of index/element pairs and a list, and returns a new list with the elements inserted at the specified indices.
insertAt :: [(Int, a)] -> [a] -> [a]
insertAt = go 0
  where
    go :: Int -> [(Int, a)] -> [a] -> [a]
    go _ [] as = as
    -- go _ _ [] = []
    go ~n0 ns0@((n, new_a):ns) as0@[]
      | n0 == n = new_a : go (n0 + 1) ns as0
      | otherwise = undefined : go (n0 + 1) ns0 as0
    go ~n0 ns0@((n, new_a):ns) as0@(a:as)
      | n0 == n = new_a : go (n0 + 1) ns as0
      | otherwise = a : go (n0 + 1) ns0 as
    {-# INLINE go #-}
{-# INLINE insertAt #-}
