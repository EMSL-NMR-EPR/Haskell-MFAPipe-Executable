-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.HasSize.Class
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports classes for calculating the size of Elementary
-- Metabolite Units (EMU) types.
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.HasSize.Class
( HasSize(..)
, partitionBySizeWith , partitionBySize
) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict

-- | The 'HasSize' class is used for types that have a size.
class HasSize a where
  {-# MINIMAL size #-}
  
  -- | @size x@ is the size of @x@.
  size :: a -> Int

partitionBySizeWith :: (Foldable f, HasSize b) => (a -> b) -> f a -> IntMap [a]
partitionBySizeWith f = foldr (\x -> Data.IntMap.Strict.alter (Just . (:) x . maybe [] id) (size (f x))) Data.IntMap.Strict.empty

partitionBySize :: (Foldable f, HasSize a) => f a -> IntMap [a]
partitionBySize = partitionBySizeWith id
