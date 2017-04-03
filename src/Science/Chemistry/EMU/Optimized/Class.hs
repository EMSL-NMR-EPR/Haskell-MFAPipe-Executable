{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.Optimized.Class
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports classes for optimizing Elementary Metabolite Units
-- (EMU) types.
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.Optimized.Class
( Optimized(..)
) where

import           Data.Set (Set)

-- | The 'Optimized' class is used for types that can be optimized for a set of objects.
class (Ord a) => Optimized a t | t -> a where
  {-# MINIMAL optimize #-}
  
  -- | @optimize xs s@ is the optimization of @s@ with respect to @xs@.
  optimize :: Set a -> t -> t
