-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.EMU.Factorized.Class
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports classes for factorization of Elementary Metabolite
-- Units (EMU).
-----------------------------------------------------------------------------

module Science.Chemistry.EMU.Factorized.Class
( Factorized(..)
) where

-- | The 'Factorized' class is used for types that have a unique factorization.
class Factorized a where
  {-# MINIMAL factors #-}

  -- | @factors x@ is the list of factors of @x@.
  factors :: a -> [a]
