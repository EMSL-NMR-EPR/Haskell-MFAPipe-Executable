{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LinearAlgebra.HMatrix.Lens
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for linear algebra.
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.HMatrix.Lens
( -- * Construction
  -- ** Vectors
  _List
  -- ** Matrices
, _Lists
, _Columns
, _Rows
  -- * AsVector class
, AsVector(..)
  -- * AsMatrix class
, AsMatrix(..)
) where

import           Control.Lens (Iso)
import qualified Control.Lens
import           Data.Functor.Compose (Compose(..))
import           Data.Monoid.CartProduct (CartProduct(..))
import           Data.Monoid.Conv (Conv(..))
import           Data.Monoid.Conv2 (Conv2(..))
import           Data.Monoid.Kronecker (Kronecker(..))
import           Foreign.Storable (Storable())
import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix

-- | An 'Iso' between a list and a 'Vector'.
_List :: (Storable a, Storable b) => Iso [a] [b] (Vector a) (Vector b)
_List = Control.Lens.iso Numeric.LinearAlgebra.HMatrix.fromList Numeric.LinearAlgebra.HMatrix.toList

-- | An 'Iso' between a list of lists and a 'Matrix'.
_Lists :: (Element a, Element b) => Iso [[a]] [[b]] (Matrix a) (Matrix b)
_Lists = Control.Lens.iso Numeric.LinearAlgebra.HMatrix.fromLists Numeric.LinearAlgebra.HMatrix.toLists

-- | An 'Iso' between a list of 'Vector's and a column-major 'Matrix'.
_Columns :: (Element a, Element b) => Iso [Vector a] [Vector b] (Matrix a) (Matrix b)
_Columns = Control.Lens.iso Numeric.LinearAlgebra.HMatrix.fromColumns Numeric.LinearAlgebra.HMatrix.toColumns

-- | An 'Iso' between a list of 'Vector's and a row-major 'Matrix'.
_Rows :: (Element a, Element b) => Iso [Vector a] [Vector b] (Matrix a) (Matrix b)
_Rows = Control.Lens.iso Numeric.LinearAlgebra.HMatrix.fromRows Numeric.LinearAlgebra.HMatrix.toRows

-- | The 'AsVector' class is used for types that can be converted to and from 'Vector'.
class AsVector f where
  {-# MINIMAL _Vector #-}

  _Vector :: (Container Vector a, Container Vector b) => Iso (f a) (f b) (Vector a) (Vector b)

instance AsVector Vector where
  _Vector = id
  {-# INLINE _Vector #-}

instance AsVector [] where
  _Vector = _List

instance AsVector CartProduct where
  _Vector = Control.Lens.iso getCartProduct CartProduct

instance AsVector Conv where
  _Vector = Control.Lens.iso getConv Conv

-- | The 'AsMatrix' class is used for types that can be converted to and from 'Matrix'.
class AsMatrix f where
  {-# MINIMAL _Matrix #-}

  _Matrix :: (Container Vector a, Container Vector b, Element a, Element b) => Iso (f a) (f b) (Matrix a) (Matrix b)

instance AsMatrix Matrix where
  _Matrix = id
  {-# INLINE _Matrix #-}

instance AsMatrix (Compose [] []) where
  _Matrix = Control.Lens.iso getCompose Compose . _Lists

instance AsMatrix Conv2 where
  _Matrix = Control.Lens.iso getConv2 Conv2

instance AsMatrix Kronecker where
  _Matrix = Control.Lens.iso getKronecker Kronecker
