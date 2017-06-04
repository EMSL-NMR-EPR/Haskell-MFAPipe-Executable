{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Injective
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports classes and types that are used for the implementation
-- of injective functors.
--
-- See also
--
--  * \"Data types a la carte\",
--    by W. Swierstra,
--    /Journal of functional programming/ 18:4 (2008) 423-436, online at
--    <http://www.cs.nott.ac.uk/~wss/Publications/DataTypesALaCarte.pdf>.
--
-----------------------------------------------------------------------------

module Data.Injective
  ( -- * The ':<:' class
    (:<:)(..)
    -- * Fixed points
  , Fix(..)
    -- * Natural transformations
  , hoist
    -- * Folding
  , inject
  , project
  , cata
  ) where

import           Data.Functor.Sum (Sum(..))

-- | The ':<:' class is used for types that can be injected into other types.
class (Functor sub, Functor sup) => (:<:) sub sup where
  -- | Inject sub-type into super-type.
  inj :: sub a -> sup a
  -- | Project super-type into sub-type.
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  {-# INLINE inj #-}
  prj = Just
  {-# INLINE prj #-}

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f `Sum` g) where
  inj = InL
  {-# INLINE inj #-}
  prj (InL x) = Just x
  prj _ = Nothing
  {-# INLINE prj #-}

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: h) => f :<: (g `Sum` h) where
  inj = InR . inj
  {-# INLINE inj #-}
  prj (InR x) = prj x
  prj _ = Nothing
  {-# INLINE prj #-}

-- | Least fixed-point of a 'Functor'.
newtype Fix f = In { out :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Read (f (Fix f)) => Read (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

-- | Natural transformation.
hoist :: (Functor g) => (forall x. f x -> g x) -> Fix f -> Fix g
hoist nat = In . fmap (hoist nat) . nat . out
{-# INLINE hoist #-}

-- | Inject sub-type into super-type.
inject :: (f :<: g) => f (Fix g) -> Fix g
inject = In . inj
{-# INLINE inject #-}

-- | Project super-type into sub-type.
project :: (f :<: g) => Fix g -> Maybe (f (Fix g))
project = prj . out
{-# INLINE project #-}

-- | Catamorphism.
cata
  :: (Functor f)
  => (f a -> a)
  -- ^ @f@-algebra
  -> Fix f
  -- ^ fixed point
  -> a
  -- ^ result
cata alg = alg . fmap (cata alg) . out
{-# INLINE cata #-}
