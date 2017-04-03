{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
  -- * Numeric types
, NumF(..)
, FractionalF(..)
, FloatingF(..)
) where

import           Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
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

-- | Free 'Functor' for 'Num' class.
data NumF a
  = Add a a
  | Subtract a a
  | Multiply a a
  | Negate a
  | Abs a
  | Signum a
  | FromInteger Integer
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1 NumF where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance Ord1 NumF where
  compare1 = compare
  {-# INLINE compare1 #-}

instance Read1 NumF where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance Show1 NumF where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance (NumF :<: f) => Num (Fix f) where
  xL + xR = inject (Add xL xR)
  {-# INLINE (+) #-}
  xL - xR = inject (Subtract xL xR)
  {-# INLINE (-) #-}
  xL * xR = inject (Multiply xL xR)
  {-# INLINE (*) #-}
  negate x = inject (Negate x)
  {-# INLINE negate #-}
  abs x = inject (Abs x)
  {-# INLINE abs #-}
  signum x = inject (Signum x)
  {-# INLINE signum #-}
  fromInteger n = inject (FromInteger n)
  {-# INLINE fromInteger #-}

-- | Free 'Functor' for 'Fractional' class.
data FractionalF a
  = Divide a a
  | Recip a
  | FromRational Rational
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1 FractionalF where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance Ord1 FractionalF where
  compare1 = compare
  {-# INLINE compare1 #-}

instance Read1 FractionalF where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance Show1 FractionalF where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance (NumF :<: f, FractionalF :<: f) => Fractional (Fix f) where
  xL / xR = inject (Divide xL xR)
  {-# INLINE (/) #-}
  recip x = inject (Recip x)
  {-# INLINE recip #-}
  fromRational n = inject (FromRational n)
  {-# INLINE fromRational #-}

-- | Free 'Functor' for 'Floating' class.
data FloatingF a
  = Pi
  | Exp a
  | Log a
  | LogBase a a
  | Sqrt a
  | Power a a
  | Sin a
  | Cos a
  | Tan a
  | ArcSin a
  | ArcCos a
  | ArcTan a
  | Sinh a
  | Cosh a
  | Tanh a
  | ArcSinh a
  | ArcCosh a
  | ArcTanh a
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Eq1 FloatingF where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance Ord1 FloatingF where
  compare1 = compare
  {-# INLINE compare1 #-}

instance Read1 FloatingF where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance Show1 FloatingF where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance (NumF :<: f, FractionalF :<: f, FloatingF :<: f) => Floating (Fix f) where
  pi = inject Pi
  {-# INLINE pi #-}
  exp x = inject (Exp x)
  {-# INLINE exp #-}
  log x = inject (Log x)
  {-# INLINE log #-}
  sqrt x = inject (Sqrt x)
  {-# INLINE sqrt #-}
  xL ** xR = inject (Power xL xR)
  {-# INLINE (**) #-}
  logBase xL xR = inject (LogBase xL xR)
  {-# INLINE logBase #-}
  sin x = inject (Sin x)
  {-# INLINE sin #-}
  cos x = inject (Cos x)
  {-# INLINE cos #-}
  tan x = inject (Tan x)
  {-# INLINE tan #-}
  asin x = inject (ArcSin x)
  {-# INLINE asin #-}
  acos x = inject (ArcCos x)
  {-# INLINE acos #-}
  atan x = inject (ArcTan x)
  {-# INLINE atan #-}
  sinh x = inject (Sinh x)
  {-# INLINE sinh #-}
  cosh x = inject (Cosh x)
  {-# INLINE cosh #-}
  tanh x = inject (Tanh x)
  {-# INLINE tanh #-}
  asinh x = inject (ArcSinh x)
  {-# INLINE asinh #-}
  acosh x = inject (ArcCosh x)
  {-# INLINE acosh #-}
  atanh x = inject (ArcTanh x)
  {-# INLINE atanh #-}
