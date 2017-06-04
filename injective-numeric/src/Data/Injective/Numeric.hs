{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Injective.Numeric
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Injective.Numeric
  ( -- * Numeric types
    NumF(..)
  , FractionalF(..)
  , FloatingF(..)
  ) where

import           Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
import           Data.Injective ((:<:)(), Fix)
import qualified Data.Injective

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
  xL + xR = Data.Injective.inject (Add xL xR)
  {-# INLINE (+) #-}
  xL - xR = Data.Injective.inject (Subtract xL xR)
  {-# INLINE (-) #-}
  xL * xR = Data.Injective.inject (Multiply xL xR)
  {-# INLINE (*) #-}
  negate x = Data.Injective.inject (Negate x)
  {-# INLINE negate #-}
  abs x = Data.Injective.inject (Abs x)
  {-# INLINE abs #-}
  signum x = Data.Injective.inject (Signum x)
  {-# INLINE signum #-}
  fromInteger n = Data.Injective.inject (FromInteger n)
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
  xL / xR = Data.Injective.inject (Divide xL xR)
  {-# INLINE (/) #-}
  recip x = Data.Injective.inject (Recip x)
  {-# INLINE recip #-}
  fromRational n = Data.Injective.inject (FromRational n)
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
  pi = Data.Injective.inject Pi
  {-# INLINE pi #-}
  exp x = Data.Injective.inject (Exp x)
  {-# INLINE exp #-}
  log x = Data.Injective.inject (Log x)
  {-# INLINE log #-}
  sqrt x = Data.Injective.inject (Sqrt x)
  {-# INLINE sqrt #-}
  xL ** xR = Data.Injective.inject (Power xL xR)
  {-# INLINE (**) #-}
  logBase xL xR = Data.Injective.inject (LogBase xL xR)
  {-# INLINE logBase #-}
  sin x = Data.Injective.inject (Sin x)
  {-# INLINE sin #-}
  cos x = Data.Injective.inject (Cos x)
  {-# INLINE cos #-}
  tan x = Data.Injective.inject (Tan x)
  {-# INLINE tan #-}
  asin x = Data.Injective.inject (ArcSin x)
  {-# INLINE asin #-}
  acos x = Data.Injective.inject (ArcCos x)
  {-# INLINE acos #-}
  atan x = Data.Injective.inject (ArcTan x)
  {-# INLINE atan #-}
  sinh x = Data.Injective.inject (Sinh x)
  {-# INLINE sinh #-}
  cosh x = Data.Injective.inject (Cosh x)
  {-# INLINE cosh #-}
  tanh x = Data.Injective.inject (Tanh x)
  {-# INLINE tanh #-}
  asinh x = Data.Injective.inject (ArcSinh x)
  {-# INLINE asinh #-}
  acosh x = Data.Injective.inject (ArcCosh x)
  {-# INLINE acosh #-}
  atanh x = Data.Injective.inject (ArcTanh x)
  {-# INLINE atanh #-}
