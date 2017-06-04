{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Functor.Classes.Monoid1
( Monoid1(..)
, (<<>>)
) where

import GHC.Exts (Constraint)

class Monoid1 f where
  type Monoid1Ty (f :: * -> *) (a :: *) :: Constraint
  mempty1 :: (Monoid1Ty f a) => f a
  mappend1 :: (Monoid1Ty f a) => f a -> f a -> f a
  mconcat1 :: (Monoid1Ty f a) => [f a] -> f a

(<<>>) :: (Monoid1 f, Monoid1Ty f a) => f a -> f a -> f a
(<<>>) = mappend1
