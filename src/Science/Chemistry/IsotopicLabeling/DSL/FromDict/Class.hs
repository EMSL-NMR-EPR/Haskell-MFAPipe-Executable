{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.DSL.FromDict.Class
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types and functions for dereferencing domain-specific
-- language (DSL) types.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.DSL.FromDict.Class
( FromDict(..)
, fromDict
) where

import           Control.Applicative (Const(..), liftA, liftA2)
import           Control.Comonad.Trans.Rall (RallT)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except
import           Data.Functor.Product (Product(..))
import           Data.Functor.Sum (Sum(..))
import           Data.Injective (Fix, FloatingF(..), FractionalF(..), NumF(..))
import qualified Data.Injective

class (Ord i, Functor f) => FromDict i e f where
  type Dict f i :: * -> *
  type DictError f i :: *
  fromDictAlg
    :: (Monad m)
    => Dict f i e
    -> f (ExceptT (DictError f i) m (RallT ((->) i) e))
    -> ExceptT (DictError f i) m (RallT ((->) i) e)

fromDict
  :: (FromDict i e f, Monad m)
  => Dict f i e
  -> Fix f
  -> ExceptT (DictError f i) m (RallT ((->) i) e)
fromDict dict = Data.Injective.cata (fromDictAlg dict)

instance (FromDict i e f, FromDict i e g) => FromDict i e (f `Sum` g) where
  type Dict (f `Sum` g) i = Dict f i `Product` Dict g i
  type DictError (f `Sum` g) i = Either (DictError f i) (DictError g i)
  fromDictAlg (Pair dictL _) (InL x) = Control.Monad.Trans.Except.withExceptT Left (fromDictAlg dictL (Control.Monad.Trans.Except.withExceptT (either id (error "Right")) <$> x))
  fromDictAlg (Pair _ dictR) (InR x) = Control.Monad.Trans.Except.withExceptT Right (fromDictAlg dictR (Control.Monad.Trans.Except.withExceptT (either (error "Left") id) <$> x))

instance (Ord i, Num e) => FromDict i e NumF where
  type Dict NumF i = Const ()
  type DictError NumF i = ()
  fromDictAlg (Const ()) = go
    where
      go (Add xL xR) = liftA2 (+) xL xR
      go (Subtract xL xR) = liftA2 (-) xL xR
      go (Multiply xL xR) = liftA2 (*) xL xR
      go (Negate x) = liftA negate x
      go (Abs x) = liftA abs x
      go (Signum x) = liftA signum x
      go (FromInteger n) = pure (fromInteger n)

instance (Ord i, Fractional e) => FromDict i e FractionalF where
  type Dict FractionalF i = Const ()
  type DictError FractionalF i = ()
  fromDictAlg (Const ()) = go
    where
      go (Divide xL xR) = liftA2 (/) xL xR
      go (Recip x) = liftA recip x
      go (FromRational n) = pure (fromRational n)

instance (Ord i, Floating e) => FromDict i e FloatingF where
  type Dict FloatingF i = Const ()
  type DictError FloatingF i = ()
  fromDictAlg (Const ()) = go
    where
      go Pi = pure pi
      go (Exp x) = liftA exp x
      go (Log x) = liftA log x
      go (Sqrt x) = liftA sqrt x
      go (Power xL xR) = liftA2 (**) xL xR
      go (LogBase xL xR) = liftA2 logBase xL xR
      go (Sin x) = liftA sin x
      go (Cos x) = liftA cos x
      go (Tan x) = liftA tan x
      go (ArcSin x) = liftA asin x
      go (ArcCos x) = liftA acos x
      go (ArcTan x) = liftA atan x
      go (Sinh x) = liftA sinh x
      go (Cosh x) = liftA cosh x
      go (Tanh x) = liftA tanh x
      go (ArcSinh x) = liftA asinh x
      go (ArcCosh x) = liftA acosh x
      go (ArcTanh x) = liftA atanh x
