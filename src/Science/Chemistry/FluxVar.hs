-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.FluxVar
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of metabolic flux
-- variables.
-----------------------------------------------------------------------------

module Science.Chemistry.FluxVar
( -- * Metabolic Flux Variables
  FluxVar(..)
, Reversibility(..)
, Directionality(..)
  -- * Predicates
, isTransport , isExchange
, isReversible , isIrreversible
, isBackwards , isForwards
, isReverse
) where

import           Data.Bifunctor (Bifunctor(bimap, second))
import           Data.Csv (ToField(toField))
import           Data.Graph.Inductive.Query.DFS.EdgeSmoothing (Smoothable(smoothMaybe))
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text

-- | A metabolic flux variable.
data FluxVar a b
  = Transport a (Reversibility a)
  | Exchange b (Reversibility b)
  deriving (Eq, Ord, Read, Show)

instance Bifunctor FluxVar where
  bimap f _ (Transport x mx) = Transport (f x) (fmap f mx)
  bimap _ g (Exchange x mx) = Exchange (g x) (fmap g mx)
  {-# INLINE bimap #-}

instance Functor (FluxVar a) where
  fmap = second
  {-# INLINE fmap #-}

instance Smoothable (FluxVar a b) where
  smoothMaybe _ _ = Nothing
  {-# INLINE smoothMaybe #-}

instance (Pretty a, Pretty b) => Pretty (FluxVar a b) where
  pretty (Transport x _) = pretty x
  pretty (Exchange x _) = pretty x

instance (Pretty a, Pretty b) => ToField (FluxVar a b) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | The reversibility of a chemical reaction that is denoted by a 'FluxVar'.
data Reversibility a
  = Reversible (Directionality a)
  | Irreversible
  deriving (Eq, Ord, Read, Show)

instance Functor Reversibility where
  fmap f (Reversible x) = Reversible (fmap f x)
  fmap _ Irreversible = Irreversible
  {-# INLINE fmap #-}

-- | The direction of a chemical reaction that is denoted by a 'FluxVar'.
data Directionality a = BackwardsOf a | ForwardsOf a
  deriving (Eq, Ord, Read, Show)

instance Functor Directionality where
  fmap f (BackwardsOf x) = BackwardsOf (f x)
  fmap f (ForwardsOf x) = ForwardsOf (f x)
  {-# INLINE fmap #-}

-- | Return 'True' if the given metabolic flux variable is 'Transport', 'False' otherwise.
isTransport :: FluxVar a b -> Bool
isTransport (Transport _ _) = True
isTransport _ = False
{-# INLINE isTransport #-}

-- | Return 'True' if the given metabolic flux variable is 'Exchange', 'False' otherwise.
isExchange :: FluxVar a b -> Bool
isExchange (Exchange _ _) = True
isExchange _ = False
{-# INLINE isExchange #-}

-- | Return 'True' if the 'Reversibility' of the given metabolic flux variable is 'Reversible', 'False' otherwise.
isReversible :: FluxVar a b -> Bool
isReversible (Transport _ (Reversible _)) = True
isReversible (Exchange _ (Reversible _)) = True
isReversible _ = False
{-# INLINE isReversible #-}

-- | Return 'True' if the 'Reversibility' of the given metabolic flux variable is 'Irreversible', 'False' otherwise.
isIrreversible :: FluxVar a b -> Bool
isIrreversible (Transport _ Irreversible) = True
isIrreversible (Exchange _ Irreversible) = True
isIrreversible _ = False
{-# INLINE isIrreversible #-}

-- | Return 'True' if the 'Directionality' of the given metabolic flux variable is 'BackwardsOf', 'False' otherwise.
isBackwards :: FluxVar a b -> Bool
isBackwards (Transport _ (Reversible (BackwardsOf _))) = True
isBackwards (Exchange _ (Reversible (BackwardsOf _))) = True
isBackwards _ = False
{-# INLINE isBackwards #-}

-- | Return 'True' if the 'Reversibility' of the given metabolic flux variable is 'Irreversible' or if 'Directionality' of the given metabolic flux variable is 'ForwardsOf', 'False' otherwise.
isForwards :: FluxVar a b -> Bool
isForwards (Transport _ Irreversible) = True
isForwards (Transport _ (Reversible (ForwardsOf _))) = True
isForwards (Exchange _ Irreversible) = True
isForwards (Exchange _ (Reversible (ForwardsOf _))) = True
isForwards _ = False
{-# INLINE isForwards #-}

-- | Return 'True' if the given metabolic flux variables are the reverse of each other, 'False' otherwise.
--
-- Note: This function is reflexive, i.e.,
-- 
-- > forall x y. isReverse x y == isReverse y x
--
isReverse :: (Eq a, Eq b) => FluxVar a b -> FluxVar a b -> Bool
isReverse (Transport xL (Reversible (ForwardsOf xR))) (Transport yL (Reversible (BackwardsOf yR))) = (xL == yR) && (xR == yL)
isReverse (Transport xL (Reversible (BackwardsOf xR))) (Transport yL (Reversible (ForwardsOf yR))) = (xL == yR) && (xR == yL)
isReverse (Exchange xL (Reversible (ForwardsOf xR))) (Exchange yL (Reversible (BackwardsOf yR))) = (xL == yR) && (xR == yL)
isReverse (Exchange xL (Reversible (BackwardsOf xR))) (Exchange yL (Reversible (ForwardsOf yR))) = (xL == yR) && (xR == yL)
isReverse _ _ = False
{-# INLINE isReverse #-}
