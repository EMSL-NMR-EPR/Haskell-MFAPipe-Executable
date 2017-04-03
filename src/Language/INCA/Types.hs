-----------------------------------------------------------------------------
-- |
-- Module      :  Language.INCA.Types
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of INCA syntax.
-----------------------------------------------------------------------------

module Language.INCA.Types
( -- * Arrows
  INCAArrow(..)
  -- * Chemical reactions
, INCAReaction(..) , INCAReactionSide(..) , INCAReactionPart(..)
  -- * Metabolic flux variables
, INCAFluxVar(..)
  -- * Metabolite variables
, INCAMetaboliteVar(..)
  -- * Elementary Metabolite Units (EMU)
, INCAEMU(..)
  -- ** Isotopomer and cumomer fractions
, INCAIsotopomerFraction(..)
, INCACumomerFraction(..)
, isINCAIsotopomerFraction
, toINCAIsotopomerFraction
, toINCACumomerFraction
  -- ** Mass fractions
, INCAMassFraction(..)
) where

import           Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.Maybe

-- | An arrow in INCA syntax.
data INCAArrow
  = INCAArrowL
  -- ^ @<-@
  | INCAArrowR
  -- ^ @->@
  | INCAArrowLR
  -- ^ @\<-\>@
  deriving (Eq, Ord, Read, Show)

-- | A chemical reaction in INCA syntax, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
data INCAReaction k a
  = INCAReaction INCAArrow (INCAReactionSide k a) (INCAReactionSide k a)
  deriving (Eq, Ord, Read, Show)

instance Bifunctor INCAReaction where
  bimap f g (INCAReaction arr ls rs) = INCAReaction arr (bimap f g ls) (bimap f g rs)
  {-# INLINE bimap #-}

instance Functor (INCAReaction k) where
  fmap = second
  {-# INLINE fmap #-}

-- | A chemical reaction side in INCA syntax, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
newtype INCAReactionSide k a
  = INCAReactionSide { getINCAReactionSide :: [INCAReactionPart k a] }
  deriving (Eq, Ord, Read, Show)

instance Bifunctor INCAReactionSide where
  bimap f g = INCAReactionSide . map (bimap f g) . getINCAReactionSide
  {-# INLINE bimap #-}

instance Functor (INCAReactionSide k) where
  fmap = second
  {-# INLINE fmap #-}

-- | A chemical reaction part in INCA syntax, where:
--
-- * @k@ is the type of chemical species; and,
--
-- * @a@ is the type of atoms.
--
data INCAReactionPart k a
  = INCAReactionPart Rational k [(Rational, [a])]
  -- ^ Stoichiometric coefficient, metabolite variable, and list of pairs of mixing probabilities and lists of atoms.
  deriving (Eq, Ord, Read, Show)

instance Bifunctor INCAReactionPart where
  bimap f g (INCAReactionPart c k xs) = INCAReactionPart c (f k) (map (second (map g)) xs)
  {-# INLINE bimap #-}

instance Functor (INCAReactionPart k) where
  fmap = second
  {-# INLINE fmap #-}

-- | A metabolic flux variable in INCA syntax.
data INCAFluxVar a
  = INCAFluxVar a (Maybe a)
  -- ^ Name and direction (optional).
  deriving (Eq, Ord, Read, Show)

instance Functor INCAFluxVar where
  fmap f (INCAFluxVar name directionMaybe) = INCAFluxVar (f name) (fmap f directionMaybe)
  {-# INLINE fmap #-}

-- | A metabolite variable in INCA syntax.
data INCAMetaboliteVar a
  = INCAMetaboliteVar a (Maybe a)
  -- ^ Name and compartment name (optional).
  deriving (Eq, Ord, Read, Show)

instance Functor INCAMetaboliteVar where
  fmap f (INCAMetaboliteVar name compartmentNameMaybe) = INCAMetaboliteVar (f name) (fmap f compartmentNameMaybe)
  {-# INLINE fmap #-}

-- | An Elementary Metabolite Unit (EMU) in INCA syntax.
data INCAEMU a
  = INCAEMU a [Int]
  deriving (Eq, Ord, Read, Show)

instance Functor INCAEMU where
  fmap f (INCAEMU k ixs) = INCAEMU (f k) ixs
  {-# INLINE fmap #-}

-- | An isotopomer fraction in INCA syntax.
data INCAIsotopomerFraction a
  = INCAIsotopomerFraction [a] [Int]
  deriving (Eq, Ord, Read, Show)

instance Functor INCAIsotopomerFraction where
  fmap f (INCAIsotopomerFraction k ixs) = INCAIsotopomerFraction (map f k) ixs
  {-# INLINE fmap #-}

-- | A cumomer fraction in INCA syntax.
data INCACumomerFraction a
  = INCACumomerFraction [a] [Maybe Int]
  deriving (Eq, Ord, Read, Show)

instance Functor INCACumomerFraction where
  fmap f (INCACumomerFraction k ixs) = INCACumomerFraction (map f k) ixs
  {-# INLINE fmap #-}

isINCAIsotopomerFraction :: INCACumomerFraction a -> Bool
-- isINCAIsotopomerFraction = Data.Maybe.isJust . toINCAIsotopomerFraction
isINCAIsotopomerFraction (INCACumomerFraction _ ixs) = all Data.Maybe.isJust ixs
{-# INLINE isINCAIsotopomerFraction #-}

toINCAIsotopomerFraction :: INCACumomerFraction a -> Maybe (INCAIsotopomerFraction a)
toINCAIsotopomerFraction (INCACumomerFraction k ixs) = fmap (INCAIsotopomerFraction k) (sequence ixs)
{-# INLINE toINCAIsotopomerFraction #-}

toINCACumomerFraction :: INCAIsotopomerFraction a -> INCACumomerFraction a
toINCACumomerFraction (INCAIsotopomerFraction k ixs) = INCACumomerFraction k (map Just ixs)
{-# INLINE toINCACumomerFraction #-}

-- | A mass fraction in INCA syntax.
data INCAMassFraction a
  = INCAMassFraction [a] Int
  deriving (Eq, Ord, Read, Show)

instance Functor INCAMassFraction where
  fmap f (INCAMassFraction ks ix) = INCAMassFraction (map f ks) ix
  {-# INLINE fmap #-}
