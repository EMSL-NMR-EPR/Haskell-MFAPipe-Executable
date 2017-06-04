{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.FractionTypeDict
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of dictionaries of
-- fraction-specific types.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.FractionTypeDict
( -- * HasFractionTypeDict class
  HasFractionTypeDict(..)
, fractionTypeDict
  -- * FractionTypeDict type
, FractionTypeDict
  -- * Operators
  -- * Query
, null
, size
, member
, notMember
, lookup
  -- * Construction
, empty
, singleton
  -- * Insertion
, insert
, insertWith
, insertWithKey
  -- * Delete/Update
  -- * Combine
  -- ** Union
, union
, unionWith
, unionWithKey
  -- ** Difference
  -- ** Intersection
  -- ** Universal combining function
  -- * Traversal
  -- ** Map
  -- ** Folds
, foldrWithKey
  -- *** Strict folds
, foldrWithKey'
  -- * Conversion
, elems
, keys
, assocs
, keysSet
, fromSet
  -- ** Lists
, toList
, fromList
, fromListWith
, fromListWithKey
  -- ** Ordered lists
  -- * Filter
  -- * Indexed
  -- * Debugging
) where

import qualified Data.Foldable
import           Data.Functor.Sum (Sum(..))
import           Data.Injective (Fix)
import qualified Data.Injective
import           Data.Injective.Numeric (FloatingF(..), FractionalF(..), NumF(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Set (Set)
import qualified Data.Set
import qualified GHC.Exts
import           Prelude hiding (null, lookup)
import           Science.Chemistry.IsotopicLabeling.FractionType

class (Ord k, Ord a, Functor f) => HasFractionTypeDict k a f where
  fractionTypeDictAlg :: f (FractionTypeDict k a) -> FractionTypeDict k a

fractionTypeDict :: (HasFractionTypeDict k a f) => Fix f -> FractionTypeDict k a
fractionTypeDict = Data.Injective.cata fractionTypeDictAlg

instance (HasFractionTypeDict k a f, HasFractionTypeDict k a g) => HasFractionTypeDict k a (f `Sum` g) where
  fractionTypeDictAlg (InL fm) = fractionTypeDictAlg fm
  fractionTypeDictAlg (InR gm) = fractionTypeDictAlg gm

instance (Ord k, Ord a) => HasFractionTypeDict k a NumF where
  fractionTypeDictAlg (Add mL mR) = mL `mappend` mR
  fractionTypeDictAlg (Subtract mL mR) = mL `mappend` mR
  fractionTypeDictAlg (Multiply mL mR) = mL `mappend` mR
  fractionTypeDictAlg (Negate m) = m
  fractionTypeDictAlg (Abs m) = m
  fractionTypeDictAlg (Signum m) = m
  fractionTypeDictAlg (FromInteger _) = mempty

instance (Ord k, Ord a) => HasFractionTypeDict k a FractionalF where
  fractionTypeDictAlg (Divide mL mR) = mL `mappend` mR
  fractionTypeDictAlg (Recip m) = m
  fractionTypeDictAlg (FromRational _) = mempty

instance (Ord k, Ord a) => HasFractionTypeDict k a FloatingF where
  fractionTypeDictAlg Pi = mempty
  fractionTypeDictAlg (Exp m) = m
  fractionTypeDictAlg (Log m) = m
  fractionTypeDictAlg (Sqrt m) = m
  fractionTypeDictAlg (Power mL mR) = mL `mappend` mR
  fractionTypeDictAlg (LogBase mL mR) = mL `mappend` mR
  fractionTypeDictAlg (Sin m) = m
  fractionTypeDictAlg (Cos m) = m
  fractionTypeDictAlg (Tan m) = m
  fractionTypeDictAlg (ArcSin m) = m
  fractionTypeDictAlg (ArcCos m) = m
  fractionTypeDictAlg (ArcTan m) = m
  fractionTypeDictAlg (Sinh m) = m
  fractionTypeDictAlg (Cosh m) = m
  fractionTypeDictAlg (Tanh m) = m
  fractionTypeDictAlg (ArcSinh m) = m
  fractionTypeDictAlg (ArcCosh m) = m
  fractionTypeDictAlg (ArcTanh m) = m

data FractionTypeDict k a = FractionTypeDict {-# UNPACK #-} !Int (Map FractionType (Map k (Map a Int)))
  deriving (Eq, Ord, Read, Show)

instance (Ord k, Ord a) => Monoid (FractionTypeDict k a) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = union
  {-# INLINE mappend #-}

-- (!)

-- (\\)

null :: FractionTypeDict k a -> Bool
null (FractionTypeDict 0 _) = True
null _ = False

size :: FractionTypeDict k a -> Int
size (FractionTypeDict n0 _) = n0

member :: (Ord k, Ord a) => FractionType -> k -> a -> FractionTypeDict k a -> Bool
member _ _ _ (FractionTypeDict 0 _) = False
member ty k a (FractionTypeDict _ m) = maybe False (maybe False (Data.Map.Strict.member a) . Data.Map.Strict.lookup k) . Data.Map.Strict.lookup ty $ m

notMember :: (Ord k, Ord a) => FractionType -> k -> a -> FractionTypeDict k a -> Bool
notMember _ _ _ (FractionTypeDict 0 _) = True
notMember ty k a (FractionTypeDict _ m) = maybe True (maybe True (Data.Map.Strict.notMember a) . Data.Map.Strict.lookup k) . Data.Map.Strict.lookup ty $ m

lookup :: (Ord k, Ord a) => FractionType -> k -> a -> FractionTypeDict k a -> Maybe Int
lookup _ _ _ (FractionTypeDict 0 _) = Nothing
lookup ty k a (FractionTypeDict _ m) = Data.Map.Strict.lookup ty m >>= Data.Map.Strict.lookup k >>= Data.Map.Strict.lookup a

-- findWithDefault

-- lookupLT

-- lookupGT

-- lookupLE

-- lookupGE

empty :: FractionTypeDict k a
empty = FractionTypeDict 0 Data.Map.Strict.empty

singleton :: FractionType -> k -> a -> Int -> FractionTypeDict k a
singleton ty k a n = FractionTypeDict 1 (Data.Map.Strict.singleton ty (Data.Map.Strict.singleton k (Data.Map.Strict.singleton a n)))

insert :: (Ord k, Ord a) => FractionType -> k -> a -> Int -> FractionTypeDict k a -> FractionTypeDict k a
insert = insertWith (+)

insertWith :: (Ord k, Ord a) => (Int -> Int -> Int) -> FractionType -> k -> a -> Int -> FractionTypeDict k a -> FractionTypeDict k a
insertWith f = insertWithKey (\_ty _k _a -> f)

insertWithKey :: (Ord k, Ord a) => (FractionType -> k -> a -> Int -> Int -> Int) -> FractionType -> k -> a -> Int -> FractionTypeDict k a -> FractionTypeDict k a
insertWithKey f ty k a n t@(FractionTypeDict n0 m) = FractionTypeDict new_size (Data.Map.Strict.alter (Just . Data.Map.Strict.alter (Just . Data.Map.Strict.insertWith (f ty k a) a n . maybe Data.Map.Strict.empty id) k . maybe Data.Map.Strict.empty id) ty m)
  where
    new_size :: Int
    new_size
      | member ty k a t = n0
      | otherwise = succ n0

-- insertLookupWithKey

-- delete

-- adjust

-- adjustWithKey

-- update

-- updateWithKey

-- updateLookupWithKey

-- alter

union :: (Ord k, Ord a) => FractionTypeDict k a -> FractionTypeDict k a -> FractionTypeDict k a
union = foldrWithKey' insert

unionWith :: (Ord k, Ord a) => (Int -> Int -> Int) -> FractionTypeDict k a -> FractionTypeDict k a -> FractionTypeDict k a
unionWith f = foldrWithKey' (insertWith f)

unionWithKey :: (Ord k, Ord a) => (FractionType -> k -> a -> Int -> Int -> Int) -> FractionTypeDict k a -> FractionTypeDict k a -> FractionTypeDict k a
unionWithKey f = foldrWithKey' (insertWithKey f)

-- unions

-- unionsWith

-- difference

-- differenceWith

-- differenceWithKey

-- intersection

-- intersectionWith

-- intersectionWithKey

-- mergeWithKey

-- map

-- mapWithKey

-- traverseWithkey

-- mapAccum

-- mapAccumWithKey

-- mapKeys

-- mapKeysMonotonic

-- foldr

-- foldl

foldrWithKey :: (FractionType -> k -> a -> Int -> b -> b) -> b -> FractionTypeDict k a -> b
foldrWithKey c z (FractionTypeDict _ m) = flip (Data.Map.Strict.foldrWithKey (\ty -> flip (Data.Map.Strict.foldrWithKey (\k -> flip (Data.Map.Strict.foldrWithKey (c ty k)))))) m z

-- foldlWithKey

-- foldMapWithKey

-- foldr'

-- foldl'

foldrWithKey' :: (FractionType -> k -> a -> Int -> b -> b) -> b -> FractionTypeDict k a -> b
foldrWithKey' c z (FractionTypeDict _ m) = flip (Data.Map.Strict.foldrWithKey' (\ty -> flip (Data.Map.Strict.foldrWithKey' (\k -> flip (Data.Map.Strict.foldrWithKey' (c ty k)))))) m z

-- foldlWithKey'

-- foldMapWithKey'

elems :: FractionTypeDict k a -> [Int]
elems t = GHC.Exts.build (\cons nil -> foldrWithKey' (\_ty _k _a n -> cons n) nil t)

keys :: FractionTypeDict k a -> [(FractionType, k, a)]
keys t = GHC.Exts.build (\cons nil -> foldrWithKey' (\ty k a _n -> cons (ty, k, a)) nil t)

assocs :: FractionTypeDict k a -> [(FractionType, k, a, Int)]
assocs t = GHC.Exts.build (\cons nil -> foldrWithKey' (\ty k a n -> cons (ty, k, a, n)) nil t)

keysSet :: (Ord k, Ord a) => FractionTypeDict k a -> Set (FractionType, k, a)
keysSet = Data.Set.fromList . keys

fromSet :: (Ord k, Ord a) => (FractionType -> k -> a -> Int) -> Set (FractionType, k, a) -> FractionTypeDict k a
fromSet f = Data.Set.foldr' (\(ty, k, a) -> insert ty k a (f ty k a)) empty

toList :: FractionTypeDict k a -> [(FractionType, k, a, Int)]
toList = assocs

fromList :: (Ord k, Ord a) => [(FractionType, k, a, Int)] -> FractionTypeDict k a
fromList = Data.Foldable.foldr' (\(ty, k, a, n) -> insert ty k a n) empty

fromListWith :: (Ord k, Ord a) => (Int -> Int -> Int) -> [(FractionType, k, a, Int)] -> FractionTypeDict k a
fromListWith f = Data.Foldable.foldr' (\(ty, k, a, n) -> insertWith f ty k a n) empty

fromListWithKey :: (Ord k, Ord a) => (FractionType -> k -> a -> Int -> Int -> Int) -> [(FractionType, k, a, Int)] -> FractionTypeDict k a
fromListWithKey f = Data.Foldable.foldr' (\(ty, k, a, n) -> insertWithKey f ty k a n) empty
