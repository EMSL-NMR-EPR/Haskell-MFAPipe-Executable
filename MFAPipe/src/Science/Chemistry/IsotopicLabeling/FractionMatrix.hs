{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.FractionMatrix
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation, construction and
-- dereference of fraction-specific matrices.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.FractionMatrix
( -- * FractionMatrix type family
  FractionMatrix
, _FractionMatrix
  -- * FractionMatrixDict type
, FractionMatrixDict(..)
  -- * FractionMatrixDictError type
, FractionMatrixDictError(..)
  -- * FractionMatrixPtr type
, FractionMatrixPtr(..)
, runFractionMatrixPtr
  -- * FractionMatrixPtrObject type family
, FractionMatrixPtrObject
  -- * FromFractionMatrixPtr class
, FromFractionMatrixPtr(..)
  -- * LookupFractionMatrixF type
, LookupFractionMatrixF(..)
, lookupIsotopomerFractionMatrix , lookupMassFractionMatrix
) where

import           Control.Comonad.Trans.Rall (RallT(..))
import           Control.Lens (Iso)
import qualified Control.Lens
import           Control.Monad.Except (MonadError())
import qualified Control.Monad.Except
import           Data.Bifunctor (Bifunctor(bimap))
import           Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
import           Data.Injective ((:<:)(), Fix)
import qualified Data.Injective
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
-- import           Data.Monoid (Product(..), Sum(..))
import           Data.Proxy (Proxy(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), IndexOf, Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Lens (AsMatrix(_Matrix))
import           Science.Chemistry.IsotopicLabeling.FractionType
import           Science.Chemistry.IsotopicLabeling.Isotopomer
import           Science.Chemistry.IsotopicLabeling.IsotopomerFractionMatrix
import           Science.Chemistry.IsotopicLabeling.MassFraction
import           Science.Chemistry.IsotopicLabeling.MassFractionMatrix
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<>))
import qualified Text.PrettyPrint.Leijen.Text

-- | Type family that asserts the fraction-specific 'Matrix' type for each
-- 'FractionType'.
type family FractionMatrix (ty :: FractionType) :: * -> * where
  FractionMatrix 'IsotopomerFractionTy = IsotopomerFractionMatrix
  FractionMatrix 'MassFractionTy = MassFractionMatrix
  -- FractionMatrix 'ProductTy = Product
  -- FractionMatrix 'SumTy = Sum

-- instance AsMatrix Product where
--   _Matrix = Control.Lens.iso getProduct Product . Control.Lens.iso Numeric.LinearAlgebra.HMatrix.scalar Numeric.LinearAlgebra.HMatrix.prodElements
--
-- instance AsMatrix Sum where
--   _Matrix = Control.Lens.iso getSum Sum . Control.Lens.iso Numeric.LinearAlgebra.HMatrix.scalar Numeric.LinearAlgebra.HMatrix.sumElements

-- | An 'Iso' between fraction-specific 'Matrix' and 'Matrix'.
_FractionMatrix :: (AsMatrix (FractionMatrix ty), Container Vector a, Container Vector b, Element a, Element b) => Proxy ty -> Iso (FractionMatrix ty a) (FractionMatrix ty b) (Matrix a) (Matrix b)
_FractionMatrix Proxy = _Matrix

-- | A dictionary for instances of a fraction-specific 'Matrix' type.
data FractionMatrixDict (ty :: FractionType) i k a = FractionMatrixDict (Proxy ty) (Map k (FractionMatrix ty a)) (Map i (Map k (FractionMatrix ty a)))

deriving instance (Eq i, Eq k, Eq (FractionMatrix ty a)) => Eq (FractionMatrixDict ty i k a)
deriving instance (Ord i, Ord k, Ord (FractionMatrix ty a)) => Ord (FractionMatrixDict ty i k a)
deriving instance (Ord i, Ord k, Read i, Read k, Read (FractionMatrix ty a)) => Read (FractionMatrixDict ty i k a)
deriving instance (Show i, Show k, Show (FractionMatrix ty a)) => Show (FractionMatrixDict ty i k a)

-- | An error that is thrown when a 'FractionMatrixPtr' is dereferenced.
data FractionMatrixDictError (ty :: FractionType) i k
  = FractionMatrixNotFound (FractionMatrixPtr ty k)
  | FractionMatrixIndexOutOfBounds (FractionMatrixPtr ty k)
  | FractionMatrixSensitivityNotFound (FractionMatrixPtr ty k) i
  | FractionMatrixSensitivityIndexOutOfBounds (FractionMatrixPtr ty k) i
  deriving (Eq, Ord, Read, Show)

-- | A pointer to a fraction-specific 'Matrix' in a 'FractionMatrixDict'.
data FractionMatrixPtr (ty :: FractionType) k = FractionMatrixPtr (Proxy ty) k (IndexOf Matrix)
  deriving (Eq, Ord, Read, Show)

instance Functor (FractionMatrixPtr ty) where
  fmap f (FractionMatrixPtr Proxy k ix) = FractionMatrixPtr Proxy (f k) ix
  {-# INLINE fmap #-}

instance (FromFractionMatrixPtr ty k, Pretty k, Pretty (FractionMatrixPtrObject ty)) => Pretty (FractionMatrixPtr ty k) where
  pretty ptr@(FractionMatrixPtr _proxy k _ix) = pretty k <> Text.PrettyPrint.Leijen.Text.comma <> pretty (fromFractionMatrixPtr ptr)

-- | /O(log n)/.
-- @runFractionMatrixPtr ptr dict@ is the result of dereferencing @ptr@ with
-- respect to @dict@.
runFractionMatrixPtr
  :: (MonadError (FractionMatrixDictError ty i k) m, AsMatrix (FractionMatrix ty), Ord i, Ord k, Container Vector a, Num a)
  => FractionMatrixPtr ty k
  -> FractionMatrixDict ty i k a
  -> m (RallT ((->) i) a)
runFractionMatrixPtr ptr@(FractionMatrixPtr proxy@Proxy k0 ix0@(row0, col0)) (FractionMatrixDict Proxy m mm') = do
  let l = Control.Lens.view (_FractionMatrix proxy)
  case Data.Map.Strict.lookup k0 m of
    Nothing -> do
      Control.Monad.Except.throwError (FractionMatrixNotFound ptr)
    Just t -> do
      let matrix = l t
      if (row0 >= 0) && (row0 < Numeric.LinearAlgebra.HMatrix.rows matrix) && (col0 >= 0) && (col0 < Numeric.LinearAlgebra.HMatrix.cols matrix)
        then do
          let x = matrix `Numeric.LinearAlgebra.HMatrix.atIndex` ix0
          xs <- flip Data.Map.Strict.traverseWithKey mm' $ \k m' -> do
            case Data.Map.Strict.lookup k0 m' of
              Nothing -> do
                Control.Monad.Except.throwError (FractionMatrixSensitivityNotFound ptr k)
              Just t' -> do
                let matrix' = l t'
                if (row0 >= 0) && (row0 < Numeric.LinearAlgebra.HMatrix.rows matrix') && (col0 >= 0) && (col0 < Numeric.LinearAlgebra.HMatrix.cols matrix')
                  then do
                    return (matrix' `Numeric.LinearAlgebra.HMatrix.atIndex` ix0)
                  else do
                    Control.Monad.Except.throwError (FractionMatrixSensitivityIndexOutOfBounds ptr k)
          let w = RallT x (\ix -> Data.Map.Strict.findWithDefault 0 ix xs)
          return w
        else do
          Control.Monad.Except.throwError (FractionMatrixIndexOutOfBounds ptr)

-- | Type family that asserts the \"object\" representation for each
-- 'FractionType'.
type family FractionMatrixPtrObject (ty :: FractionType) :: * where
  FractionMatrixPtrObject 'IsotopomerFractionTy = (Isotopomer, Isotopomer)
  FractionMatrixPtrObject 'MassFractionTy = (MassFraction, MassFraction)

-- | The 'FromFractionMatrixPtr' is used to convert a 'FractionMatrixPtr' to
-- a 'FractionMatrixPtrObject' for the specified 'FractionType'.
class FromFractionMatrixPtr (ty :: FractionType) k where
  -- | Convert a 'FractionMatrixPtr' to a 'FractionMatrixPtrObject'.
  fromFractionMatrixPtr :: FractionMatrixPtr ty k -> FractionMatrixPtrObject ty

-- | Free 'Functor' for lookup in a 'FractionMatrixDict'.
data LookupFractionMatrixF (ty :: FractionType) k a
  = LookupFractionMatrix (FractionMatrixPtr ty k)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Eq k) => Eq1 (LookupFractionMatrixF 'IsotopomerFractionTy k) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord k) => Ord1 (LookupFractionMatrixF 'IsotopomerFractionTy k) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read k) => Read1 (LookupFractionMatrixF 'IsotopomerFractionTy k) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show k) => Show1 (LookupFractionMatrixF 'IsotopomerFractionTy k) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance (Eq k) => Eq1 (LookupFractionMatrixF 'MassFractionTy k) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord k) => Ord1 (LookupFractionMatrixF 'MassFractionTy k) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read k) => Read1 (LookupFractionMatrixF 'MassFractionTy k) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show k) => Show1 (LookupFractionMatrixF 'MassFractionTy k) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance Bifunctor (LookupFractionMatrixF ty) where
  bimap f _ (LookupFractionMatrix ptr) = LookupFractionMatrix (fmap f ptr)
  {-# INLINE bimap #-}

instance (FromFractionMatrixPtr ty k, Pretty k, Pretty (FractionMatrixPtrObject ty)) => Pretty (LookupFractionMatrixF ty k a) where
  pretty (LookupFractionMatrix ptr) = pretty ptr

-- | Lookup the value of a key in a isotopomer-specific 'FractionMatrixDict'.
lookupIsotopomerFractionMatrix :: (LookupFractionMatrixF 'IsotopomerFractionTy k :<: f) => k -> IndexOf Matrix -> Fix f
lookupIsotopomerFractionMatrix k ix = Data.Injective.inject (LookupFractionMatrix (FractionMatrixPtr (Proxy :: Proxy 'IsotopomerFractionTy) k ix))

-- | Lookup the value of a key in a mass-specific 'FractionMatrixDict'.
lookupMassFractionMatrix :: (LookupFractionMatrixF 'MassFractionTy k :<: f) => k -> IndexOf Matrix -> Fix f
lookupMassFractionMatrix k ix = Data.Injective.inject (LookupFractionMatrix (FractionMatrixPtr (Proxy :: Proxy 'MassFractionTy) k ix))
