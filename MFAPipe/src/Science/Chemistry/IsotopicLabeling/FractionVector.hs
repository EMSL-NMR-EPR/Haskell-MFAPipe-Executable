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
-- Module      :  Science.Chemistry.IsotopicLabeling.FractionVector
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation, construction
-- and dereference of fraction-specific vectors.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.FractionVector
( -- * FractionVector type family
  FractionVector
, _FractionVector
  -- * FractionVectorDict type
, FractionVectorDict(..)
, emptyFractionVectorDict
, unionFractionVectorDict , unionsFractionVectorDict
, lookupFractionVectorDict , lookupFractionVectorDict'
  -- * FractionVectorDictError type
, FractionVectorDictError(..)
  -- * FractionVectorPtr type
, FractionVectorPtr(..)
, runFractionVectorPtr
  -- * FractionVectorPtrObject type family
, FractionVectorPtrObject
  -- * FromFractionVectorPtr class
, FromFractionVectorPtr(..)
  -- * LookupFractionVectorF type
, LookupFractionVectorF(..)
, lookupIsotopomerFractionVector , lookupMassFractionVector
) where

import           Control.Comonad.Trans.Rall (RallT(..))
import           Control.Lens (Iso)
import qualified Control.Lens
import           Control.Monad (foldM)
import           Control.Monad.Except (MonadError())
import qualified Control.Monad.Except
import           Data.Bifunctor (Bifunctor(bimap))
import           Data.Functor.Classes (Eq1(..), Ord1(..), Read1(..), Show1(..))
import           Data.Injective ((:<:)(), Fix)
import qualified Data.Injective
import qualified Data.List.Extras.Zipper
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
-- import           Data.Monoid (Product(..), Sum(..))
import           Data.Proxy (Proxy(..))
import           Numeric.LinearAlgebra.HMatrix (Container(), IndexOf, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Lens (AsVector(_Vector))
import           Science.Chemistry.EMU.Factorized.Class
import           Science.Chemistry.IsotopicLabeling.DSL.Display.Class
import           Science.Chemistry.IsotopicLabeling.DSL.FromDict.Class
import           Science.Chemistry.IsotopicLabeling.FractionType
import           Science.Chemistry.IsotopicLabeling.FractionTypeDict (HasFractionTypeDict(fractionTypeDictAlg))
import qualified Science.Chemistry.IsotopicLabeling.FractionTypeDict
import           Science.Chemistry.IsotopicLabeling.Isotopomer
import           Science.Chemistry.IsotopicLabeling.IsotopomerFractionVector
import           Science.Chemistry.IsotopicLabeling.MassFraction
import           Science.Chemistry.IsotopicLabeling.MassFractionVector
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<>))
import qualified Text.PrettyPrint.Leijen.Text

-- | Type family that asserts the fraction-specific 'Vector' type for each
-- 'FractionType'.
type family FractionVector (ty :: FractionType) :: * -> * where
  FractionVector 'IsotopomerFractionTy = IsotopomerFractionVector
  FractionVector 'MassFractionTy = MassFractionVector
  -- FractionVector 'ProductTy = Product
  -- FractionVector 'SumTy = Sum

-- instance AsVector Product where
--   _Vector = Control.Lens.iso getProduct Product . Control.Lens.iso Numeric.LinearAlgebra.HMatrix.scalar Numeric.LinearAlgebra.HMatrix.prodElements
--
-- instance AsVector Sum where
--   _Vector = Control.Lens.iso getSum Sum . Control.Lens.iso Numeric.LinearAlgebra.HMatrix.scalar Numeric.LinearAlgebra.HMatrix.sumElements

-- | An 'Iso' between fraction-specific 'Vector' and 'Vector'.
_FractionVector :: (AsVector (FractionVector ty), Container Vector a, Container Vector b) => Proxy ty -> Iso (FractionVector ty a) (FractionVector ty b) (Vector a) (Vector b)
_FractionVector Proxy = _Vector

-- | A dictionary of instances of a fraction-specific 'Vector' type.
data FractionVectorDict (ty :: FractionType) i k a = FractionVectorDict (Proxy ty) (Map k (FractionVector ty a)) (Map i (Map k (FractionVector ty a)))

deriving instance (Eq i, Eq k, Eq (FractionVector ty a)) => Eq (FractionVectorDict ty i k a)
deriving instance (Ord i, Ord k, Ord (FractionVector ty a)) => Ord (FractionVectorDict ty i k a)
deriving instance (Ord i, Ord k, Read i, Read k, Read (FractionVector ty a)) => Read (FractionVectorDict ty i k a)
deriving instance (Show i, Show k, Show (FractionVector ty a)) => Show (FractionVectorDict ty i k a)

-- | An error that is thrown when a 'FractionVectorPtr' is dereferenced.
data FractionVectorDictError (ty :: FractionType) i k
  = FractionVectorNotFound (FractionVectorPtr ty k)
  | FractionVectorIndexOutOfBounds (FractionVectorPtr ty k)
  | FractionVectorSensitivityNotFound (FractionVectorPtr ty k) i
  | FractionVectorSensitivityIndexOutOfBounds (FractionVectorPtr ty k) i
  | DuplicateFractionVector (FractionVectorPtr ty k)
  | DuplicateFractionVectorSensitivity (FractionVectorPtr ty k) i
  deriving (Eq, Ord, Read, Show)

emptyFractionVectorDict :: Proxy ty -> FractionVectorDict ty i k a
emptyFractionVectorDict proxy = FractionVectorDict proxy Data.Map.Strict.empty Data.Map.Strict.empty

unionFractionVectorDict :: (MonadError (FractionVectorDictError ty i k) m, Ord i, Ord k) => FractionVectorDict ty i k a -> FractionVectorDict ty i k a -> m (FractionVectorDict ty i k a)
unionFractionVectorDict (FractionVectorDict proxy kMapL ikMapL) (FractionVectorDict Proxy kMapR ikMapR) = do
  let
    insertM k x m
      | Data.Map.Strict.member k m = do
          Control.Monad.Except.throwError (DuplicateFractionVector (FractionVectorPtr proxy k (-1)))
      | otherwise = do
          return (Data.Map.Strict.insert k x m)
    insertM' i k x m = case Data.Map.Strict.lookup i m of
      Nothing -> do
          return (Data.Map.Strict.insert i (Data.Map.Strict.singleton k x) m)
      Just m'
        | Data.Map.Strict.member k m' -> do
            Control.Monad.Except.throwError (DuplicateFractionVectorSensitivity (FractionVectorPtr proxy k (-1)) i)
        | otherwise -> do
            return (Data.Map.Strict.alter (Just . maybe (Data.Map.Strict.singleton k x) (Data.Map.Strict.insert k x)) i m)
  kMap <- foldM (\acc (k, x) -> insertM k x acc) kMapL (Data.Map.Strict.toAscList kMapR)
  ikMap <- foldM (\acc (i, kMap') -> foldM (\acc' (k, x) -> insertM' i k x acc') acc (Data.Map.Strict.toAscList kMap')) ikMapL (Data.Map.Strict.toAscList ikMapR)
  return (FractionVectorDict proxy kMap ikMap)

unionsFractionVectorDict :: (MonadError (FractionVectorDictError ty i k) m, Ord i, Ord k) => [FractionVectorDict ty i k a] -> m (FractionVectorDict ty i k a)
unionsFractionVectorDict = foldM unionFractionVectorDict (emptyFractionVectorDict Proxy)

lookupFractionVectorDict
  :: (MonadError (FractionVectorDictError ty i k) m, Ord k, Factorized k, Monoid (FractionVector ty e))
  => k
  -> FractionVectorDict ty i k e
  -> m (FractionVector ty e)
lookupFractionVectorDict k0 (FractionVectorDict proxy kMap _) = mconcat <$> mapM f (factors k0)
  where
    f k = maybe (Control.Monad.Except.throwError (FractionVectorNotFound (FractionVectorPtr proxy k (-1)))) return (Data.Map.Strict.lookup k kMap)

lookupFractionVectorDict'
  :: (MonadError (FractionVectorDictError ty i k) m, Ord i, Ord k, Factorized k, Monoid (FractionVector ty e))
  => i
  -> k
  -> FractionVectorDict ty i k e
  -> m [FractionVector ty e]
lookupFractionVectorDict' i k0 (FractionVectorDict proxy kMap ikMap) = mapM f (Data.List.Extras.Zipper.zipper (factors k0))
  where
    f (ksL, k, ksR) = do
      new_ksL <- mapM (\kL -> maybe (Control.Monad.Except.throwError (FractionVectorNotFound (FractionVectorPtr proxy kL (-1)))) return (Data.Map.Strict.lookup kL kMap)) ksL
      new_k <- maybe (Control.Monad.Except.throwError (FractionVectorSensitivityNotFound (FractionVectorPtr proxy k (-1)) i)) return (Data.Map.Strict.lookup i ikMap >>= Data.Map.Strict.lookup k)
      new_ksR <- mapM (\kR -> maybe (Control.Monad.Except.throwError (FractionVectorNotFound (FractionVectorPtr proxy kR (-1)))) return (Data.Map.Strict.lookup kR kMap)) ksR
      return (mconcat new_ksL `mappend` new_k `mappend` mconcat new_ksR)

-- | A pointer to a fraction-specific 'Vector' in a 'FractionVectorDict'.
data FractionVectorPtr (ty :: FractionType) k = FractionVectorPtr (Proxy ty) k (IndexOf Vector)
  deriving (Eq, Ord, Read, Show)

instance Functor (FractionVectorPtr ty) where
  fmap f (FractionVectorPtr Proxy k ix) = FractionVectorPtr Proxy (f k) ix
  {-# INLINE fmap #-}

instance (FromFractionVectorPtr ty k, Pretty k, Pretty (FractionVectorPtrObject ty)) => Pretty (FractionVectorPtr ty k) where
  pretty ptr@(FractionVectorPtr _proxy k _ix) = pretty k <> Text.PrettyPrint.Leijen.Text.comma <> pretty (fromFractionVectorPtr ptr)

-- | /O(log n)./
-- @runFractionVectorPtr ptr dict@ is the result of dereferencing @ptr@ with
-- respect to @dict@.
runFractionVectorPtr
  :: (MonadError (FractionVectorDictError ty i k) m, AsVector (FractionVector ty), Ord i, Ord k, Container Vector a, Num a)
  => FractionVectorPtr ty k
  -> FractionVectorDict ty i k a
  -> m (RallT ((->) i) a)
runFractionVectorPtr ptr@(FractionVectorPtr proxy@Proxy k0 ix0) (FractionVectorDict Proxy m mm') = do
  let l = Control.Lens.view (_FractionVector proxy)
  case Data.Map.Strict.lookup k0 m of
    Nothing -> do
      Control.Monad.Except.throwError (FractionVectorNotFound ptr)
    Just t -> do
      let vector = l t
      if (ix0 >= 0) && (ix0 < Numeric.LinearAlgebra.HMatrix.size vector)
        then do
          let x = vector `Numeric.LinearAlgebra.HMatrix.atIndex` ix0
          xs <- flip Data.Map.Strict.traverseWithKey mm' $ \k m' -> do
            case Data.Map.Strict.lookup k0 m' of
              Nothing -> do
                Control.Monad.Except.throwError (FractionVectorSensitivityNotFound ptr k)
              Just t' -> do
                let vector' = l t'
                if (ix0 >= 0) && (ix0 < Numeric.LinearAlgebra.HMatrix.size vector')
                  then do
                    return (vector' `Numeric.LinearAlgebra.HMatrix.atIndex` ix0)
                  else do
                    Control.Monad.Except.throwError (FractionVectorSensitivityIndexOutOfBounds ptr k)
          let w = RallT x (\ix -> Data.Map.Strict.findWithDefault 0 ix xs)
          return w
        else do
          Control.Monad.Except.throwError (FractionVectorIndexOutOfBounds ptr)

-- | Type family that asserts the \"object\" representation for each
-- 'FractionType'.
type family FractionVectorPtrObject (ty :: FractionType) :: * where
  FractionVectorPtrObject 'IsotopomerFractionTy = Isotopomer
  FractionVectorPtrObject 'MassFractionTy = MassFraction

-- | The 'FromFractionVectorPtr' is used to convert a 'FractionVectorPtr' to
-- a 'FractionVectorPtrObject' for the specified 'FractionType'.
class FromFractionVectorPtr (ty :: FractionType) k where
  -- | Convert a 'FractionVectorPtr' to a 'FractionVectorPtrObject'.
  fromFractionVectorPtr :: FractionVectorPtr ty k -> FractionVectorPtrObject ty

-- | Free 'Functor' for lookup in a 'FractionVectorDict'.
data LookupFractionVectorF (ty :: FractionType) k a
  = LookupFractionVector (FractionVectorPtr ty k)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Eq k) => Eq1 (LookupFractionVectorF 'IsotopomerFractionTy k) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord k) => Ord1 (LookupFractionVectorF 'IsotopomerFractionTy k) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read k) => Read1 (LookupFractionVectorF 'IsotopomerFractionTy k) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show k) => Show1 (LookupFractionVectorF 'IsotopomerFractionTy k) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance (Eq k) => Eq1 (LookupFractionVectorF 'MassFractionTy k) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord k) => Ord1 (LookupFractionVectorF 'MassFractionTy k) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read k) => Read1 (LookupFractionVectorF 'MassFractionTy k) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show k) => Show1 (LookupFractionVectorF 'MassFractionTy k) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance Bifunctor (LookupFractionVectorF ty) where
  bimap f _ (LookupFractionVector ptr) = LookupFractionVector (fmap f ptr)
  {-# INLINE bimap #-}

instance (FromFractionVectorPtr ty k, Pretty k, Pretty (FractionVectorPtrObject ty)) => Pretty (LookupFractionVectorF ty k a) where
  pretty (LookupFractionVector ptr) = pretty ptr

-- | Lookup the value of a key in a isotopomer-specific 'FractionVectorDict'.
lookupIsotopomerFractionVector :: (LookupFractionVectorF 'IsotopomerFractionTy k :<: f) => k -> IndexOf Vector -> Fix f
lookupIsotopomerFractionVector k ix = Data.Injective.inject (LookupFractionVector (FractionVectorPtr (Proxy :: Proxy 'IsotopomerFractionTy) k ix))

-- | Lookup the value of a key in a mass-specific 'FractionVectorDict'.
lookupMassFractionVector :: (LookupFractionVectorF 'MassFractionTy k :<: f) => k -> IndexOf Vector -> Fix f
lookupMassFractionVector k ix = Data.Injective.inject (LookupFractionVector (FractionVectorPtr (Proxy :: Proxy 'MassFractionTy) k ix))

instance (FromFractionVectorPtr ty k, Pretty k, Pretty (FractionVectorPtrObject ty)) => Display (LookupFractionVectorF ty k) where
  displayAlg = pretty

instance (AsVector (FractionVector ty), Ord i, Ord k, Container Vector e, Num (Vector e), Num e) => FromDict i e (LookupFractionVectorF ty k) where
  type Dict (LookupFractionVectorF ty k) i = FractionVectorDict ty i k
  type DictError (LookupFractionVectorF ty k) i = FractionVectorDictError ty i k
  fromDictAlg dict (LookupFractionVector ptr) = runFractionVectorPtr ptr dict

-- instance (Ord k) => HasFractionTypeDict k (IndexOf Vector) (LookupFractionVectorF 'IsotopomerFractionTy k) where
instance (Ord k) => HasFractionTypeDict k Int (LookupFractionVectorF 'IsotopomerFractionTy k) where
  fractionTypeDictAlg (LookupFractionVector (FractionVectorPtr Proxy k ix)) = Science.Chemistry.IsotopicLabeling.FractionTypeDict.singleton IsotopomerFractionTy k ix 1

-- instance (Ord k) => HasFractionTypeDict k (IndexOf Vector) (LookupFractionVectorF 'MassFractionTy k) where
instance (Ord k) => HasFractionTypeDict k Int (LookupFractionVectorF 'MassFractionTy k) where
  fractionTypeDictAlg (LookupFractionVector (FractionVectorPtr Proxy k ix)) = Science.Chemistry.IsotopicLabeling.FractionTypeDict.singleton MassFractionTy k ix 1
