{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.Stoichiometry
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports classes and types for the construction of sparse and
-- dense stoichiometric models, including stoichiometry matrices.
-----------------------------------------------------------------------------

module Science.Chemistry.Stoichiometry
( -- * HasStoichiometry class
  HasStoichiometry(..)
  -- * MutableStoichiometricModel class
, MutableStoichiometricModel(..)
, modifyStoichiometricModel
  -- * Stoichiometric models
, Sparse(..)
, Dense(..)
  -- ** Conversion
, sparseToDense
  -- * Utility functions
, mapAccumStoichiometricModelM , mapAccumStoichiometricModel
) where

import           Control.Applicative (liftA2, liftA3)
import qualified Control.Lens
import           Control.Monad.Reader.Class (MonadReader())
import qualified Control.Monad.Reader.Class
import           Control.Monad.State.Class (MonadState())
import qualified Control.Monad.State.Class
import qualified Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as Control.Monad.Trans.State
import           Data.Function (on)
import qualified Data.Functor.Identity
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Maybe (catMaybes)
import           Data.Monoid (Sum(..))
import           Data.Set (Set)
import qualified Data.Set
import           Numeric.LinearAlgebra.HMatrix (Container(), Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.Types

-- | The 'HasStoichiometry' class is used for types that have stoichiometry, where:
--
-- * @a@ is the type of chemical species; and,
--
-- * @r@ is the type of the environment.
--
class (Ord a) => HasStoichiometry a r | r -> a where
  {-# MINIMAL assertStoichiometryM #-}

  -- | @assertStoichiometryM f g r@ is a computation that asserts the stoichiometry of @r@, given the functions @f@ and @g@, in the context of an arbitrary 'Monad'.
  assertStoichiometryM
    :: (Monad m)
    => (a -> StoichiometricCoefficient -> m ())
    -- ^ Assert chemical species with 'StoichiometricCoefficient'.
    -> (Either a a -> m ())
    -- ^ Assert chemical species, where 'Left' and 'Right' denote,
    -- respectively, intermediates and reagents/products.
    -> r
    -- ^ Environment
    -> m ()

-- | The 'MutableStoichiometricModel' class is used for types that are mutable stoichiometric models, where:
--
-- * @i@ is the type of chemical reaction indices;
--
-- * @a@ is the type of chemical species; and,
--
-- * @s@ is the type of the state of the stoichiometric model.
--
class (Ord i, Ord a) => MutableStoichiometricModel i a s | s -> i, s -> a where
  {-# MINIMAL modifyStoichiometricModelM #-}

  -- | @modifyStoichiometricModelM i@ is a computation that modifies the current state of a stoichiometric model.
  modifyStoichiometricModelM :: (MonadReader r m, MonadState s m, HasStoichiometry a r) => i -> m ()

-- | @modifyStoichiometricModel i r s@ is a computation that modifies the current state of the stoichiometric model @s@, given the environment @r@.
modifyStoichiometricModel :: (HasStoichiometry a r, MutableStoichiometricModel i a s) => i -> r -> s -> s
modifyStoichiometricModel ix r = Data.Functor.Identity.runIdentity . Control.Monad.Trans.State.execStateT (Control.Monad.Trans.Reader.runReaderT (modifyStoichiometricModelM ix) r)

-- | The 'mapAccumStoichiometricModelM' applies 'modifyStoichiometricModelM' to each element of a structure, passing the accumulated stoichiometric model from left to right.
mapAccumStoichiometricModelM :: (Foldable f, Monad m, HasStoichiometry a r, MutableStoichiometricModel i a s) => f (i, r) -> s -> m s
mapAccumStoichiometricModelM = Control.Monad.Trans.State.execStateT . mapM_ (uncurry (\ix r -> Control.Monad.Trans.Reader.runReaderT (modifyStoichiometricModelM ix) r))

-- | The 'mapAccumStoichiometricModel' applies 'modifyStoichiometricModel' to each element of a structure, passing the accumulated stoichiometric model from left to right.
mapAccumStoichiometricModel :: (Foldable f, HasStoichiometry a r, MutableStoichiometricModel i a s) => f (i, r) -> s -> s
mapAccumStoichiometricModel = (Data.Functor.Identity.runIdentity .) . mapAccumStoichiometricModelM

-- | A stoichiometric model; a sparse matrix of stoichiometric coefficients by chemical reaction and species indices, along with sets of intermediate and reagent/product indices, where:
--
-- * @i@ is the type of chemical reaction indices.
--
-- * @a@ is the type of chemical species.
--
data Sparse i a = Sparse
  { _sparseStoichiometry  :: Map i (Map a StoichiometricCoefficient)
  , _sparseIntermediate   :: Set a
  , _sparseReagentProduct :: Set a
  } deriving (Eq, Ord, Read, Show)

Control.Lens.makeLenses ''Sparse

instance (Ord i, Ord a) => Monoid (Sparse i a) where
  mempty = Sparse
    { _sparseStoichiometry = Data.Map.Strict.empty
    , _sparseIntermediate = Data.Set.empty
    , _sparseReagentProduct = Data.Set.empty
    }
  {-# INLINE mempty #-}
  mL `mappend` mR = Sparse
    { _sparseStoichiometry = Data.Map.Strict.unionWith (Data.Map.Strict.unionWith mappend) (_sparseStoichiometry mL) (_sparseStoichiometry mR)
    , _sparseIntermediate = Data.Set.union (_sparseIntermediate mL) (_sparseIntermediate mR)
    , _sparseReagentProduct = Data.Set.union (_sparseReagentProduct mL) (_sparseReagentProduct mR)
    }
  {-# INLINE mappend #-}

instance (Ord i, Ord a) => MutableStoichiometricModel i a (Sparse i a) where
  modifyStoichiometricModelM ix0 = Control.Monad.Reader.Class.ask >>= assertStoichiometryM (\k x -> Control.Monad.State.Class.modify (assertStoichiometricCoefficient ix0 k x)) (\e -> Control.Monad.State.Class.modify (assertChemicalSpecies e))
    where
      assertStoichiometricCoefficient :: (Ord i, Ord a) => i -> a -> StoichiometricCoefficient -> Sparse i a -> Sparse i a
      assertStoichiometricCoefficient ix k x = Control.Lens.over (sparseStoichiometry . Control.Lens.at ix) (Just . Control.Lens.over (Control.Lens.at k) (Just . mappend x . maybe mempty id) . maybe Data.Map.Strict.empty id)
      assertChemicalSpecies :: (Ord a) => Either a a -> Sparse i a -> Sparse i a
      assertChemicalSpecies = either (Control.Lens.over sparseIntermediate . Data.Set.insert) (Control.Lens.over sparseReagentProduct . Data.Set.insert)

-- | A stoichiometric model; a pair of dense matrices of stoichiometric coefficients by chemical reaction index and chemical species, where:
--
-- * @i@ is the type of chemical reaction indices.
--
-- * @a@ is the type of chemical species.
--
-- * @e@ is the type of matrix elements.
--
data Dense i a e = Dense
  { _denseReactionIndices :: Set i
  , _denseIntermediate    :: (Set a, Matrix e)
  -- ^ Dimension @a><i@, where @a@ is the number of chemical species, and @i@ is the number of chemical reaction indices.
  , _denseReagentProduct  :: (Set a, Matrix e)
  -- ^ Dimension @a><i@, where @a@ is the number of chemical species, and @i@ is the number of chemical reaction indices.
  } deriving (Read, Show)

deriving instance (Eq i, Eq a, Container Vector e, Num e) => Eq (Dense i a e)

-- Control.Lens.makeLenses ''Dense

-- | Converts a 'Sparse' stoichiometric model to a 'Dense' stoichiometric model.
sparseToDense :: (Ord i, Ord a, Container Vector e, Fractional e) => Sparse i a -> Dense i a e
sparseToDense = liftA3 (\mm -> Dense (Data.Map.Strict.keysSet mm) `on` liftA2 (,) id (mkMatrix mm)) _sparseStoichiometry _sparseIntermediate _sparseReagentProduct
  where
    -- | The 'mkMatrix' function constructs a matrix for the specified chemical species indices.
    mkMatrix :: (Ord i, Ord a, Container Vector e, Fractional e) => Map i (Map a StoichiometricCoefficient) -> Set a -> Matrix e
    -- mkMatrix mm ks = (Data.Set.size ks >< Data.Map.Strict.size mm) $ do
    --   k <- Data.Set.toAscList ks
    --   m <- Data.Map.Strict.elems mm
    --   let
    --     x = Data.Map.Strict.findWithDefault mempty k m
    --     x' = fromRational (getSum (getStoichiometricCoefficient x))
    --   return x'
    mkMatrix mm ks = Numeric.LinearAlgebra.HMatrix.assoc (Data.Set.size ks, Data.Map.Strict.size mm) 0 $ catMaybes $ do
      ~(i, k) <- zip (enumFromThen 0 1) (Data.Set.toAscList ks)
      ~(j, m) <- zip (enumFromThen 0 1) (Data.Map.Strict.elems mm)
      return (fmap (\(StoichiometricCoefficient (Sum x)) -> ((i, j), fromRational x)) (Data.Map.Strict.lookup k m))
