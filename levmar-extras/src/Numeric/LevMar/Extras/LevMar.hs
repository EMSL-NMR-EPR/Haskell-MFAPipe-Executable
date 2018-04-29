{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LevMar.Extras.LevMar
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports a representation of a minimization using the
-- Levenberg-Marquardt algorithm.
--
-- See also
--
--  * \"levmar : Levenberg-Marquardt nonlinear least squares algorithms in C/C++\",
--    by M. Lourakis, online at
--    <http://users.ics.forth.gr/~lourakis/levmar/>.
-----------------------------------------------------------------------------

module Numeric.LevMar.Extras.LevMar
( -- * LevMar type
  LevMar(..)
, levmar
, runLevMar
  -- * Unsafe IO operations
, unsafeWithLevMarIO
  -- * Utilities
  -- ** Box constraints
, boxConstraints
  -- ** Fixed parameters
, fixParams
  -- ** Weighting
, weightedWith , weighted
) where

import           Control.Applicative (liftA2)
import           Control.Lens (Iso', _1, _3)
import qualified Control.Lens
import qualified Data.List.Extras.At
import           Data.Profunctor (dimap, lmap)
import qualified Data.Set
import           Foreign.Storable (Storable())
import           Numeric.LevMar (LevMarable(), Constraints(..), Info, Jacobian, LevMarError, Model, Options, Params, Samples)
import qualified Numeric.LevMar
import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), Numeric(), IndexOf, Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Lens (_Columns, _List, _Rows)
import           System.IO.Unsafe (unsafePerformIO)

-- | A minimization using the Levenberg-Marquardt algorithm.
data LevMar r a = LevMar
  ((Params r, Info r, Matrix r) -> a)
  (Model r)
  (Maybe (Jacobian r))
  (Params r)
  (Samples r)
  (Constraints r)

instance Functor (LevMar r) where
  fmap f (LevMar done model mJac ps ys constraints) = LevMar (f . done) model mJac ps ys constraints
  {-# INLINE fmap #-}

levmar :: Model r -> Maybe (Jacobian r) -> Params r -> Samples r -> Constraints r -> LevMar r (Params r, Info r, Matrix r)
levmar = LevMar id
{-# INLINE levmar #-}

runLevMar :: (LevMarable r) => LevMar r a -> Int -> Options r -> Either LevMarError a
runLevMar (LevMar done model mJac ps ys constraints) itMax opts = fmap done (Numeric.LevMar.levmar model mJac ps ys itMax opts constraints)
{-# INLINE runLevMar #-}

unsafeWithLevMarIO
  :: (Params r -> IO s)
  -> (s -> Model r -> Params r -> IO (Samples r))
  -> (s -> Jacobian r -> Params r -> IO (Matrix r))
  -> LevMar r a
  -> IO (s, LevMar r a)
unsafeWithLevMarIO newSt withModelSt withJacobianSt (LevMar done model mJacobian ps0 ys0 constraints) = do
  s0 <- newSt ps0
  let
    new_model = unsafePerformIO . withModelSt s0 model
    new_mJacobian = fmap (\jacobian -> unsafePerformIO . withJacobianSt s0 jacobian) mJacobian
  return (s0, LevMar done new_model new_mJacobian ps0 ys0 constraints)

boxConstraints :: (Show r, Container Vector r, Numeric r, Ord r) => (r, r) -> r -> Matrix r -> LevMar r a -> LevMar r a
boxConstraints ~(lo, hi) w nullspace0 (LevMar done model mJac ps0 ys0 (Constraints lBs uBs ws linFunc)) =
  let
    nullspace = Numeric.LinearAlgebra.HMatrix.fromRows (Data.Set.toAscList (Data.Set.fromList (Numeric.LinearAlgebra.HMatrix.toRows nullspace0)))
    ~(rows, cols) = Numeric.LinearAlgebra.HMatrix.size nullspace
    f = Numeric.LinearAlgebra.HMatrix.subVector 0 cols
    g = Numeric.LinearAlgebra.HMatrix.subMatrix (0, 0) (Numeric.LinearAlgebra.HMatrix.size ps0, Numeric.LinearAlgebra.HMatrix.size ps0)
    new_done = done . (\ ~(ps, info, covar_ps) -> (f ps, info, g covar_ps))
    new_model = model . f
    new_mJac = fmap (. f) mJac
    new_ps0 = Numeric.LinearAlgebra.HMatrix.vjoin [ps0, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows 0)]
    new_ys0 = ys0
    new_lBs = case lBs of
      Nothing -> Just (Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows lo)])
        where
          vector = Numeric.LinearAlgebra.HMatrix.fromList (replicate (Numeric.LinearAlgebra.HMatrix.size ps0) lo)
      Just vector -> Just (Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows lo)])
    new_uBs = case uBs of
      Nothing -> Just (Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows hi)])
        where
          vector = Numeric.LinearAlgebra.HMatrix.fromList (replicate (Numeric.LinearAlgebra.HMatrix.size ps0) hi)
      Just vector -> Just (Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows hi)])
    new_ws = fmap (\vector -> Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.fromList (replicate rows w)]) ws
    new_linFunc = case linFunc of
      Nothing -> Just (Numeric.LinearAlgebra.HMatrix.fromBlocks [[Numeric.LinearAlgebra.HMatrix.scale (-1) nullspace, Numeric.LinearAlgebra.HMatrix.ident rows]], Numeric.LinearAlgebra.HMatrix.konst 0 rows)
      Just ~(matrix, vector) -> Just (Numeric.LinearAlgebra.HMatrix.fromBlocks [[matrix, Numeric.LinearAlgebra.HMatrix.konst 0 (Numeric.LinearAlgebra.HMatrix.rows matrix, rows)], [Numeric.LinearAlgebra.HMatrix.scale (-1) nullspace, Numeric.LinearAlgebra.HMatrix.ident rows]], Numeric.LinearAlgebra.HMatrix.vjoin [vector, Numeric.LinearAlgebra.HMatrix.konst 0 rows])
  in
    LevMar new_done new_model new_mJac new_ps0 new_ys0 (Constraints new_lBs new_uBs new_ws new_linFunc)

-- | @fixParams rs levmar@ fixes the 'Params' in @levmar@ with respect to @rs@, a list of index/value pairs.
fixParams :: (Numeric r, Num (Vector r), Num r) => [(IndexOf Vector, r)] -> LevMar r a -> LevMar r a
fixParams [] x0 = x0
fixParams rs0 (LevMar done model mJac ps ys (Constraints lBs uBs ws linFunc)) =
  let
    (indices, values) = unzip rs0
    new_done = lmap (Control.Lens.over _1 (insertAtParams rs0) . Control.Lens.over _3 (insertAtColumnsAndRows (Numeric.LinearAlgebra.HMatrix.size ps) indices)) done
    new_model = lmap (insertAtParams rs0) model
    new_mJac = fmap (dimap (insertAtParams rs0) (deleteAtColumns indices)) mJac
    new_ps = deleteAtParams indices ps
    new_lBs = fmap (deleteAtParams indices) lBs
    new_uBs = fmap (deleteAtParams indices) uBs
    new_ws = fmap (deleteAtParams indices) ws
    new_linFunc = fmap f linFunc
      where
        f (matrix0, vector0) =
          let
            (columnsAt, columnsNotAt) = Data.List.Extras.At.partitionAt indices (Control.Lens.review (Control.Lens.mapping _List . _Columns) matrix0)
            matrixAt = Control.Lens.view (Control.Lens.mapping _List . _Columns) columnsAt
            matrixNotAt = Control.Lens.view (Control.Lens.mapping _List . _Columns) columnsNotAt
            vector1 = vector0 - Numeric.LinearAlgebra.HMatrix.app matrixAt (Numeric.LinearAlgebra.HMatrix.fromList values)
          in
            (matrixNotAt, vector1)
  in
    LevMar new_done new_model new_mJac new_ps ys (Constraints new_lBs new_uBs new_ws new_linFunc)
  where
    deleteAtParams :: (Storable r) => [IndexOf Vector] -> Params r -> Params r
    deleteAtParams = Control.Lens.under _List . Data.List.Extras.At.deleteAt
    insertAtParams :: (Storable r) => [(IndexOf Vector, r)] -> Params r -> Params r
    insertAtParams = Control.Lens.under _List . Data.List.Extras.At.insertAt
    deleteAtColumns :: (Element a) => [IndexOf Vector] -> Matrix a -> Matrix a
    deleteAtColumns = Control.Lens.under (Control.Lens.mapping _List . _Columns) . Data.List.Extras.At.deleteAt
    -- insertAtColumns :: (Element a) => [(IndexOf Vector, [a])] -> Matrix a -> Matrix a
    -- insertAtColumns = Control.Lens.under (Control.Lens.mapping _List . _Columns) . Data.List.Extras.At.insertAt
    insertAtColumnsAndRows :: (Element a, Num a) => IndexOf Vector -> [IndexOf Vector] -> Matrix a -> Matrix a
    insertAtColumnsAndRows size0 indices0 = liftA2 (.) (f _Columns size0) (f _Rows (size0 - length indices0)) indices0
      where
        f :: (Element a, Num a) => Iso' [Vector a] (Matrix a) -> Int -> [IndexOf Vector] -> Matrix a -> Matrix a
        f l n = Control.Lens.under (Control.Lens.mapping _List . l) . Data.List.Extras.At.insertAt . map (flip (,) (replicate n 0))

-- | @weightedWith c ws levmar@ is the weighted 'Model' and 'Jacobian' obtained by subtracting 'Samples' from the result of 'Model' in @levmar@ (denoted @rs@), and then applying @c@ to each pair of @ws@ and @rs@.
--
-- Note: The calling convention is @c w r@.
weightedWith :: (Container Vector r, Num (Vector r), Num r) => (r -> r -> r) -> Samples r -> LevMar r a -> LevMar r a
weightedWith c ws (LevMar done model mJac ps ys constraints) = LevMar done (g . model) (fmap (h .) mJac) ps new_ys constraints
  where
    -- Applies @c@ to each pair of elements of the 'Model' and 'Sample'.
    f = zipWith c (Control.Lens.review _List ws)
    -- Subtract @ys@ and then apply @f@.
    g = Control.Lens.under _List f . subtract ys
    -- Apply @f@.
    h = Control.Lens.under (Control.Lens.mapping _List . _Columns) (map f)
    -- Zero out @ys@.
    new_ys = Numeric.LinearAlgebra.HMatrix.konst 0 (Numeric.LinearAlgebra.HMatrix.size ys)

-- | Weighting by division by sigma, i.e.,
--
-- > weighted = weightedWith (flip (/))
--
-- See also
--
-- * \"GNU Scientific Library - Reference Manual: Overview of Weighted Nonlinear Least-Squares Fitting\",
--    by M. Galassi, et al., (2009), online at
--    <http://www.gnu.org/software/gsl/manual/html_node/Overview-of-Weighted-Nonlinear-Least_002dSquares-Fitting.html>.
--
weighted :: (Container Vector r, Num (Vector r), Fractional r) => Samples r -> LevMar r a -> LevMar r a
weighted = weightedWith (flip (/))
