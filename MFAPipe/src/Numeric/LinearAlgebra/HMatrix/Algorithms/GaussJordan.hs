{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan
( Epsilon(..)
, isNear , isNearZero
, rat
, Tolerance(..)
, Operation(..) , runOperation
, rcef
, rref
, Nullspace(..)
, nullspace
) where

import           Control.Monad.State.Class (MonadState())
import qualified Control.Monad.State.Class as State
import           Control.Monad.Writer.Class (MonadWriter())
import qualified Control.Monad.Writer.Class as Writer
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import           Control.Monad.Trans.Writer.Strict (runWriter)
import           Data.Bifunctor (bimap, first)
import           Data.List (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Exts (build)
import           Numeric.LinearAlgebra.HMatrix (Container(), Normed(), IndexOf, Matrix, Vector, ℝ)
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix

class Epsilon e where
  epsilon :: e

instance Epsilon Float where
  epsilon = 1e-6
  {-# INLINE epsilon #-}

instance Epsilon Double where
  epsilon = 1e-12
  {-# INLINE epsilon #-}

-- | @isNear x@ returns 'True' if the absolute value of @x@ is within
-- the specified tolerance (i.e., @x@ is negligible). Otherwise, returns
-- 'False'.
--
-- See also: <http://hackage.haskell.org/package/linear/docs/Linear-Epsilon.html>
isNear :: (Num e, Ord e) => e -> e -> Bool
isNear tol x = abs x <= tol
{-# INLINE isNear #-}

isNearZero :: (Epsilon e, Num e, Ord e) => e -> Bool
isNearZero = isNear epsilon
{-# INLINE isNearZero #-}

toContinuedFraction :: (RealFrac e, Integral a) => e -> e -> [a]
toContinuedFraction tol x0 = unfoldr (>>= (\x -> let ~(n, f) = properFraction x in Just (n, if isNear tol f then Nothing else Just (recip f)))) (Just x0)
{-# INLINE toContinuedFraction #-}

fromContinuedFraction :: (Integral a, Fractional b) => [a] -> Maybe b
fromContinuedFraction = foldr (\x acc -> Just (maybe id ((+) . recip) acc (fromIntegral x))) Nothing
{-# INLINE fromContinuedFraction #-}

-- See also: <http://www.mathworks.com/help/matlab/ref/rat.html>
rat :: (RealFrac e) => e -> e -> Rational
-- rat tol x = maybe 0 id (fromContinuedFraction (toContinuedFraction tol x :: [Integer]))
rat tol x = maybe 0 id (fromContinuedFraction (take maxTerms (toContinuedFraction tol x :: [Integer])))
  where
    maxTerms :: Int
    maxTerms = 21
{-# INLINE rat #-}

class (Container c e, Epsilon e, Normed (c e), Num e) => Tolerance c e where
  tolerance_0 :: c e -> ℝ
  tolerance_1 :: c e -> ℝ
  tolerance_2 :: c e -> ℝ
  tolerance_Inf :: c e -> ℝ

instance (Container Vector e, Epsilon e, Normed (Vector e), Num e) => Tolerance Vector e where
  tolerance_0 vector = fromIntegral (HMatrix.size vector) * epsilon * HMatrix.norm_0 vector
  {-# INLINE tolerance_0 #-}
  tolerance_1 vector = fromIntegral (HMatrix.size vector) * epsilon * HMatrix.norm_1 vector
  {-# INLINE tolerance_1 #-}
  tolerance_2 vector = fromIntegral (HMatrix.size vector) * epsilon * HMatrix.norm_2 vector
  {-# INLINE tolerance_2 #-}
  tolerance_Inf vector = fromIntegral (HMatrix.size vector) * epsilon * HMatrix.norm_Inf vector
  {-# INLINE tolerance_Inf #-}

instance (Container Vector e, Epsilon e, Normed (Matrix e), Num e) => Tolerance Matrix e where
  tolerance_0 matrix = fromIntegral (uncurry max (HMatrix.size matrix)) * epsilon * HMatrix.norm_0 matrix
  {-# INLINE tolerance_0 #-}
  tolerance_1 matrix = fromIntegral (uncurry max (HMatrix.size matrix)) * epsilon * HMatrix.norm_1 matrix
  {-# INLINE tolerance_1 #-}
  tolerance_2 matrix = fromIntegral (uncurry max (HMatrix.size matrix)) * epsilon * HMatrix.norm_2 matrix
  {-# INLINE tolerance_2 #-}
  tolerance_Inf matrix = fromIntegral (uncurry max (HMatrix.size matrix)) * epsilon * HMatrix.norm_Inf matrix
  {-# INLINE tolerance_Inf #-}

data Operation a
  = NoOp
  -- ^ I
  | SwapOp {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  -- ^ R_{i} <-> R_{j}
  | MultiplyOp {-# UNPACK #-} !Int a
  -- ^ k.R_{i} -> R_{i} where k /= 0
  | AddOp {-# UNPACK #-} !Int {-# UNPACK #-} !Int a
  -- ^ R_{i} + k.R_{j} -> R_{i} where i /= j
  deriving (Eq, Ord, Read, Show)

instance Functor Operation where
  fmap _ NoOp = NoOp
  fmap _ (SwapOp i j) = SwapOp i j
  fmap f (MultiplyOp i x) = MultiplyOp i (f x)
  fmap f (AddOp i j x) = AddOp i j (f x)
  {-# INLINE fmap #-}

runOperation :: (Eq a, Num a) => Operation a -> [Int] -> [(IndexOf Matrix, a)]
runOperation NoOp = map f
  where
    f k = ((k, k), 1)
runOperation (SwapOp i j) = map f
  where
    f k
      | k == i = ((k, j), 1)
      | k == j = ((k, i), 1)
      | otherwise = ((k, k), 1)
runOperation (MultiplyOp i x)
  | x == 0 = error "scalar must be non-zero"
  | otherwise = map f
  where
    f k
      | k == i = ((k, k), x)
      | otherwise = ((k, k), 1)
runOperation (AddOp i j x)
  | i == j = error "indices must be distinct"
  | otherwise = (:) ((i, j), x) . runOperation NoOp
{-# INLINE runOperation #-}

-- | Reduced column echelon form (Gauss-Jordan elimination) with arbitrary
-- tolerance.
rcef :: ℝ -> Matrix ℝ -> (Matrix ℝ, [Operation ℝ])
rcef tol = first HMatrix.tr . rref tol . HMatrix.tr

-- | Reduced row echelon form (Gauss-Jordan elimination) with arbitrary
-- tolerance.
rref :: ℝ -> Matrix ℝ -> (Matrix ℝ, [Operation ℝ])
rref tol matrix0 = runWriter (evalStateT (rounding <$> elimGaussJordanM matrix0) (0, 0))
  where
    -- | @rowCount@ and @columnCount@ are, respectively, the number of rows and
    -- columns in the specified matrix.
    rowCount, columnCount :: Int
    ~(rowCount, columnCount) = HMatrix.size matrix0
    
    -- | @isEndOfRow ix@ returns 'True' if the row component of the specified
    -- @ix@ is out of bounds. Otherwise, returns 'False'.
    -- | @isEndOfColumn ix@ returns 'True' if the column component of the
    -- specified @ix@ is out of bounds. Otherwise, returns 'False'.
    -- | @isOutOfBounds ix@ returns 'True' if either the row or column
    -- component of the specified @ix@ is out of bounds. Otherwise, returns
    -- 'False'.
    isEndOfRow, isEndOfColumn, isOutOfBounds :: IndexOf Matrix -> Bool
    isEndOfRow = (rowCount <=) . fst
    isEndOfColumn = (columnCount <=) . snd
    isOutOfBounds = pure (||) <*> isEndOfRow <*> isEndOfColumn
    
    -- | @fromOperation op@ is an invertible matrix that performs an elementary
    -- row operation.
    fromOperation :: Operation ℝ -> Matrix ℝ
    fromOperation op = HMatrix.toDense (runOperation op (enumFromThenTo 0 1 (pred rowCount)))
    
    -- | @fromOperationList opList@ is an invertible matrix that performs a
    -- sequence of /zero/ or many of elementary row operations.
    fromOperationList :: [Operation ℝ] -> Matrix ℝ
    fromOperationList = foldr HMatrix.mul (HMatrix.ident rowCount) . map fromOperation
    
    -- | @elimGaussJordanM matrix@ returns /reduced/ row echelon form of the
    -- specified @matrix@ using Gauss-Jordan elimination with partial pivoting.
    --
    -- Note: Implementation uses monad transformers to record the current index
    -- within the specified @matrix@ (state) and the sequence of elementary row
    -- operations (writer).
    elimGaussJordanM :: (MonadState (IndexOf Matrix) m, MonadWriter [Operation ℝ] m) => Matrix ℝ -> m (Matrix ℝ)
    elimGaussJordanM acc = do
      outOfBounds <- State.gets isOutOfBounds
      if outOfBounds
        then do
          return acc
        else do
          ~(r, old_lead) <- State.get
          let
            old_i :: Int
            old_i = r
            old_ix :: IndexOf Matrix
            old_ix = (old_i, old_lead)
          ~(endOfColumn, (new_i, new_lead)) <- runStateT (findPivotM acc r) old_ix
          swapped_acc <- do
            if new_i == r
              then do
                return acc
              else do
                let
                  op :: Operation ℝ
                  op = SwapOp new_i r
                Writer.tell (pure op)
                return (fromOperation op `HMatrix.mul` acc)
          if endOfColumn
            then do
              return swapped_acc
            else do
              swapped_and_multiplied_acc <- do
                let
                  x :: Double
                  x = swapped_acc `HMatrix.atIndex` (r, new_lead)
                if isNear tol x
                  then do
                    return swapped_acc
                  else do
                    let
                      op :: Operation ℝ
                      op = MultiplyOp r (recip x)
                    Writer.tell (pure op)
                    return (fromOperation op `HMatrix.mul` swapped_acc)
              swapped_and_multiplied_and_summed_acc <- do
                let
                  opList :: [Operation ℝ]
                  opList = map (uncurry (\k x -> AddOp k r (negate x))) . filter ((/= r) . fst) . zip (enumFromThen 0 1) . map (`HMatrix.atIndex` new_lead) . HMatrix.toRows $ swapped_and_multiplied_acc
                Writer.tell opList
                return (fromOperationList opList `HMatrix.mul` swapped_and_multiplied_acc)
              State.put (succ r, succ new_lead)
              elimGaussJordanM swapped_and_multiplied_and_summed_acc
    
    -- | @findPivotM matrix row@ returns the index of the next pivot in the
    -- specified @matrix@ with respect to the specified @row@.
    --
    -- Note: Implementation uses monad transformers to record the current index
    -- within the specified @matrix@ (state).
    findPivotM :: (MonadState (IndexOf Matrix) m) => Matrix ℝ -> Int -> m Bool
    findPivotM acc r = loop
      where
        loop :: (MonadState (IndexOf Matrix) m) => m Bool
        loop = do
          nearZero <- State.gets (isNear tol . HMatrix.atIndex acc)
          if nearZero
            then do
              State.state ((,) () . first succ)
              endOfRow <- State.gets isEndOfRow
              if endOfRow
                then do
                  State.state ((,) () . bimap (const r) succ)
                  endOfColumn <- State.gets isEndOfColumn
                  if endOfColumn
                    then do
                      return endOfColumn
                    else do
                      loop
                else do
                  loop
            else do
              State.gets isEndOfColumn
    
    -- | @rounding matrix@ attempts to round the elements of the specified
    -- @matrix@ with respect to the specified @tol@.
    rounding :: Matrix ℝ -> Matrix ℝ
    rounding = HMatrix.cmap (\x -> fromRational (rat tol x))

findFreeColumn :: ℝ -> Vector ℝ -> Maybe (IndexOf Vector)
findFreeColumn tol vector = case build (\cons nil -> foldr (uncurry (\k x -> if isNear tol x then id else cons (k, x))) nil (zip (enumFromThen 0 1) (HMatrix.toList vector))) of
  [(k, x)]
    | isNear tol (1 - x) -> Just k
    | otherwise -> Nothing
  _ -> Nothing

findFreeColumns :: (Ord k) => [k] -> ℝ -> Matrix ℝ -> (Map k (Maybe (IndexOf Vector)), Map (Maybe (IndexOf Vector)) (Set k))
findFreeColumns ks tol = foldr (uncurry (\k iMaybe ~(fwd, bwd) -> (Map.insert k iMaybe fwd, Map.alter (Just . Set.insert k . maybe Set.empty id) iMaybe bwd))) (Map.empty, Map.empty) . zip ks . map (findFreeColumn tol) . HMatrix.toRows

-- toFree :: ℝ -> Matrix ℝ -> Matrix ℝ
-- toFree tol matrix =
--   let
--     rows :: [(IndexOf Vector, (Maybe (IndexOf Vector), Vector ℝ))]
--     rows = zip (enumFromThen 0 1) . map (findFreeColumn tol >>= (,)) . HMatrix.toRows $ matrix
--     new_matrix :: Matrix ℝ
--     new_matrix = HMatrix.assoc (HMatrix.size matrix) 0 (Map.foldrWithKey cataWithKey [] m)
--       where
--         cataWithKey :: Maybe (IndexOf Vector) -> [(IndexOf Vector, Vector ℝ)] -> AssocMatrix -> AssocMatrix
--         cataWithKey Nothing _ = id
--         cataWithKey (Just _) [] = id
--         cataWithKey (Just j) ((i, _):_) = (:) ((i, j), 1)
--         m :: Map (Maybe (IndexOf Vector)) [(IndexOf Vector, Vector ℝ)]
--         m = foldr (\(i, (jMaybe, vector)) -> Map.alter (Just . (:) (i, vector) . maybe [] id) jMaybe) Map.empty rows
--   in
--     matrix `HMatrix.mul` HMatrix.tr new_matrix
-- 
-- toLinearConstraints :: (Ord k) => [k] -> ℝ -> Matrix ℝ -> [(Map k ℝ, ℝ)]
-- toLinearConstraints ks tol matrix = map (flip (,) 0) (filter (not . Map.null) (map (foldr (uncurry (\k x -> if isNear tol x then id else Map.insert k x)) Map.empty . zip ks . HMatrix.toList) (HMatrix.toRows (toFree tol matrix - HMatrix.ident (HMatrix.rows matrix)))))

data Nullspace k e = Nullspace
  { _nullspaceKernel :: Matrix e
  , _nullspaceKernelGaussJordan :: Matrix e
  , _nullspaceTolerance_Inf :: e
  , _nullspaceForwards :: Map k (Maybe (IndexOf Vector))
  , _nullspaceBackwards :: Map (Maybe (IndexOf Vector)) (Set k)
  } deriving (Read, Show)

deriving instance (Eq k, Eq e, Container Vector e, Num e) => Eq (Nullspace k e)

nullspace :: (Ord k) => Set k -> Matrix ℝ -> Nullspace k ℝ
nullspace ks matrix =
  let
    ker = HMatrix.nullspace matrix
    tol = tolerance_Inf ker
    ~(rcef_ker, _) = rcef tol ker
    ~(fwd, bwd) = findFreeColumns (Set.toAscList ks) tol rcef_ker
  in
    Nullspace
      { _nullspaceKernel = ker
      , _nullspaceKernelGaussJordan = rcef_ker
      , _nullspaceTolerance_Inf = tol
      , _nullspaceForwards = fwd
      , _nullspaceBackwards = bwd
      }
