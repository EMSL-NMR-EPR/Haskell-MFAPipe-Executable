{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Science.Chemistry.IsotopicLabeling.SteadyState
( SteadyState(..)
, runSteadyState
, SteadyStateLayer(..)
, runSteadyStateLayer
, SteadyStateStep(..)
, runSteadyStateStep
) where

import qualified Control.Lens
import           Control.Monad (foldM)
import           Control.Monad.Error.Class (MonadError())
import           Control.Monad.Parallel (MonadParallel())
import qualified Control.Monad.Parallel
import           Control.Monad.Reader.Class (MonadReader())
import qualified Control.Monad.Reader.Class
import qualified Control.Parallel.Strategies
import           Data.Bifunctor (second)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Monoid.EndoM (EndoM(..))
import           Data.Proxy (Proxy(..))
import           Numeric.LinearAlgebra.HMatrix (Field(), Numeric(), IndexOf, Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Lens (AsVector(..), _Rows)
import           Science.Chemistry.EMU.Factorized.Class
import           Science.Chemistry.IsotopicLabeling.FractionVector

newtype SteadyState i k e = SteadyState [SteadyStateLayer i k e]
  deriving (Eq, Ord, Read, Show)

runSteadyState
  :: (MonadError (FractionVectorDictError ty i k) m, MonadParallel m, MonadReader (Vector e) m, Ord i, Ord k, Factorized k, AsVector (FractionVector ty), Monoid (FractionVector ty e), Eq e, Num (Vector e), Field e, Numeric e)
  => SteadyState i k e
  -> EndoM m (FractionVectorDict ty i k e)
runSteadyState (SteadyState layers) = EndoM (\acc0 -> foldM (\acc layer -> appEndoM (runSteadyStateLayer layer) acc >>= unionFractionVectorDict acc) acc0 layers)

newtype SteadyStateLayer i k e = SteadyStateLayer [SteadyStateStep i k e]
  deriving (Eq, Ord, Read, Show)

runSteadyStateLayer
  :: (MonadError (FractionVectorDictError ty i k) m, MonadParallel m, MonadReader (Vector e) m, Ord i, Ord k, Factorized k, AsVector (FractionVector ty), Monoid (FractionVector ty e), Eq e, Num (Vector e), Field e, Numeric e)
  => SteadyStateLayer i k e
  -> EndoM m (FractionVectorDict ty i k e)
runSteadyStateLayer (SteadyStateLayer steps) = EndoM (\acc -> Control.Monad.Parallel.mapM (\step -> appEndoM (runSteadyStateStep step) acc) steps >>= unionsFractionVectorDict)

data SteadyStateStep i k e = SteadyStateStep
  { _steadyStateStepU :: [i]
  , _steadyStateStepX :: [k]
  , _steadyStateStepY :: [k]
  , _steadyStateStepA :: [(IndexOf Matrix, Vector e)]
  , _steadyStateStepB :: [(IndexOf Matrix, Vector e)]
  , _steadyStateStepA' :: Map i [(IndexOf Matrix, e)]
  , _steadyStateStepB' :: Map i [(IndexOf Matrix, e)]
  } deriving (Eq, Ord, Read, Show)

runSteadyStateStep
  :: (MonadError (FractionVectorDictError ty i k) m, MonadParallel m, MonadReader (Vector e) m, Ord i, Ord k, Factorized k, AsVector (FractionVector ty), Monoid (FractionVector ty e), Eq e, Num (Vector e), Field e, Numeric e)
  => SteadyStateStep i k e
  -> EndoM m (FractionVectorDict ty i k e)
runSteadyStateStep SteadyStateStep{..} = EndoM $ \acc -> do
  let
    n = length _steadyStateStepX
    m = length _steadyStateStepY
    size_A = (n, n)
    size_B = (n, m)
  matrix_A <- Control.Monad.Reader.Class.asks (\vector_U -> Numeric.LinearAlgebra.HMatrix.assoc size_A 0 (Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rseq (second (Numeric.LinearAlgebra.HMatrix.dot vector_U)) _steadyStateStepA))
  matrix_B <- Control.Monad.Reader.Class.asks (\vector_U -> Numeric.LinearAlgebra.HMatrix.assoc size_B 0 (Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rseq (second (Numeric.LinearAlgebra.HMatrix.dot vector_U)) _steadyStateStepB))
  matrix_Y <- Control.Lens.view (Control.Lens.mapping _Vector . _Rows) <$> Control.Monad.Parallel.mapM (\k -> lookupFractionVectorDict k acc) _steadyStateStepY
  let
    matrix_A_inv
      | null _steadyStateStepA = Numeric.LinearAlgebra.HMatrix.konst 0 size_A
      | otherwise              = Numeric.LinearAlgebra.HMatrix.inv matrix_A
    matrix_X = matrix_A_inv `Numeric.LinearAlgebra.HMatrix.mul` matrix_B `Numeric.LinearAlgebra.HMatrix.mul` matrix_Y
    kMap = Data.Map.Strict.fromAscList (zip _steadyStateStepX (Control.Lens.review (Control.Lens.mapping _Vector . _Rows) matrix_X))
    mkAsc i = do
      let
        matrix_A' = Numeric.LinearAlgebra.HMatrix.assoc size_A 0 (Data.Map.Strict.findWithDefault [] i _steadyStateStepA')
        matrix_B' = Numeric.LinearAlgebra.HMatrix.assoc size_B 0 (Data.Map.Strict.findWithDefault [] i _steadyStateStepB')
      matrix_Y' <- Control.Lens.view (Control.Lens.mapping (Control.Lens.mapping _Vector . Control.Lens.iso (foldr1 (+)) pure) . _Rows) <$> Control.Monad.Parallel.mapM (\k -> lookupFractionVectorDict' i k acc) _steadyStateStepY
      let
        matrix_X' = matrix_A_inv `Numeric.LinearAlgebra.HMatrix.mul` ((matrix_B' `Numeric.LinearAlgebra.HMatrix.mul` matrix_Y) + (matrix_B `Numeric.LinearAlgebra.HMatrix.mul` matrix_Y') - (matrix_A' `Numeric.LinearAlgebra.HMatrix.mul` matrix_X))
        kMap' = Data.Map.Strict.fromAscList (zip _steadyStateStepX (Control.Lens.review (Control.Lens.mapping _Vector . _Rows) matrix_X'))
      return (i, kMap')
  ikMap <- Data.Map.Strict.fromAscList <$> Control.Monad.Parallel.mapM mkAsc _steadyStateStepU
  return (FractionVectorDict Proxy kMap ikMap)
