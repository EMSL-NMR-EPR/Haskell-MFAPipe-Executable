{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.MFA
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types and functions for metabolic flux analysis (MFA)
-- in the \"mfaPipe\" executable.
-----------------------------------------------------------------------------

module MFAPipe.MFA
( MFA(..)
, MFASpec(..)
, MFAConstraints(..)
, MFAParallelLabelingExperiment(..)
, MFASingleLabelingExperiment(..)
, MFAResult(..)
, MFAError(..)
, InvalidRadixReason(..)
, InvalidBoundReason(..)
, InvalidWeightReason(..)
, InvalidLinearConstraintReason(..)
, toMFA
, toMFASpec
, toArchive
, toArchiveWith
) where

import           Codec.Archive.Zip (Archive(..), Entry)
import qualified Codec.Archive.Zip
import           Control.Applicative (Const(..))
import qualified Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.Random (RandT)
import           Control.Monad.Random.Class (MonadRandom())
import qualified Control.Monad.Random.Class
import           Control.Monad.Random.Logger.Instances ()
import           Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Reader
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Csv (ToField(), EncodeOptions)
import qualified Data.Csv
import qualified Data.Either
import           Data.Functor.Product (Product(..))
import qualified Data.HashMap.Strict
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import qualified Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import qualified Data.Maybe
import           Data.Proxy (Proxy(..))
import           Data.Set (Set)
import qualified Data.Set
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector.Unboxed
import           Foreign.Storable (Storable())
import           Language.FluxJS.Conversion.Text
import qualified Language.FluxJS.Types as FluxJS
import           Language.INCA.Conversion.Text
import           MFAPipe.Csv.Types.ContributionMatrix (ContributionMatrixRecords(..))
import qualified MFAPipe.Csv.Types.ContributionMatrix
import qualified MFAPipe.Csv.Types.EMUReactionNetwork
import           MFAPipe.Csv.Types.Flux (WeightedFluxRecords(..))
import qualified MFAPipe.Csv.Types.Flux
import           MFAPipe.Csv.Types.FluxCovarianceMatrix (FluxCovarianceMatrixRecords(..))
import qualified MFAPipe.Csv.Types.FluxCovarianceMatrix
import qualified MFAPipe.Csv.Types.Info
import           MFAPipe.Csv.Types.IsotopomerFractionVector (IsotopomerFractionVectorRecords(..))
import qualified MFAPipe.Csv.Types.IsotopomerFractionVector
import           MFAPipe.Csv.Types.JacobianMatrix (JacobianMatrixRecords(..))
import qualified MFAPipe.Csv.Types.JacobianMatrix
import           MFAPipe.Csv.Types.MassFractionVector (MassFractionVectorRecords(..))
import qualified MFAPipe.Csv.Types.MassFractionVector
import           MFAPipe.Csv.Types.NullspaceMatrix (NullspaceMatrixRecords(..))
import qualified MFAPipe.Csv.Types.NullspaceMatrix
import           MFAPipe.Csv.Types.Parameters (ParametersRecords(..))
import qualified MFAPipe.Csv.Types.Parameters
import qualified MFAPipe.Csv.Types.ReactionNetwork
import           MFAPipe.Csv.Types.Residual (WeightedResidualRecords(..))
import qualified MFAPipe.Csv.Types.Residual
import           MFAPipe.Csv.Types.Statistics (StatisticsRecords(..))
import qualified MFAPipe.Csv.Types.Statistics
import           MFAPipe.Csv.Types.Stoichiometry (DenseEncoding(DenseEncodeAll))
import qualified MFAPipe.Csv.Types.Stoichiometry
import           MFAPipe.Utils
import           Numeric.LevMar (LevMarError, Constraints(..), Info, Jacobian, Model, Options, Params, Samples)
import           Numeric.LevMar.Extras.LevMar (LevMar(..))
import qualified Numeric.LevMar.Extras.LevMar
import           Numeric.LinearAlgebra.HMatrix (Container(), Element(), Numeric(), IndexOf, Matrix, Vector)
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan (Nullspace(..))
import qualified Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan
import           Science.Chemistry.EMU.IsotopicLabeling.DSL.Evaluate.Class
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.EMUGraph
import           Science.Chemistry.EMU.EMUReactionNetwork
import           Science.Chemistry.FluxVar
import           Science.Chemistry.IsotopicLabeling.DSL.FromDict.Class (Dict)
import           Science.Chemistry.IsotopicLabeling.FractionTypeDict (FractionTypeDict)
import qualified Science.Chemistry.IsotopicLabeling.FractionTypeDict
import           Science.Chemistry.IsotopicLabeling.FractionVector
import           Science.Chemistry.IsotopicLabeling.IsotopomerFractionVector
import           Science.Chemistry.IsotopicLabeling.MassFractionVector
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.ReactionNetwork
import           Science.Chemistry.Stoichiometry
import qualified Statistics.Distribution
import qualified Statistics.Distribution.Normal
import qualified Statistics.Sample
import qualified Statistics.Test.KolmogorovSmirnov
import           Statistics.Types (Sample)
import           System.IO (hFlush, hPutChar, stdout)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.Data (MonadRecord(), Data, Lvl, Msg)
import qualified System.Log.Simple
import           System.Random (Random(), RandomGen())
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text
import qualified Text.Printf

newtype MFA i k e = MFA
  { runMFA :: Int -> Options e -> Either LevMarError (MFAResult i k e)
  }

data MFASpec i k a e = MFASpec
  { _mfaSpecRadix :: Integer
  , _mfaSpecReactionNetwork :: ReactionNetwork i k a
  , _mfaSpecReactionNetworkDense :: Dense i k e
  , _mfaSpecReactionNetworkNullspace :: Nullspace i e
  , _mfaSpecEMUReactionNetwork :: EMUReactionNetwork i k
  , _mfaSpecEMUReactionNetworkDense :: IntMap (Dense i (EMU k) e)
  , _mfaSpecMFAConstraints :: MFAConstraints i e
  , _mfaSpecMFAParallelLabelingExperiment :: MFAParallelLabelingExperiment i k e
  , _mfaSpecInitialParams :: Vector e
  }

data MFAConstraints i e = MFAConstraints
  { _mfaConstraintsEqualities :: Map (IndexOf Vector) e
  , _mfaConstraintsLowerBounds :: Map (IndexOf Vector) e
  , _mfaConstraintsUpperBounds :: Map (IndexOf Vector) e
  , _mfaConstraintsWeights :: Map (IndexOf Vector) e
  , _mfaConstraintsLinearConstraints :: [(Map i e, e)]
  , _mfaConstraintsDefaultLowerBoundMaybe :: Maybe e
  , _mfaConstraintsDefaultLowerBound :: e
  , _mfaConstraintsDefaultUpperBoundMaybe :: Maybe e
  , _mfaConstraintsDefaultUpperBound :: e
  , _mfaConstraintsDefaultWeightMaybe :: Maybe e
  , _mfaConstraintsDefaultWeight :: e
  }

data MFAParallelLabelingExperiment i k e = MFAParallelLabelingExperiment
  { _mfaParallelLabelingExperimentSingleLabelingExperiments :: Map Text (MFASingleLabelingExperiment i k e)
  , _mfaParallelLabelingExperimentIndependentFluxVarsCount :: Integer
  , _mfaParallelLabelingExperimentIndependentMeasurementsCount :: Integer
  }

data MFASingleLabelingExperiment i k e = MFASingleLabelingExperiment
  { _mfaSingleLabelingExperimentIsotopicDistributions :: Map k [(Rational, Matrix e)]
  , _mfaSingleLabelingExperimentMeasurements :: Map (EMUExpr k) [(e, e)]
  , _mfaSingleLabelingExperimentIndependentMeasurementsCount :: Integer
  }

data MFAResult i k e = MFAResult
  { _mfaResultFlux :: Map i e
  , _mfaResultFluxCovariance :: Map i (Map i e)
  , _mfaResultFluxVariance :: Map i e
  , _mfaResultResidual :: Map Text (Map (EMUExpr k) [e])
  , _mfaResultResidualVariance :: Map Text (Map (EMUExpr k) [e])
  , _mfaResultResidualMeanVariance :: Map Text (e, e)
  , _mfaResultResidualMeanVariance' :: (e, e)
  , _mfaResultResidualKolmogorovSmirnov :: Map Text (e, e)
  , _mfaResultResidualKolmogorovSmirnov' :: (e, e)
  , _mfaResultResidualSSE :: Map Text e
  , _mfaResultResidualSSE' :: e
  , _mfaResultResidualSST :: Map Text e
  , _mfaResultResidualSST' :: e
  , _mfaResultJacobianMatrix :: Map Text (Map (EMUExpr k) [Map i e])
  , _mfaResultContributionMatrix :: Map Text (Map (EMUExpr k) [Map i e])
  , _mfaResultDict :: Map Text (Dict (EMUExprF k) i e)
  , _mfaResultInfo :: Info e
  }

data MFAError i k a e
  = InvalidRadix InvalidRadixReason
  | InvalidBound (InvalidBoundReason i)
  | InvalidWeight (InvalidWeightReason i)
  | InvalidLinearConstraint (InvalidLinearConstraintReason i e)
  | WrapINCAError { unwrapINCAError :: INCAError a }
  | WrapFluxJSError { unwrapFluxJSError :: FluxJSError a }
  deriving (Eq, Ord, Read, Show)

data InvalidRadixReason
  = RadixNotFound
  | RadixIsNegative Integer
  | RadixIsZero Integer
  deriving (Eq, Ord, Read, Show)

data InvalidBoundReason i
  = CannotBoundDependentVar i
  | DuplicateBound (IndexOf Vector) i
  deriving (Eq, Ord, Read, Show)

data InvalidWeightReason i
  = CannotWeightDependentVar i
  | DuplicateWeight (IndexOf Vector) i
  deriving (Eq, Ord, Read, Show)

data InvalidLinearConstraintReason i e
  = LinearConstraintMustBeEqualTo (Map i e)
  deriving (Eq, Ord, Read, Show)

toMFA
  :: (i ~ FluxVar a a, k ~ MetaboliteVar a, a ~ Text, e ~ Double)
  => MFASpec i k a e
  -> MFA i k e
toMFA MFASpec{..} =
  let
    ixs :: Set (FluxVar Text Text)
    ixs = _denseReactionIndices _mfaSpecReactionNetworkDense

    ixsAscList :: [FluxVar Text Text]
    ixsAscList = Data.Set.toAscList ixs

    xs :: [(Map (MetaboliteVar Text) [(Rational, Matrix Double)], [EMUExpr (MetaboliteVar Text)])]
    ys, sigmas :: Samples Double
    ~(xs, (ys, sigmas)) = fromMFAParallelLabelingExperiment _mfaSpecMFAParallelLabelingExperiment
      where
        fromMFAParallelLabelingExperiment :: (Storable e) => MFAParallelLabelingExperiment i k e -> ([(Map k [(Rational, Matrix e)], [EMUExpr k])], (Samples e, Samples e))
        fromMFAParallelLabelingExperiment = second (bimap fromLists fromLists . unzip . map unzip) . unzip . map (uncurry ((. unzip) . first . (,)) . second (concatMap (uncurry (map . (,)))) . second Data.Map.Strict.toAscList) . map (pure (,) <*> _mfaSingleLabelingExperimentIsotopicDistributions <*> _mfaSingleLabelingExperimentMeasurements) . Data.Map.Strict.elems . _mfaParallelLabelingExperimentSingleLabelingExperiments
          where
            fromLists :: (Storable e) => [[e]] -> Vector e
            fromLists = Numeric.LinearAlgebra.HMatrix.vjoin . map Numeric.LinearAlgebra.HMatrix.fromList

    gr :: EMUGraph (FluxVar Text Text) (MetaboliteVar Text)
    gr = Control.Lens.view _EMUGraph _mfaSpecEMUReactionNetwork

    doSteadyStateEMUExpr :: Params Double -> (Vector Double, Matrix Double)
    doSteadyStateEMUExpr = either (error . show) (either (error . show) id . concatModelAndJacobianE . map Control.Monad.Trans.Except.runExcept) . Control.Monad.Trans.Except.runExcept . Control.Monad.Trans.Reader.runReaderT (fmap (map (\ ~(_, _, e) -> e)) (runSteadyStateEMUExpr (fromInteger _mfaSpecRadix) ixs gr xs))

    doSteadyStateEMUExpr' :: Params Double -> [Dict (EMUExprF (MetaboliteVar Text)) (FluxVar Text Text) Double]
    doSteadyStateEMUExpr' = either (error . show) id . Control.Monad.Trans.Except.runExcept . Control.Monad.Trans.Reader.runReaderT (runSteadyStateEMUExpr' (fromInteger _mfaSpecRadix) ixs gr (map fst xs))

    model :: Model Double
    model = fst . doSteadyStateEMUExpr

    mJac :: Maybe (Jacobian Double)
    mJac = Just (snd . doSteadyStateEMUExpr)

    ps0 :: Params Double
    ps0 = _mfaSpecInitialParams

    _mfaConstraints :: MFAConstraints (FluxVar Text Text) Double
    _mfaConstraints@MFAConstraints{..} = _mfaSpecMFAConstraints

    constraints :: Constraints Double
    constraints = Constraints lBs_Vector uBs_Vector ws_Vector linearConstraints_Matrix_Vector
      where
        freeVars :: [IndexOf Vector]
        freeVars = map fst (zip (enumFromThen 0 1) (Data.Set.toList ixs))

        lBs_Vector, uBs_Vector, ws_Vector :: Maybe (Params Double)
        lBs_Vector = mkVectorWithDefault freeVars _mfaConstraintsDefaultLowerBoundMaybe _mfaConstraintsDefaultLowerBound _mfaConstraintsLowerBounds
        uBs_Vector = mkVectorWithDefault freeVars _mfaConstraintsDefaultUpperBoundMaybe _mfaConstraintsDefaultUpperBound _mfaConstraintsUpperBounds
        ws_Vector = mkVectorWithDefault freeVars _mfaConstraintsDefaultWeightMaybe _mfaConstraintsDefaultWeight _mfaConstraintsWeights

        linearConstraints_Matrix_Vector :: Maybe (Matrix Double, Vector Double)
        linearConstraints_Matrix_Vector
          | (Numeric.LinearAlgebra.HMatrix.rows new_matrix > 0) && (Numeric.LinearAlgebra.HMatrix.rows new_matrix == Numeric.LinearAlgebra.HMatrix.size new_vector) = Just r0
          | otherwise = Nothing
          where
            r0@(new_matrix, new_vector) = bimap f g (mkLinearConstraints ixsAscList _mfaConstraintsLinearConstraints)
              where
                f matrix
                  | Numeric.LinearAlgebra.HMatrix.rows matrix == 0 = matrix0
                  | otherwise = Numeric.LinearAlgebra.HMatrix.fromBlocks [[matrix0], [matrix]]
                g vector
                  | Numeric.LinearAlgebra.HMatrix.size vector == 0 = vector0
                  | otherwise = Numeric.LinearAlgebra.HMatrix.vjoin [vector0, vector]
                matrix0 = snd (_denseIntermediate _mfaSpecReactionNetworkDense)
                vector0 = Numeric.LinearAlgebra.HMatrix.konst 0 (Numeric.LinearAlgebra.HMatrix.rows matrix0)

    ascList :: [(IndexOf Vector, Double)]
    ascList = Data.Map.Strict.toAscList _mfaConstraintsEqualities

    toMFAResult :: Params Double -> Matrix Double -> Samples Double -> Matrix Double -> Info Double -> MFAResult (FluxVar Text Text) (MetaboliteVar Text) Double
    toMFAResult ps covar_ps new_ys jac_ps info =
      let
        var_ps :: Vector Double
        var_ps = Numeric.LinearAlgebra.HMatrix.takeDiag covar_ps

        covar_ys :: Matrix Double
        covar_ys = jac_ps `Numeric.LinearAlgebra.HMatrix.mul` covar_ps `Numeric.LinearAlgebra.HMatrix.mul` Numeric.LinearAlgebra.HMatrix.tr jac_ps

        var_ys :: Vector Double
        var_ys = Numeric.LinearAlgebra.HMatrix.takeDiag covar_ys

        contrib :: Matrix Double
        contrib = Numeric.LinearAlgebra.HMatrix.fromLists $ do
          let
            m :: Matrix Double
            m = Numeric.LinearAlgebra.HMatrix.cmap (\x -> x * x) (covar_ps `Numeric.LinearAlgebra.HMatrix.mul` Numeric.LinearAlgebra.HMatrix.tr jac_ps)
            rows, cols :: Int
            ~(rows, cols) = Numeric.LinearAlgebra.HMatrix.size m
          j <- enumFromThenTo 0 1 (cols - 1)
          return $ do
            i <- enumFromThenTo 0 1 (rows - 1)
            let
              m_ij, var_ps_i, var_ys_j :: Double
              m_ij = m `Numeric.LinearAlgebra.HMatrix.atIndex` (i, j)
              var_ps_i = var_ps `Numeric.LinearAlgebra.HMatrix.atIndex` i
              var_ys_j = var_ys `Numeric.LinearAlgebra.HMatrix.atIndex` j
            return (m_ij / (var_ps_i * var_ys_j))

        measurementCounts :: Map Text [(EMUExpr (MetaboliteVar Text), Int)]
        measurementCounts = Data.Map.Strict.map (map (second length) . Data.Map.Strict.toAscList . _mfaSingleLabelingExperimentMeasurements) (_mfaParallelLabelingExperimentSingleLabelingExperiments _mfaSpecMFAParallelLabelingExperiment)

        ys_List, new_ys_List, var_ys_List, sigmas_List :: [Double]
        ys_List = Numeric.LinearAlgebra.HMatrix.toList ys
        new_ys_List = Numeric.LinearAlgebra.HMatrix.toList new_ys
        var_ys_List = Numeric.LinearAlgebra.HMatrix.toList var_ys
        sigmas_List = Numeric.LinearAlgebra.HMatrix.toList sigmas

        ps_Map, var_ps_Map :: Map (FluxVar Text Text) Double
        ps_Map = fromVectorAsMap ixsAscList ps
        var_ps_Map = fromVectorAsMap ixsAscList var_ps

        covar_ps_Map :: Map (FluxVar Text Text) (Map (FluxVar Text Text) Double)
        covar_ps_Map = fromMatrixAsMap ixsAscList ixsAscList covar_ps

        ys_Map, new_ys_Map, var_ys_Map, sigmas_Map :: Map Text [(EMUExpr (MetaboliteVar Text), [Double])]
        ys_Map = fromListAsAscListMap ys_List measurementCounts
        new_ys_Map = fromListAsAscListMap new_ys_List measurementCounts
        var_ys_Map = fromListAsAscListMap var_ys_List measurementCounts
        sigmas_Map = fromListAsAscListMap sigmas_List measurementCounts

        fromMatrixRows :: Matrix Double -> Map Text (Map (EMUExpr (MetaboliteVar Text)) [Map (FluxVar Text Text) Double])
        fromMatrixRows = \matrix -> Data.Map.Strict.map (Data.Map.Strict.fromList . map (second (map (fromVectorAsMap ixsAscList)))) (fromListAsAscListMap (Numeric.LinearAlgebra.HMatrix.toRows matrix) measurementCounts)

        mean_ys_Map, sse_Map, sst_Map :: Map Text Double
        mean_ys, sse, sst :: Double
        sse_Map = Data.Map.Strict.intersectionWith (\sigmas' ys' -> sum (zipWith (\sigma' y' -> sigma' * y' * y') (concatMap snd sigmas') (concatMap snd ys'))) sigmas_Map new_ys_Map
        sse = sum (Data.Map.Strict.elems sse_Map)
        mean_ys_Map = Data.Map.Strict.map (Statistics.Sample.mean . Numeric.LinearAlgebra.HMatrix.fromList . concatMap snd) ys_Map
        mean_ys = Statistics.Sample.mean ys
        sst_Map = Data.Map.Strict.intersectionWith (\sigmas' ys' -> sum (zipWith (\sigma' y' -> sigma' * y' * y') (concatMap snd sigmas') ys')) sigmas_Map (Data.Map.Strict.intersectionWith (\ys' mean' -> map (subtract mean') (concatMap snd ys')) ys_Map mean_ys_Map)
        sst = sigmas `Numeric.LinearAlgebra.HMatrix.dot` (Numeric.LinearAlgebra.HMatrix.cmap (\y' -> y' * y') (Numeric.LinearAlgebra.HMatrix.cmap (\y' -> y' - mean_ys) ys))
      in
        MFAResult
          { _mfaResultFlux = ps_Map
          , _mfaResultFluxCovariance = covar_ps_Map
          , _mfaResultFluxVariance = var_ps_Map
          , _mfaResultResidual = Data.Map.Strict.map Data.Map.Strict.fromList new_ys_Map
          , _mfaResultResidualVariance = Data.Map.Strict.map Data.Map.Strict.fromList var_ys_Map
          , _mfaResultResidualMeanVariance = Data.Map.Strict.map (Statistics.Sample.meanVariance . Data.Vector.Unboxed.fromList . concatMap snd) new_ys_Map
          , _mfaResultResidualMeanVariance' = Statistics.Sample.meanVariance (Data.Vector.Unboxed.fromList new_ys_List)
          , _mfaResultResidualKolmogorovSmirnov = Data.Map.Strict.map (doKolmogorovSmirnov . Data.Vector.Unboxed.fromList . concatMap snd) new_ys_Map
          , _mfaResultResidualKolmogorovSmirnov' = doKolmogorovSmirnov (Data.Vector.Unboxed.fromList new_ys_List)
          , _mfaResultResidualSSE = sse_Map
          , _mfaResultResidualSSE' = sse
          , _mfaResultResidualSST = sst_Map
          , _mfaResultResidualSST' = sst
          , _mfaResultJacobianMatrix = fromMatrixRows jac_ps
          , _mfaResultContributionMatrix = fromMatrixRows contrib
          , _mfaResultDict = Data.Map.Strict.fromList (zip (Data.Map.Strict.keys measurementCounts) (doSteadyStateEMUExpr' ps))
          , _mfaResultInfo = info
          }

    levmar0, levmar1, levmar2 :: LevMar Double (Params Double, Info Double, Matrix Double)
    levmar0 = Numeric.LevMar.Extras.LevMar.levmar model mJac ps0 ys constraints
    levmar1@(LevMar _ mkWeightedResiduals (Just mkWeightedJacobian) _ _ _) = Numeric.LevMar.Extras.LevMar.weighted sigmas levmar0
    levmar2 = Numeric.LevMar.Extras.LevMar.fixParams ascList levmar1
  in
    case fmap (\(ps, info, covar_ps) -> toMFAResult ps covar_ps (mkWeightedResiduals ps) (mkWeightedJacobian ps) info) levmar2 of
      LevMar new_done new_model new_mJac new_ps0 new_ys new_constraints -> MFA (Numeric.LevMar.Extras.LevMar.runLevMar (LevMar new_done (new_model . (\x -> unsafePerformIO (hPutChar stdout 'x' >> hFlush stdout >> return x))) (fmap (\f -> f . (\x -> unsafePerformIO (hPutChar stdout 'J' >> hFlush stdout >> return x))) new_mJac) new_ps0 new_ys new_constraints))

toMFASpec
  :: (RandomGen g, Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m, i ~ FluxVar a a, k ~ MetaboliteVar a, a ~ Text, e ~ Double)
  => FluxJS.Model e
  -> ExceptT (MFAError i k a e) (RandT g m) (MFASpec i k a e)
toMFASpec (FluxJS.Model radixMaybe0 reactionNetwork0 experiments0 (FluxJS.Constraints _ (FluxJS.Bounds bounds0) (FluxJS.Weights weights0) (FluxJS.LinearConstraints linearConstraints0) lowerBoundMaybe0 lowerBound0 upperBoundMaybe0 upperBound0 weightMaybe0 weight0)) = do
  System.Log.Simple.info "Determining number of isotopic labeling states per atom"
  radix <- case radixMaybe0 of
    Nothing -> do
      Control.Monad.Trans.Except.throwE (InvalidRadix RadixNotFound)
    Just radix0 -> do
      case radix0 `compare` 0 of
        LT -> do
          Control.Monad.Trans.Except.throwE (InvalidRadix (RadixIsNegative radix0))
        EQ -> do
          Control.Monad.Trans.Except.throwE (InvalidRadix (RadixIsZero radix0))
        GT -> do
          return radix0
  System.Log.Simple.info "Number of isotopic labeling states per atom was determined successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Number of isotopic labeling states per atom: %d" radix)

  System.Log.Simple.info "Constructing chemical reaction network"
  reactionNetwork <- Control.Monad.Trans.Except.withExceptT (either WrapINCAError WrapFluxJSError) (fromFluxJSReactionNetwork reactionNetwork0)
  System.Log.Simple.info "Chemical reaction network was constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size (getReactionNetwork reactionNetwork)) "chemical reaction"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList (getReactionNetwork reactionNetwork))) $ \ ~(n, (ix, reaction)) -> do
    let
      ix' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty ix)))
      reaction' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty reaction)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %s" n ix' reaction')

  System.Log.Simple.info "Constructing stoichiometric model"
  let
    reactionNetworkDense :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double
    reactionNetworkDense@Dense{..} = toStoichiometricModel reactionNetwork
  System.Log.Simple.info "Stoichiometric model was constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Set.size (fst _denseIntermediate)) "intracellular metabolite"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Set.toAscList (fst _denseIntermediate))) $ \ ~(n, k) -> do
    let
      k' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty k)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s" n k')
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Set.size (fst _denseReagentProduct)) "extracellular metabolite"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Set.toAscList (fst _denseReagentProduct))) $ \ ~(n, k) -> do
    let
      k' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty k)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s" n k')

  System.Log.Simple.info "Constructing nullspace"
  let
    reactionNetworkNullspace :: Nullspace (FluxVar Text Text) Double
    reactionNetworkNullspace@Nullspace{..} = Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan.nullspace _denseReactionIndices (snd _denseIntermediate)
    freeVars :: [IndexOf Vector]
    freeVars = Data.Maybe.catMaybes (Data.Map.Strict.keys _nullspaceBackwards)
    freeVarsCount :: Int
    freeVarsCount = length freeVars
  System.Log.Simple.debug (Text.Printf.printf "Found %s and %s" (pluralizeWith (++ "s") freeVarsCount "independent variable") (pluralizeWith (++ "s") (Data.Set.size (Data.Map.Strict.findWithDefault Data.Set.empty Nothing _nullspaceBackwards)) "dependent variable"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList _nullspaceForwards)) $ \ ~(n, (ix, uMaybe)) -> do
    let
      ix' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty ix)))
    case uMaybe of
      Nothing -> do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s is a dependent variable" n ix')
      Just u -> do
        let
          u' = 'u' : show (u + 1)
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %s" n ix' u')
  System.Log.Simple.info "Nullspace was constructed successfully!"

  System.Log.Simple.info "Constructing EMU reaction network"
  let
    emuReactionNetwork :: EMUReactionNetwork (FluxVar Text Text) (MetaboliteVar Text)
    emuReactionNetwork = toEMUReactionNetwork (fromInteger radix) reactionNetwork
  System.Log.Simple.info "EMU reaction network was constructed successfully!"
  forM_ (Data.IntMap.Strict.toAscList (getEMUReactionNetwork emuReactionNetwork)) $ \ ~(emuSize, emuReactions) -> do
    System.Log.Simple.debug (Text.Printf.printf "Found %s of size %d" (pluralizeWith (++ "s") (length emuReactions) "EMU reaction") emuSize)
    forM_ (zip (enumFromThen 1 2 :: [Integer]) emuReactions) $ \ ~(n, (ix, emuReaction)) -> do
      let
        ix' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty ix)))
        emuReaction' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty emuReaction)))
      System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %s" n ix' emuReaction')

  System.Log.Simple.info "Constructing EMU stoichiometric model"
  let
    emuReactionNetworkDense :: IntMap (Dense (FluxVar Text Text) (EMU (MetaboliteVar Text)) Double)
    emuReactionNetworkDense = toEMUStoichiometricModel emuReactionNetwork
  System.Log.Simple.info "EMU stoichiometric model was constructed successfully!"

  System.Log.Simple.info "Constructing bounds"
  System.Log.Simple.debug (Text.Printf.printf "Default lower bound: %f" lowerBound0)
  System.Log.Simple.debug (Text.Printf.printf "Default upper bound: %f" upperBound0)
  bounds <-
    let
      cataM
        :: (Monad m)
        => ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m (Map (IndexOf Vector) (FluxJS.Bound Double))
        -> Text
        -> FluxJS.Bound Double
        -> ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m (Map (IndexOf Vector) (FluxJS.Bound Double))
      cataM macc fluxVarName x = do
        acc <- macc
        fluxVar <- Control.Monad.Trans.Except.withExceptT (either WrapINCAError WrapFluxJSError) (fromFluxJSFluxVar fluxVarName)
        case Data.Map.Strict.findWithDefault Nothing fluxVar m of
          Nothing -> do
            Control.Monad.Trans.Except.throwE (InvalidBound (CannotBoundDependentVar fluxVar))
          Just k -> do
            case Data.Map.Strict.lookup k acc of
              Nothing -> do
                return (Data.Map.Strict.insert k x acc)
              Just _ -> do
                Control.Monad.Trans.Except.throwE (InvalidBound (DuplicateBound k fluxVar))
      m :: Map (FluxVar Text Text) (Maybe (IndexOf Vector))
      m = Data.Map.Strict.fromList (zip (Data.Set.toList _denseReactionIndices) (map Just (enumFromThen 0 1)))
    in
      Data.HashMap.Strict.foldlWithKey' cataM (return Data.Map.Strict.empty) bounds0
  System.Log.Simple.info "Bounds were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size bounds) "bound"))
  if Data.Map.Strict.null bounds
    then do
      System.Log.Simple.warning (Text.Printf.printf "Using default lower and upper bounds for all metabolic flux variables (denoted \"v\"): %f <= v <= %f" lowerBound0 upperBound0)
    else do
      forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList bounds)) $ \ ~(n, (u, x)) -> do
        let
          u' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (Data.Set.toAscList _denseReactionIndices !! u))))
        case x of
          FluxJS.Free -> do
            System.Log.Simple.warning (Text.Printf.printf "[%d] %s is a free variable" n u')
          FluxJS.GreaterThan lB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s >= %f" n u' lB)
          FluxJS.LessThan uB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s <= %f" n u' uB)
          FluxJS.EqualTo equ -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f" n u' equ)
          FluxJS.Between lB uB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %f <= %s <= %f" n lB u' uB)

  System.Log.Simple.info "Constructing weights"
  System.Log.Simple.debug (Text.Printf.printf "Default weight: %f" weight0)
  weights <-
    let
      cataM
        :: (Monad m)
        => ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m (Map (IndexOf Vector) Double)
        -> Text
        -> FluxJS.Measurement Double
        -> ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m (Map (IndexOf Vector) Double)
      cataM macc fluxVarName measurement = do
        acc <- macc
        fluxVar <- Control.Monad.Trans.Except.withExceptT (either WrapINCAError WrapFluxJSError) (fromFluxJSFluxVar fluxVarName)
        case Data.Map.Strict.findWithDefault Nothing fluxVar m of
          Nothing -> do
            Control.Monad.Trans.Except.throwE (InvalidWeight (CannotWeightDependentVar fluxVar))
          Just k -> do
            case Data.Map.Strict.lookup k acc of
              Nothing -> do
                return (Data.Map.Strict.insert k (fromFluxJSMeasurement measurement) acc)
              Just _ -> do
                Control.Monad.Trans.Except.throwE (InvalidWeight (DuplicateWeight k fluxVar))
      m :: Map (FluxVar Text Text) (Maybe (IndexOf Vector))
      m = Data.Map.Strict.fromList (zip (Data.Set.toList _denseReactionIndices) (map Just (enumFromThen 0 1)))
    in
      Data.HashMap.Strict.foldlWithKey' cataM (return Data.Map.Strict.empty) weights0
  System.Log.Simple.info "Weights were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size weights) "weight"))
  if Data.Map.Strict.null weights
    then do
      System.Log.Simple.warning (Text.Printf.printf "Using default weight for all metabolic flux variables (denoted \"v\"): v +/- %f σ" weight0)
    else do
      forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList weights)) $ \ ~(n, (u, x)) -> do
        let
          u' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (Data.Set.toAscList _denseReactionIndices !! u))))
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s +/- %f σ" n u' x)

  System.Log.Simple.info "Constructing linear constraints"
  linearConstraints <-
    let
      cataM
        :: (Monad m)
        => ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m [(Map (FluxVar Text Text) Double, Double)]
        -> Text
        -> FluxJS.Bound Double
        -> ExceptT (MFAError (FluxVar Text Text) (MetaboliteVar Text) Text Double) m [(Map (FluxVar Text Text) Double, Double)]
      cataM macc text bound = do
        acc <- macc
        m <- Control.Monad.Trans.Except.withExceptT (either WrapINCAError WrapFluxJSError) (fromFluxJSLinearConstraint text)
        case bound of
          FluxJS.EqualTo x -> do
            return ((m, x):acc)
          _ -> do
            Control.Monad.Trans.Except.throwE (InvalidLinearConstraint (LinearConstraintMustBeEqualTo m))
    in
      Data.HashMap.Strict.foldlWithKey' cataM (return []) linearConstraints0
  System.Log.Simple.info "Linear constraints were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (length linearConstraints) "linear constraint"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) linearConstraints) $ \ ~(n, (m, x)) -> do
    let
      m' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (LinearFunction (Data.Map.Strict.toAscList m)))))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f" n m' x)

  System.Log.Simple.info "Constructing constraints"
  let
    equalities, lowerBounds, upperBounds :: Map (IndexOf Vector) Double
    ~(equalities, lowerBounds, upperBounds) = partitionBounds bounds
    mfaConstraints :: MFAConstraints (FluxVar Text Text) Double
    mfaConstraints@MFAConstraints{..} = MFAConstraints
      { _mfaConstraintsEqualities = equalities
      , _mfaConstraintsLowerBounds = lowerBounds
      , _mfaConstraintsUpperBounds = upperBounds
      , _mfaConstraintsWeights = weights
      , _mfaConstraintsLinearConstraints = linearConstraints
      , _mfaConstraintsDefaultLowerBoundMaybe = lowerBoundMaybe0
      , _mfaConstraintsDefaultLowerBound = lowerBound0
      , _mfaConstraintsDefaultUpperBoundMaybe = upperBoundMaybe0
      , _mfaConstraintsDefaultUpperBound = upperBound0
      , _mfaConstraintsDefaultWeightMaybe = weightMaybe0
      , _mfaConstraintsDefaultWeight = weight0
      }
  System.Log.Simple.info "Contraints were constructed successfully!"

  System.Log.Simple.info "Constructing experiments"
  let
    toMFAParallelLabelingExperiment :: Map Text (Map (MetaboliteVar Text) [(Rational, Matrix Double)], Map (EMUExpr (MetaboliteVar Text)) [(Double, Double)]) -> MFAParallelLabelingExperiment (FluxVar Text Text) (MetaboliteVar Text) Double
    toMFAParallelLabelingExperiment m0 =
      let
        dictMap :: Map Text (FractionTypeDict (EMU (MetaboliteVar Text)) (IndexOf Vector))
        dictMap = Data.Map.Strict.map (fractionTypeDictEMU . Data.Map.Strict.keysSet . snd) m0
        independentFluxVarsCount :: Integer
        independentFluxVarsCount = toInteger freeVarsCount
        independentMeasurementsCount :: Integer
        independentMeasurementsCount = toInteger (Science.Chemistry.IsotopicLabeling.FractionTypeDict.size (Data.Map.Strict.foldr mappend mempty dictMap))
      in
        MFAParallelLabelingExperiment
          { _mfaParallelLabelingExperimentSingleLabelingExperiments = Data.Map.Strict.map toMFASingleLabelingExperiment (Data.Map.Strict.intersectionWith (,) m0 dictMap)
          , _mfaParallelLabelingExperimentIndependentFluxVarsCount = independentFluxVarsCount
          , _mfaParallelLabelingExperimentIndependentMeasurementsCount = independentMeasurementsCount
          }
    toMFASingleLabelingExperiment :: ((Map (MetaboliteVar Text) [(Rational, Matrix Double)], Map (EMUExpr (MetaboliteVar Text)) [(Double, Double)]), FractionTypeDict (EMU (MetaboliteVar Text)) Int) -> MFASingleLabelingExperiment (FluxVar Text Text) (MetaboliteVar Text) Double
    toMFASingleLabelingExperiment ~((isotopicDistributions, measurements), dict) =
      let
        independentMeasurementsCount :: Integer
        independentMeasurementsCount = toInteger (Science.Chemistry.IsotopicLabeling.FractionTypeDict.size dict)
      in
        MFASingleLabelingExperiment
          { _mfaSingleLabelingExperimentIsotopicDistributions = isotopicDistributions
          , _mfaSingleLabelingExperimentMeasurements = measurements
          , _mfaSingleLabelingExperimentIndependentMeasurementsCount = independentMeasurementsCount
          }
  mfaParallelLabelingExperiment@MFAParallelLabelingExperiment{..} <- toMFAParallelLabelingExperiment <$> Control.Monad.Trans.Except.withExceptT (either WrapINCAError WrapFluxJSError) (fromFluxJSExperiments (fromInteger radix) experiments0)
  System.Log.Simple.info "Experiments were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size _mfaParallelLabelingExperimentSingleLabelingExperiments) "experiment"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.keys _mfaParallelLabelingExperimentSingleLabelingExperiments)) $ \(n, experimentName) -> do
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s" n (Data.Text.unpack experimentName))

  System.Log.Simple.info "Initializing metabolic flux variables"
  initialParams <- getRandomParamsM mfaConstraints (snd _denseIntermediate)
  System.Log.Simple.info "Metabolic flux variables were initialized successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Initialized %s" (pluralizeWith (++ "s") (Numeric.LinearAlgebra.HMatrix.size initialParams) "metabolic flux variable"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Numeric.LinearAlgebra.HMatrix.toList initialParams)) $ \ ~(n, x) -> do
    let
      ix :: Int
      ix = fromInteger n - 1
      u = Data.Set.toAscList _denseReactionIndices !! ix
      u' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty u)))
    if Data.Map.Strict.member ix _mfaConstraintsEqualities
      then do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f (bound)" n u' x)
      else do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f" n u' x)

  return $ MFASpec
    { _mfaSpecRadix = radix
    , _mfaSpecReactionNetwork = reactionNetwork
    , _mfaSpecReactionNetworkDense = reactionNetworkDense
    , _mfaSpecReactionNetworkNullspace = reactionNetworkNullspace
    , _mfaSpecEMUReactionNetwork = emuReactionNetwork
    , _mfaSpecEMUReactionNetworkDense = emuReactionNetworkDense
    , _mfaSpecMFAConstraints = mfaConstraints
    , _mfaSpecMFAParallelLabelingExperiment = mfaParallelLabelingExperiment
    , _mfaSpecInitialParams = initialParams
    }

-- | Convert the result of a MFA computation to a zip archive.
--
-- Efficiently serialize CSV records as lazy 'Data.ByteString.Lazy.ByteString's.
--
-- Directory tree:
--
-- > .
-- > +-- emu_reaction_network
-- > |   +-- [EMU size]
-- > |       +-- reaction.csv
-- > |       +-- stoichiometry.csv
-- > +-- reaction_network
-- > |   +-- nullspace.csv
-- > |   +-- reaction.csv
-- > |   +-- stoichiometry.csv
-- > +-- result
-- >     +-- [experiment name]
-- >     |   +-- [EMU size]
-- >     |   |   +-- isotopomer_fraction.csv
-- >     |   |   +-- mass_fraction.csv
-- >     +-- contribution.csv
-- >     +-- flux.csv
-- >     +-- flux_covariance.csv
-- >     +-- info.csv
-- >     +-- jacobian.csv
-- >     +-- parameters.csv
-- >     +-- residual.csv
-- >     +-- statistics.csv
--
toArchive
  :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m, Ord i, Ord k, Pretty i, Pretty k, Pretty a, Element e, ToField i, ToField k, ToField e, Show e, Fractional e)
  => Integer
  -- ^ modification time (seconds since unix epoch)
  -> MFASpec i k a e
  -- ^ stoichiometric models for EMU reaction network
  -> MFAResult i k e
  -- ^ result of MFA computation
  -> m Archive
  -- ^ archive
toArchive = toArchiveWith Data.Csv.defaultEncodeOptions

-- | Like 'toArchive', but lets you customize how the CSV data is encoded.
toArchiveWith
  :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m, Ord i, Ord k, Pretty i, Pretty k, Pretty a, Element e, ToField i, ToField k, ToField e, Show e, Fractional e)
  => EncodeOptions
  -- ^ encoding options for CSV files
  -> Integer
  -- ^ modification time (seconds since unix epoch)
  -> MFASpec i k a e
  -- ^ stoichiometric models for EMU reaction network
  -> MFAResult i k e
  -- ^ result of MFA computation
  -> m Archive
  -- ^ archive
toArchiveWith opts modtime MFASpec{..} MFAResult{..} = do
  System.Log.Simple.info "Serializing CSV documents"

  let
    ixs = _denseReactionIndices _mfaSpecReactionNetworkDense
    measurements = Data.Map.Strict.map _mfaSingleLabelingExperimentMeasurements (_mfaParallelLabelingExperimentSingleLabelingExperiments _mfaSpecMFAParallelLabelingExperiment)

  -- "./reaction_network/reaction.csv"
  System.Log.Simple.debug "[1] Chemical reaction network"
  let
    reactionNetworkEntry :: Entry
    reactionNetworkEntry = Codec.Archive.Zip.toEntry reactionNetworkFilePath modtime reactionNetworkCsv
      where
        reactionNetworkFilePath :: FilePath
        reactionNetworkFilePath = "reaction_network/reaction.csv"
        reactionNetworkCsv :: ByteString
        reactionNetworkCsv = MFAPipe.Csv.Types.ReactionNetwork.encodeWith opts _mfaSpecReactionNetwork

  -- "./reaction_network/stoichiometry.csv"
  System.Log.Simple.debug "[2] Stoichiometric model"
  let
    reactionNetworkDenseEntry :: Entry
    reactionNetworkDenseEntry = Codec.Archive.Zip.toEntry reactionNetworkDenseFilePath modtime reactionNetworkDenseCsv
      where
        reactionNetworkDenseFilePath :: FilePath
        reactionNetworkDenseFilePath = "reaction_network/stoichiometry.csv"
        reactionNetworkDenseCsv :: ByteString
        reactionNetworkDenseCsv = MFAPipe.Csv.Types.Stoichiometry.encodeWith opts DenseEncodeAll _mfaSpecReactionNetworkDense

  -- "./reaction_network/nullspace.csv"
  System.Log.Simple.debug "[3] Nullspace"
  let
    reactionNetworkNullspaceEntry :: Entry
    reactionNetworkNullspaceEntry = Codec.Archive.Zip.toEntry reactionNetworkNullspaceFilePath modtime reactionNetworkNullspaceCsv
      where
        reactionNetworkNullspaceFilePath :: FilePath
        reactionNetworkNullspaceFilePath = "reaction_network/nullspace.csv"
        reactionNetworkNullspaceCsv :: ByteString
        reactionNetworkNullspaceCsv = MFAPipe.Csv.Types.NullspaceMatrix.encodeWith opts (NullspaceMatrixRecords ixs _mfaSpecReactionNetworkNullspace)

  -- "./emu_reaction_network/[EMU size]/reaction.csv"
  System.Log.Simple.debug "[4] EMU reaction network"
  let
    emuReactionNetworkEntryList :: [Entry]
    emuReactionNetworkEntryList = map (uncurry (\n -> Codec.Archive.Zip.toEntry (emuReactionNetworkFilePath n) modtime)) (Data.IntMap.Strict.toAscList emuReactionNetworkCsv)
      where
        emuReactionNetworkFilePath :: Int -> FilePath
        emuReactionNetworkFilePath = Text.Printf.printf "emu_reaction_network/%d/reaction.csv"
        emuReactionNetworkCsv :: IntMap ByteString
        emuReactionNetworkCsv = MFAPipe.Csv.Types.EMUReactionNetwork.encodeWith opts _mfaSpecEMUReactionNetwork

  -- "./emu_reaction_network/[EMU size]/stoichiometry.csv"
  System.Log.Simple.debug "[5] EMU stoichiometric model"
  let
    emuReactionNetworkDenseEntryList :: [Entry]
    emuReactionNetworkDenseEntryList = map (uncurry (\n -> Codec.Archive.Zip.toEntry (emuReactionNetworkDenseFilePath n) modtime)) (Data.IntMap.Strict.toAscList emuReactionNetworkDenseCsv)
      where
        emuReactionNetworkDenseFilePath :: Int -> FilePath
        emuReactionNetworkDenseFilePath = Text.Printf.printf "emu_reaction_network/%d/stoichiometry.csv"
        emuReactionNetworkDenseCsv :: IntMap ByteString
        emuReactionNetworkDenseCsv = Data.IntMap.Strict.map (MFAPipe.Csv.Types.Stoichiometry.encodeWith opts DenseEncodeAll) _mfaSpecEMUReactionNetworkDense

  -- "./result/flux.csv"
  System.Log.Simple.debug "[6] Metabolic fluxes"
  let
    fluxEntry :: Entry
    fluxEntry = Codec.Archive.Zip.toEntry fluxFilePath modtime fluxCsv
      where
        fluxFilePath :: FilePath
        fluxFilePath = "result/flux.csv"
        fluxCsv :: ByteString
        fluxCsv = MFAPipe.Csv.Types.Flux.encodeWeightedWith opts (WeightedFluxRecords (Data.Map.Strict.intersectionWith (,) _mfaResultFlux _mfaResultFluxVariance))

  -- "./result/flux_covariance.csv"
  System.Log.Simple.debug "[7] Metabolic flux covariances"
  let
    fluxCovarianceMatrixEntry :: Entry
    fluxCovarianceMatrixEntry = Codec.Archive.Zip.toEntry fluxCovarianceMatrixFilePath modtime fluxCovarianceMatrixCsv
      where
        fluxCovarianceMatrixFilePath :: FilePath
        fluxCovarianceMatrixFilePath = "result/flux_covariance.csv"
        fluxCovarianceMatrixCsv :: ByteString
        fluxCovarianceMatrixCsv = MFAPipe.Csv.Types.FluxCovarianceMatrix.encodeWith opts (FluxCovarianceMatrixRecords ixs _mfaResultFluxCovariance)

  -- "./result/info.csv"
  -- "./result/parameters.csv"
  System.Log.Simple.debug "[8] Fitting information"
  let
    infoEntry :: Entry
    infoEntry = Codec.Archive.Zip.toEntry infoFilePath modtime infoCsv
      where
        infoFilePath :: FilePath
        infoFilePath = "result/info.csv"
        infoCsv :: ByteString
        infoCsv = MFAPipe.Csv.Types.Info.encodeWith opts [_mfaResultInfo]
    paramsEntry :: Entry
    paramsEntry = Codec.Archive.Zip.toEntry paramsFilePath modtime paramsCsv
      where
        paramsFilePath :: FilePath
        paramsFilePath = "result/parameters.csv"
        paramsCsv :: ByteString
        paramsCsv = MFAPipe.Csv.Types.Parameters.encodeWith opts (ParametersRecords ixs params)
          where
            params = map (Data.Map.Strict.fromAscList . zip (Data.Set.toAscList ixs) . Numeric.LinearAlgebra.HMatrix.toList) [_mfaSpecInitialParams]

  -- "./result/contribution.csv"
  System.Log.Simple.debug "[9] \"Contribution\" matrix"
  let
    contributionMatrixEntry :: Entry
    contributionMatrixEntry = Codec.Archive.Zip.toEntry contributionMatrixFilePath modtime contributionMatrixCsv
      where
        contributionMatrixFilePath :: FilePath
        contributionMatrixFilePath = "result/contribution.csv"
        contributionMatrixCsv :: ByteString
        contributionMatrixCsv = MFAPipe.Csv.Types.ContributionMatrix.encodeWith opts (ContributionMatrixRecords ixs (Data.Map.Strict.intersectionWith (Data.Map.Strict.intersectionWith zip) measurements _mfaResultContributionMatrix))

  -- "./result/jacobian.csv"
  System.Log.Simple.debug "[10] Jacobian matrix"
  let
    jacobianMatrixEntry :: Entry
    jacobianMatrixEntry = Codec.Archive.Zip.toEntry jacobianMatrixFilePath modtime jacobianMatrixCsv
      where
        jacobianMatrixFilePath :: FilePath
        jacobianMatrixFilePath = "result/jacobian.csv"
        jacobianMatrixCsv :: ByteString
        jacobianMatrixCsv = MFAPipe.Csv.Types.JacobianMatrix.encodeWith opts (JacobianMatrixRecords ixs (Data.Map.Strict.intersectionWith (Data.Map.Strict.intersectionWith zip) measurements _mfaResultJacobianMatrix))

  -- "./result/statistics.csv"
  System.Log.Simple.debug "[11] Statistics"
  let
    statisticsEntry :: Entry
    statisticsEntry = Codec.Archive.Zip.toEntry statisticsFilePath modtime statisticsCsv
      where
        statisticsFilePath :: FilePath
        statisticsFilePath = Text.Printf.printf "result/statistics.csv"
        statisticsCsv :: ByteString
        statisticsCsv = MFAPipe.Csv.Types.Statistics.encodeWith opts (StatisticsRecords (_nullspaceTolerance_Inf _mfaSpecReactionNetworkNullspace) (_mfaResultResidualMeanVariance', _mfaResultResidualMeanVariance) (_mfaResultResidualKolmogorovSmirnov', _mfaResultResidualKolmogorovSmirnov) (_mfaResultResidualSSE', _mfaResultResidualSSE) (_mfaResultResidualSST', _mfaResultResidualSST))

  -- "./result/residual.csv"
  System.Log.Simple.debug "[12] Weighted residuals"
  let
    residualEntry :: Entry
    residualEntry = Codec.Archive.Zip.toEntry residualFilePath modtime residualCsv
      where
        residualFilePath :: FilePath
        residualFilePath = "result/residual.csv"
        residualCsv :: ByteString
        residualCsv = MFAPipe.Csv.Types.Residual.encodeWeightedWith opts (WeightedResidualRecords (Data.Map.Strict.intersectionWith (Data.Map.Strict.intersectionWith zip) measurements (Data.Map.Strict.intersectionWith (Data.Map.Strict.intersectionWith zip) _mfaResultResidual _mfaResultResidualVariance)))

  -- "./result/[experiment name]/[EMU size]/isotopomer_fraction.csv"
  -- "./result/[experiment name]/[EMU size]/mass_fraction.csv"
  System.Log.Simple.debug "[13] Isotopomer fractions"
  System.Log.Simple.debug "[14] Mass fractions"
  let
    fractionVectorEntryList :: [Entry]
    fractionVectorEntryList = concatMap (uncurry (\experimentName (Pair (FractionVectorDict Proxy mL _) (Pair (FractionVectorDict Proxy mR _) (Pair (Const ()) (Pair (Const ()) (Const ()))))) -> concat [isotopomerFractionVectorEntryList experimentName mL, massFractionVectorEntryList experimentName mR])) (Data.Map.Strict.toAscList _mfaResultDict)
      where
        isotopomerFractionVectorEntryList :: (Pretty k, Element e, ToField k, ToField e) => Text -> Map (EMU k) (IsotopomerFractionVector e) -> [Entry]
        isotopomerFractionVectorEntryList experimentName = map (uncurry (\n -> Codec.Archive.Zip.toEntry (isotopomerFractionVectorFilePath experimentName n) modtime)) . Data.IntMap.Strict.toAscList . isotopomerFractionVectorCsv
          where
            isotopomerFractionVectorFilePath :: Text -> Int -> FilePath
            isotopomerFractionVectorFilePath = Text.Printf.printf "result/%s/%d/isotopomer_fraction.csv" . Data.Text.unpack
            isotopomerFractionVectorCsv :: (Pretty k, Element e, ToField k, ToField e) => Map (EMU k) (IsotopomerFractionVector e) -> IntMap ByteString
            isotopomerFractionVectorCsv = MFAPipe.Csv.Types.IsotopomerFractionVector.encodeWith opts . IsotopomerFractionVectorRecords (fromInteger _mfaSpecRadix)
        massFractionVectorEntryList :: (Pretty k, Element e, ToField k, ToField e) => Text -> Map (EMU k) (MassFractionVector e) -> [Entry]
        massFractionVectorEntryList experimentName = map (uncurry (\n -> Codec.Archive.Zip.toEntry (massFractionVectorFilePath experimentName n) modtime)) . Data.IntMap.Strict.toAscList . massFractionVectorCsv
          where
            massFractionVectorFilePath :: Text -> Int -> FilePath
            massFractionVectorFilePath = Text.Printf.printf "result/%s/%d/mass_fraction.csv" . Data.Text.unpack
            massFractionVectorCsv :: (Pretty k, Element e, ToField k, ToField e) => Map (EMU k) (MassFractionVector e) -> IntMap ByteString
            massFractionVectorCsv = MFAPipe.Csv.Types.MassFractionVector.encodeWith opts . MassFractionVectorRecords (fromInteger _mfaSpecRadix)

  System.Log.Simple.info "CSV documents were serialized successfully!"

  -- "."
  System.Log.Simple.info "Constructing zip archive"
  let
    archive :: Archive
    archive = Archive
      { zEntries = concat
          [ [ reactionNetworkEntry
            , reactionNetworkDenseEntry
            , reactionNetworkNullspaceEntry
            , fluxEntry
            , fluxCovarianceMatrixEntry
            , infoEntry
            , paramsEntry
            , contributionMatrixEntry
            , jacobianMatrixEntry
            , statisticsEntry
            , residualEntry
            ]
          , emuReactionNetworkEntryList
          , emuReactionNetworkDenseEntryList
          , fractionVectorEntryList
          ]
      , zSignature = Nothing
      , zComment = Data.ByteString.Lazy.empty
      }
  System.Log.Simple.info "Zip archive was constructed successfully!"

  -- Fin!
  return archive

doKolmogorovSmirnov :: Sample -> (Double, Double)
doKolmogorovSmirnov xs =
  let
    d = Statistics.Test.KolmogorovSmirnov.kolmogorovSmirnovCdfD (Statistics.Distribution.cumulative dist) xs
    p = Statistics.Test.KolmogorovSmirnov.kolmogorovSmirnovProbability (Data.Vector.Unboxed.length xs) d
  in
    (d, p)
  where
    dist = Statistics.Distribution.Normal.normalDistr 0 1

concatModelAndJacobianE
  :: (Element a)
  => [Either e (Vector a, Matrix a)]
  -> Either [e] (Vector a, Matrix a)
concatModelAndJacobianE = uncurry go . Data.Either.partitionEithers
  where
    go
      :: (Element a)
      => [e]
      -> [(Vector a, Matrix a)]
      -> Either [e] (Vector a, Matrix a)
    go [] xs = Right (bimap Numeric.LinearAlgebra.HMatrix.vjoin (Numeric.LinearAlgebra.HMatrix.fromBlocks . map return) (unzip xs))
    go err _xs = Left err

getRandomParamsM :: (MonadRandom m, Container Vector e, Ord e, Numeric e, Random e) => MFAConstraints i e -> Matrix e -> ExceptT (MFAError i k a e) m (Vector e)
getRandomParamsM MFAConstraints{..} matrix0 = Numeric.LinearAlgebra.HMatrix.fromList <$> sequenceA (map lookupR (enumFromThenTo 0 1 (m - 1)))
  where
    _n, m :: Int
    ~(_n, m) = Numeric.LinearAlgebra.HMatrix.size matrix0
    -- lookupR :: IndexOf Vector -> ExceptT (MFAError i k a e) m e
    lookupR ix = case Data.Map.Strict.lookup ix _mfaConstraintsEqualities of
      Nothing -> do
        let
          lo = Data.Map.Strict.findWithDefault _mfaConstraintsDefaultLowerBound ix _mfaConstraintsLowerBounds
          hi = Data.Map.Strict.findWithDefault _mfaConstraintsDefaultUpperBound ix _mfaConstraintsUpperBounds
        Control.Monad.Random.Class.getRandomR (lo, hi)
      Just x -> do
        return x

-- | @mkLinearConstraints ks xs@ constructs a linear system @Ax = B@, where @A@ is a 'Matrix' and @B@ is a 'Vector'.
mkLinearConstraints :: (Ord k, Container Vector a, Element a, Num a) => [k] -> [(Map k a, a)] -> (Matrix a, Vector a)
mkLinearConstraints ks0 = bimap Numeric.LinearAlgebra.HMatrix.fromRows Numeric.LinearAlgebra.HMatrix.fromList . unzip . Data.Maybe.mapMaybe (uncurry (mkLinearConstraint ks0))
  where
    mkLinearConstraint :: (Ord k, Container Vector a, Num a) => [k] -> Map k a -> a -> Maybe (Vector a, a)
    mkLinearConstraint ks m x = fmap (flip (,) x) (mkVectorWithDefault ks Nothing 0 m)

-- | @mkVectorWithDefault ks mx0 x0 m@ attempts to construct a 'Vector' of the elements of @m@ with respect to @ks@, @mx0@ and @x0@.
mkVectorWithDefault :: (Ord k, Container Vector a) => [k] -> Maybe a -> a -> Map k a -> Maybe (Vector a)
mkVectorWithDefault ks mx0 x0 m
  | null ks = Nothing
  | Data.Map.Strict.null m = fmap (\x -> Numeric.LinearAlgebra.HMatrix.konst x (length ks)) mx0
  | otherwise = Just (Numeric.LinearAlgebra.HMatrix.fromList (map (\k -> Data.Map.Strict.findWithDefault x0 k m) ks))

-- | @partitionBounds m@ partitions the elements of @m@ into equalities, lower bounds and upper bounds, i.e.,
--
-- > let
-- >   (equs, lBs, uBs) = partitionBounds m
-- > in
-- >   ...
--
partitionBounds :: (Ord k) => Map k (FluxJS.Bound a) -> (Map k a, Map k a, Map k a)
partitionBounds = Data.Map.Strict.foldrWithKey cataWithKey (Data.Map.Strict.empty, Data.Map.Strict.empty, Data.Map.Strict.empty)
  where
    cataWithKey
      :: (Ord k)
      => k
      -> FluxJS.Bound a
      -> (Map k a, Map k a, Map k a)
      -> (Map k a, Map k a, Map k a)
    cataWithKey _ FluxJS.Free            acc              = acc
    cataWithKey k (FluxJS.EqualTo x)     (equs, lBs, uBs) = (Data.Map.Strict.insert k x equs,                            lBs,                            uBs)
    cataWithKey k (FluxJS.GreaterThan x) (equs, lBs, uBs) = (                           equs, Data.Map.Strict.insert k x lBs,                            uBs)
    cataWithKey k (FluxJS.LessThan x)    (equs, lBs, uBs) = (                           equs,                            lBs, Data.Map.Strict.insert k x uBs)
    cataWithKey k (FluxJS.Between x y)   (equs, lBs, uBs) = (                           equs, Data.Map.Strict.insert k x lBs, Data.Map.Strict.insert k y uBs)

fromMatrix
  :: (Element a)
  => [row]
  -> [column]
  -> Matrix a
  -> [(row, [(column, a)])]
fromMatrix rows cols = map (second (zip cols)) . zip rows . Numeric.LinearAlgebra.HMatrix.toLists

fromMatrixAsMap
  :: (Eq row, Eq column, Element a)
  => [row]
  -> [column]
  -> Matrix a
  -> Map row (Map column a)
fromMatrixAsMap rows cols = Data.Map.Strict.map Data.Map.Strict.fromAscList . Data.Map.Strict.fromAscList . fromMatrix rows cols

fromVector
  :: (Storable a)
  => [k]
  -> Vector a
  -> [(k, a)]
fromVector ks = zip ks . Numeric.LinearAlgebra.HMatrix.toList

fromVectorAsMap
  :: (Eq k, Storable a)
  => [k]
  -> Vector a
  -> Map k a
fromVectorAsMap ks = Data.Map.Strict.fromAscList . fromVector ks

fromListAsAscListMap
  :: (Ord k)
  => [e]
  -> Map k [(a, Int)]
  -> Map k [(a, [e])]
fromListAsAscListMap xs0 = fst . Data.Map.Strict.foldlWithKey' cataWithKey (Data.Map.Strict.empty, xs0)
  where
    cataWithKey :: (Ord k) => (Map k [(a, [e])], [e]) -> k -> [(a, Int)] -> (Map k [(a, [e])], [e])
    cataWithKey ~(acc, es) k as = let ~(aes, new_es) = foldl cata ([], es) as in (Data.Map.Strict.insert k aes acc, new_es)
    cata :: ([(a, [e])], [e]) -> (a, Int) -> ([(a, [e])], [e])
    cata ~(acc, es) (a, n) = first ((: acc) . (,) a) (Data.List.splitAt n es)
