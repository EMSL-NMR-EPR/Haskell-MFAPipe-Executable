-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.Csv.Constants
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports constants for comma-separated values (CSV) documents.
-----------------------------------------------------------------------------

module MFAPipe.Csv.Constants where

import           Data.ByteString (ByteString)
import qualified Data.Text
import qualified Data.Text.Encoding

-- | The field name for degrees of freedom.
cDegreesOfFreedomFieldName :: Data.ByteString.ByteString
cDegreesOfFreedomFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Degrees of Freedom")
{-# INLINE cDegreesOfFreedomFieldName #-}

-- | The field name for EMUs.
cEMUFieldName :: Data.ByteString.ByteString
cEMUFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "EMU")
{-# INLINE cEMUFieldName #-}

-- | The field name for the mixing probability of an EMU reaction.
cEMUReactionMixingProbabilityFieldName :: ByteString
cEMUReactionMixingProbabilityFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "EMU Reaction Probability")
{-# INLINE cEMUReactionMixingProbabilityFieldName #-}

-- | The field name for the left-hand side of an EMU reaction.
cEMUReactionSideLeftFieldName :: ByteString
cEMUReactionSideLeftFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "EMU Reaction Substrate")
{-# INLINE cEMUReactionSideLeftFieldName #-}

-- | The field name for the right-hand side of an EMU reaction.
cEMUReactionSideRightFieldName :: ByteString
cEMUReactionSideRightFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "EMU Reaction Product")
{-# INLINE cEMUReactionSideRightFieldName #-}

-- | The field name for experiments.
cExperimentFieldName :: ByteString
cExperimentFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Experiment")
{-# INLINE cExperimentFieldName #-}

-- | The field name for mean of weighted residuals of experiments.
cExperimentMeanFieldName :: ByteString
cExperimentMeanFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Experiment Mean")
{-# INLINE cExperimentMeanFieldName #-}

-- | The field name for variance of weighted residuals of experiments.
cExperimentVarianceFieldName :: ByteString
cExperimentVarianceFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Experiment Variance")
{-# INLINE cExperimentVarianceFieldName #-}

-- | The field name for metabolic flux variables.
cFluxVarFieldName :: ByteString
cFluxVarFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Metabolic Flux Variable")
{-# INLINE cFluxVarFieldName #-}

-- | The field name for values of metabolic flux variables.
cFluxValueFieldName :: ByteString
cFluxValueFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Metabolic Flux")
{-# INLINE cFluxValueFieldName #-}

-- | The field name for variances of values of metabolic flux variables.
cFluxVarianceFieldName :: ByteString
cFluxVarianceFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Metabolic Flux Variance")
{-# INLINE cFluxVarianceFieldName #-}

-- | The field name for free variables.
cFreeVarFieldName :: ByteString
cFreeVarFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Independent Variable")
{-# INLINE cFreeVarFieldName #-}

-- | The field name for variances of values of independent metabolic flux variables counts.
cIndependentFluxesCount :: ByteString
cIndependentFluxesCount = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Independent Variables Count")
{-# INLINE cIndependentFluxesCount #-}

-- | The field name for variances of values of independent measurement counts.
cIndependentMeasurementsCount :: ByteString
cIndependentMeasurementsCount = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Independent Measurements Count")
{-# INLINE cIndependentMeasurementsCount #-}

-- | The field naem for Kolmogorov-Smirnov statistics.
cKolmogorovSmirnovDFieldName :: ByteString
cKolmogorovSmirnovDFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Kolmogorov-Smirnov \"D\" Statistic")
{-# INLINE cKolmogorovSmirnovDFieldName #-}

-- | The field naem for Kolmogorov-Smirnov probabilities.
cKolmogorovSmirnovProbabilityFieldName :: ByteString
cKolmogorovSmirnovProbabilityFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Kolmogorov-Smirnov Probability")
{-# INLINE cKolmogorovSmirnovProbabilityFieldName #-}

-- | The field name for measurements.
cMeasurementFieldName :: ByteString
cMeasurementFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Measurement")
{-# INLINE cMeasurementFieldName #-}

-- | The field name for values of measurements.
cMeasurementValueFieldName :: ByteString
cMeasurementValueFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Measurement Value")
{-# INLINE cMeasurementValueFieldName #-}

-- | The field name for weights of measurements.
cMeasurementWeightFieldName :: ByteString
cMeasurementWeightFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Measurement Weight")
{-# INLINE cMeasurementWeightFieldName #-}

-- | The field name for objective functions.
cObjectiveFunctionFieldName :: ByteString
cObjectiveFunctionFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Objective Function")
{-# INLINE cObjectiveFunctionFieldName #-}

-- | The field name for the direction of an objective function.
cObjectiveFunctionDirectionFieldName :: ByteString
cObjectiveFunctionDirectionFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Objective Function Direction")
{-# INLINE cObjectiveFunctionDirectionFieldName #-}

cObjectiveFunctionDirectionMin :: ByteString
cObjectiveFunctionDirectionMin = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "min")
{-# INLINE cObjectiveFunctionDirectionMin #-}

cObjectiveFunctionDirectionMax :: ByteString
cObjectiveFunctionDirectionMax = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "max")
{-# INLINE cObjectiveFunctionDirectionMax #-}

-- | The field name for values of objective functions.
cObjectiveFunctionValueFieldName :: ByteString
cObjectiveFunctionValueFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Objective Function Value")
{-# INLINE cObjectiveFunctionValueFieldName #-}

-- | The field name for chemical reactions.
cReactionFieldName :: ByteString
cReactionFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Reaction")
{-# INLINE cReactionFieldName #-}

-- | The field name for chemical reaction types.
cReactionTypeFieldName :: ByteString
cReactionTypeFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Reaction Type")

cReactionTypeMFA :: ByteString
cReactionTypeMFA = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "mfa")
{-# INLINE cReactionTypeMFA #-}

cReactionTypeFBA :: ByteString
cReactionTypeFBA = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "fba")
{-# INLINE cReactionTypeFBA #-}

-- | The field name for reduced chi-square.
cReducedChiSquare :: ByteString
cReducedChiSquare = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Reduced Chi-Square")
{-# INLINE cReducedChiSquare #-}

cTolerance :: ByteString
cTolerance = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Tolerance (Uniform Norm)")
{-# INLINE cTolerance #-}

-- | The field name for values of measurements, i.e., weighted residuals.
cWeightedResidualValueFieldName :: ByteString
cWeightedResidualValueFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted Residual")
{-# INLINE cWeightedResidualValueFieldName #-}

-- | The field name for variances of values of measurements, i.e., variances of weighted residuals.
cWeightedResidualVarianceFieldName :: ByteString
cWeightedResidualVarianceFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted Residual Variance")
{-# INLINE cWeightedResidualVarianceFieldName #-}

cWeightedRSquareFieldName :: ByteString
cWeightedRSquareFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted R-Square")
{-# INLINE cWeightedRSquareFieldName #-}

cWeightedRSquareAdjustedFieldName :: ByteString
cWeightedRSquareAdjustedFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted R-Square (Adjusted)")
{-# INLINE cWeightedRSquareAdjustedFieldName #-}

cWeightedSSEFieldName :: ByteString
cWeightedSSEFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted SSE")
{-# INLINE cWeightedSSEFieldName #-}

cWeightedSSTFieldName :: ByteString
cWeightedSSTFieldName = Data.Text.Encoding.encodeUtf8 (Data.Text.pack "Weighted SST")
{-# INLINE cWeightedSSTFieldName #-}
