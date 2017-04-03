{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.FluxJS.Conversion.Text
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports functions for conversion of types for the
-- representation of FluxJS documents.
-----------------------------------------------------------------------------

module Language.FluxJS.Conversion.Text
( -- * FluxJSError type
  FluxJSError(..)
  -- * Conversion
  -- ** Chemical reactions
, fromFluxJSReaction
  -- ** Chemical reaction networks
, fromFluxJSReactionNetwork
  -- ** Experiments
, fromFluxJSExperiment , fromFluxJSExperiments
  -- ** Isotopic distributions
, fromFluxJSIsotopicDistribution , fromFluxJSIsotopicDistributions
  -- ** Weighted measurements
, fromFluxJSWeightedMeasurement , fromFluxJSWeightedMeasurements
  -- ** Unweighted measurements
, fromFluxJSMeasurement
  -- ** Linear constraints
, fromFluxJSLinearConstraint
  -- ** Metabolic flux variables
, fromFluxJSFluxVar
  -- ** Metabolite variables
, fromFluxJSMetaboliteVar
) where

import           Control.Applicative (liftA2)
import           Control.Monad (liftM2)
import           Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text
import qualified Data.HashMap.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Text (Text)
import qualified Data.Text
import qualified Language.FluxJS.Types as FluxJS
import           Language.INCA.Conversion.Text
import           Numeric.LinearAlgebra.HMatrix (Matrix)
import qualified Numeric.LinearAlgebra.HMatrix
import           Science.Chemistry.EMU.IsotopicLabeling.DSL.Evaluate.Class
import           Science.Chemistry.FluxVar
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.Parser.Text
import           Science.Chemistry.ReactionNetwork

-- | An error that occurs during the parsing or conversion of FluxJS documents.
data FluxJSError a
  = FluxJSLinearConstraintBoundIsNotEqualTo a
  -- ^ In the MFA context, 'FluxJS.Bound' for \"linear_constraints\" attribute
  -- of 'FluxJS.Constraints' is not 'FluxJS.EqualTo'.
  | FluxJSIsotopicLabelingStatesPerAtomIsBlank
  -- ^ In the MFA context, the \"isotopic_labeling_states_per_atom\" attribute
  -- of 'FluxJS.Model' is not specified.
  | FluxJSIsotopicLabelingStatesPerAtomIsInvalid {-# UNPACK #-} !Int
  -- ^ In the MFA context, the \"isotopic_labeling_states_per_atom\" attribute
  -- is zero or negative.
  | FluxJSExperimentNameIsInvalid a
  -- ^ The name of a 'FluxJS.Experiment' is invalid.
  | FluxJSObjectiveFunctionIsBlank
  -- ^ In the FBA context, the 'FluxJS.ObjectiveFunction' for
  -- 'FluxJS.Constraints' is not specified.
  | FluxJSParserError a String
  -- ^ Parser error (generic).
  deriving (Eq, Ord, Read, Show)

-- | Convert a FluxJS chemical reaction to a 'ReactionNetwork'.
--
-- See also: 'parseMFAReaction' and 'parseFBAReaction'.
fromFluxJSReaction
  :: (Monad m, i ~ FluxVar Text Text, k ~ MetaboliteVar Text)
  => Text
  -- ^ name for metabolic flux variables
  -> FluxJS.Reaction
  -- ^ chemical reaction
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (ReactionNetwork i k Text)
  -- ^ result
fromFluxJSReaction fluxVarName (FluxJS.Reaction FluxJS.MFAReactionType text) = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError fluxVarName) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly (parseMFAReaction fluxVarName) text)
fromFluxJSReaction fluxVarName (FluxJS.Reaction FluxJS.FBAReactionType text) = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError fluxVarName) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly (parseFBAReaction fluxVarName) text)

-- | Convert a FluxJS chemical reaction network to a 'ReactionNetwork'
--
-- See also: 'fromFluxJSReaction'.
fromFluxJSReactionNetwork
  :: (Monad m, i ~ FluxVar Text Text, k ~ MetaboliteVar Text)
  => FluxJS.ReactionNetwork
  -- ^ chemical reaction network
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (ReactionNetwork i k Text)
  -- ^ result
fromFluxJSReactionNetwork (FluxJS.ReactionNetwork m) = fmap mconcat (mapM (uncurry fromFluxJSReaction) (Data.HashMap.Strict.toList m))

-- | Convert a FluxJS metabolite flux variable to a 'FluxVar'.
--
-- See also: 'parseFluxVar'.
fromFluxJSFluxVar
  :: (Monad m, i ~ FluxVar Text Text)
  => Text
  -- ^ text
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m i
  -- ^ result
fromFluxJSFluxVar fluxVarName = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError fluxVarName) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly parseFluxVar fluxVarName)

-- | Convert a FluxJS linear constraint.
--
-- See also: 'parseLinearConstraint'.
fromFluxJSLinearConstraint
  :: (Monad m, i ~ FluxVar Text Text, e ~ Double)
  => Text
  -- ^ text
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map i e)
  -- ^ result
fromFluxJSLinearConstraint text = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError text) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly parseLinearConstraint text)

-- | Convert a FluxJS metabolite variable to a 'MetaboliteVar'.
--
-- See also: 'parseMetaboliteVar'.
fromFluxJSMetaboliteVar
  :: (Monad m, k ~ MetaboliteVar Text)
  => Text
  -- ^ text
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m k
  -- ^ result
fromFluxJSMetaboliteVar metaboliteVarName = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError metaboliteVarName) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly parseMetaboliteVar metaboliteVarName)

-- | Convert a FluxJS expression to a 'EMUExpr' with respect to a base.
--
-- See also: 'parseEMUExpr'.
fromFluxJSEMUExpr
  :: (Monad m, k ~ MetaboliteVar Text)
  => Int
  -- ^ base
  -> Text
  -- ^ text
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (EMUExpr k)
  -- ^ result
fromFluxJSEMUExpr base text = either (Control.Monad.Trans.Except.throwE . Right . FluxJSParserError text) (Control.Monad.Trans.Except.withExceptT Left) (Data.Attoparsec.Text.parseOnly (parseEMUExpr base) text)

-- | Convert a FluxJS single-labeling experiment with respect to a base.
--
-- See also: 'fromFluxJSIsotopicDistributions' and 'fromFluxJSWeightedMeasurements'.
fromFluxJSExperiment
  :: (Monad m, k ~ MetaboliteVar Text, e ~ Double)
  => Int
  -- ^ base
  -> FluxJS.Experiment e
  -- ^ experiment
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map k [(Rational, Matrix e)], Map (EMUExpr k) [(e, e)])
  -- ^ result
fromFluxJSExperiment base (FluxJS.Experiment isotopicDistributions measurements) = liftA2 (,) (fromFluxJSIsotopicDistributions base isotopicDistributions) (fromFluxJSWeightedMeasurements base measurements)

-- | Convert a FluxJS single-labeling experiment and name with respect to a base.
--
-- Note: the experiment name must match the regular expression @[a-zA-Z0-9][a-zA-Z0-9-_]*@.
fromFluxJSExperimentWithName
  :: (Monad m, k ~ MetaboliteVar Text, e ~ Double)
  => Int
  -- ^ base
  -> Text
  -- ^ experiment name
  -> FluxJS.Experiment e
  -- ^ experiment
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Text, (Map k [(Rational, Matrix e)], Map (EMUExpr k) [(e, e)]))
  -- ^ result
fromFluxJSExperimentWithName base experimentName = liftM2 (,) (either (\_ -> Control.Monad.Trans.Except.throwE (Right (FluxJSExperimentNameIsInvalid experimentName))) return (Data.Attoparsec.Text.parseOnly parseExperimentName (Data.Text.strip experimentName))) . fromFluxJSExperiment base
  where
    parseExperimentName :: Parser Text
    parseExperimentName = do
      x <- Data.Attoparsec.Text.satisfy (Data.Attoparsec.Text.inClass "a-zA-Z0-9")
      xs <- Data.Attoparsec.Text.takeWhile (Data.Attoparsec.Text.inClass "a-zA-Z0-9-_")
      return (Data.Text.cons x xs)

-- | Convert a FluxJS parallel-labeling experiment with respect to a base.
--
-- See also: 'fromFluxJSExperiment'.
fromFluxJSExperiments
  :: (Monad m, k ~ MetaboliteVar Text, e ~ Double)
  => Int
  -- ^ base
  -> FluxJS.Experiments e
  -- ^ experiments
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map Text (Map k [(Rational, Matrix e)], Map (EMUExpr k) [(e, e)]))
  -- ^ result
fromFluxJSExperiments base (FluxJS.Experiments m) = fmap Data.Map.Strict.fromList (mapM (uncurry (fromFluxJSExperimentWithName base)) (Data.HashMap.Strict.toList m))

-- | Convert a FluxJS isotopic distribution with respect to a base.
--
-- Note: The result is right-padded.
fromFluxJSIsotopicDistribution
  :: (e ~ Double)
  => Int
  -- ^ base
  -> FluxJS.IsotopicDistribution e
  -- ^ isotopic distribution
  -> (Rational, Matrix e)
  -- ^ result
fromFluxJSIsotopicDistribution base (FluxJS.IsotopicDistribution p xss) = (toRational p, Numeric.LinearAlgebra.HMatrix.fromLists (map (padRight 0 base) xss))

-- | Convert a FluxJS isotopic distribution for a mixture with respect to a base.
--
-- Note: The result is right-padded.
--
-- See also: 'fromFluxJSIsotopicDistribution'.
fromFluxJSIsotopicDistributions
  :: (Monad m, k ~ MetaboliteVar Text, e ~ Double)
  => Int
  -- ^ base
  -> FluxJS.IsotopicDistributions e
  -- ^ isotopic distributions
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map k [(Rational, Matrix e)])
  -- ^ result
fromFluxJSIsotopicDistributions base (FluxJS.IsotopicDistributions m) = fmap Data.Map.Strict.fromList (mapM (uncurry (\text xs -> fromFluxJSMetaboliteVar text >>= return . flip (,) (map (fromFluxJSIsotopicDistribution base) xs))) (Data.HashMap.Strict.toList m))

-- | Convert a FluxJS weighted measurement.
fromFluxJSWeightedMeasurement
  :: (e ~ Double)
  => FluxJS.WeightedMeasurement e
  -- ^ weighted measurement
  -> (e, e)
  -- ^ result
fromFluxJSWeightedMeasurement (FluxJS.WeightedMeasurement value weight) = (value, weight)

-- | Convert a set of FluxJS weighted measurements with respect to a base.
--
-- See also: 'fromFluxJSWeightedMeasurement'.
fromFluxJSWeightedMeasurements
  :: (Monad m, k ~ MetaboliteVar Text, e ~ Double)
  => Int
  -- ^ base
  -> FluxJS.WeightedMeasurements e
  -- ^ weighted measurements
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map (EMUExpr k) [(e, e)])
  -- ^ result
fromFluxJSWeightedMeasurements base (FluxJS.WeightedMeasurements m) = fmap Data.Map.Strict.fromList (mapM (uncurry (\text xs -> fromFluxJSEMUExpr base text >>= return . flip (,) (map fromFluxJSWeightedMeasurement xs))) (Data.HashMap.Strict.toList m))

-- | Convert a FluxJS unweighted measurement.
fromFluxJSMeasurement
  :: (e ~ Double)
  => FluxJS.Measurement e
  -- ^ unweighted measurement
  -> e
  -- ^ result
fromFluxJSMeasurement (FluxJS.Measurement x) = x

-- ----------------------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------------------

-- | @padRight x0 n xs@ pads @xs@ with @x0@ until @'length' xs '==' n@.
padRight :: a -> Int -> [a] -> [a]
padRight x n xs
  | diff > 0 = xs ++ replicate diff x
  | otherwise = xs
  where
    diff :: Int
    diff = n - length xs
