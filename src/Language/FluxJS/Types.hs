{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.FluxJS.Types
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of FluxJS documents.
-----------------------------------------------------------------------------

module Language.FluxJS.Types
( Document(..)
, ModelMetadata(..)
, Model(..)
, ReactionNetwork(..)
, Reaction(..)
, ReactionType(..)
, Experiments(..)
, Experiment(..)
, IsotopicDistributions(..)
, IsotopicDistribution(..)
, WeightedMeasurements(..)
, WeightedMeasurement(..)
, Measurement(..)
, Constraints(..)
, Bounds(..)
, Weights(..)
, LinearConstraints(..)
, Bound(..)
, ObjectiveFunction(..)
) where

import           Data.Aeson (FromJSON(parseJSON), Value(..), (.:), (.:?))
import qualified Data.Aeson.Types
import           Data.Default (Default(def))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict
import           Data.Text (Text)
import qualified Data.Text

-- | A FluxJS document.
--
-- JSON:
--
-- > {
-- >   "model_metadata": {
-- >     ... // ModelMetadata
-- >   },
-- >   "model": {
-- >     ... // Model
-- >   }
-- > }
--
data Document a = Document ModelMetadata (Model a)
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | The metadata for a FluxJS model.
--
-- JSON:
--
-- > {
-- >   "key1": "value1",
-- >   "key2": "value2",
-- >   ...,
-- >   "keyN": "valueN"
-- > }
--
data ModelMetadata = ModelMetadata (HashMap Text Value)
  deriving (Eq, Read, Show)

-- | A FluxJS model.
--
-- JSON:
--
-- > {
-- >   "isotopic_labeling_states_per_atom": 2,
-- >   "reaction_network": {
-- >     ... // ReactionNetwork
-- >   },
-- >   "experiments": {
-- >     ... // Experiments
-- >   },
-- >   "constraints": {
-- >     ... // Constraints
-- >   }
-- > }
--
data Model a = Model (Maybe Integer) ReactionNetwork (Experiments a) (Constraints a)
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS chemical reaction network.
--
-- JSON:
--
-- > {
-- >   "fluxVar1": {
-- >     ... // Reaction
-- >   },
-- >   "fluxVar2": {
-- >     ... // Reaction
-- >   },
-- >   ...,
-- >   "fluxVarN": {
-- >     ... // Reaction
-- >   }
-- > }
--
data ReactionNetwork = ReactionNetwork (HashMap Text Reaction)
  deriving (Eq, Read, Show)

-- | A FluxJS chemical reaction.
--
-- JSON:
--
-- > {
-- >   "type": "...",
-- >   "reaction": "..."
-- > }
--
data Reaction = Reaction ReactionType Text
  deriving (Eq, Read, Show)

-- | A FluxJS chemical reaction type.
--
-- JSON:
--
-- > "mfa"
--
-- or
--
-- > "fba"
--
data ReactionType = MFAReactionType | FBAReactionType
  deriving (Eq, Read, Show)

-- | A list of FluxJS experiments.
--
-- JSON:
--
-- > {
-- >   "exp1": {
-- >     ... // Experiment
-- >   },
-- >   "exp2": {
-- >     ... // Experiment
-- >   },
-- >   ...,
-- >   "expN": {
-- >     ... // Experiment
-- >   }
-- > }
--
data Experiments a = Experiments (HashMap Text (Experiment a))
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS experiment.
--
-- JSON:
--
-- > {
-- >   "isotopic_distributions": {
-- >     ... // IsotopicDistributions
-- >   },
-- >   "measurements": {
-- >     ... // WeightedMeasurements
-- >   }
-- > }
--
data Experiment a = Experiment (IsotopicDistributions a) (WeightedMeasurements a)
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A list of FluxJS isotopic distributions.
--
-- JSON:
--
-- > {
-- >   "metaboliteVar1": [
-- >     ... // IsotopicDistribution
-- >   ],
-- >   "metaboliteVar1": [
-- >     ... // IsotopicDistribution
-- >   ],
-- >   ...,
-- >   "metaboliteVarN": [
-- >     ... // IsotopicDistribution
-- >   ]
-- > }
--
data IsotopicDistributions a = IsotopicDistributions (HashMap Text [IsotopicDistribution a])
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS isotopic distribution.
--
-- JSON:
--
-- > {
-- >   "%": 100,
-- >   "isotopic_distribution": [
-- >     [0, 0],
-- >     [0, 1],
-- >     ...,
-- >     [1, 1]
-- >   ]
-- > }
--
data IsotopicDistribution a = IsotopicDistribution a [[a]]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A list of FluxJS weighted measurements.
--
-- JSON:
--
-- > {
-- >   "expression1": [
-- >     ... // WeightedMeasurement
-- >   ],
-- >   "expression2": [
-- >     ... // WeightedMeasurement
-- >   ],
-- >   ...,
-- >   "expressionN": [
-- >     ... // WeightedMeasurement
-- >   ]
-- > }
--
data WeightedMeasurements a = WeightedMeasurements (HashMap Text [WeightedMeasurement a])
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS weighted measurement.
--
-- JSON:
--
-- > {
-- >   "=": 3.14159265359,
-- >   "weight": 1
-- > }
--
data WeightedMeasurement a = WeightedMeasurement a a
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS measurement (unweighted).
--
-- JSON:
--
-- > {
-- >   "=": 3.14159265359
-- > }
--
data Measurement a = Measurement a
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A set of constraints for a FluxJS model.
--
-- JSON:
--
-- > {
-- >   "objective_function": {
-- >     ... // ObjectiveFunction
-- >   },
-- >   "bounds": {
-- >     ... // Bounds
-- >   }
-- >   "weights": {
-- >     ... // Weights
-- >   }
-- >   "linear_constraints": {
-- >     ... // LinearConstraints
-- >   }
-- >   "default_lower_bound_maybe": 0,
-- >   "default_lower_bound": 0,
-- >   "default_upper_bound_maybe": null,
-- >   "default_upper_bound": 1000,
-- >   "default_weight_maybe": 1,
-- >   "default_weight": 1
-- > }
-- 
data Constraints a = Constraints (Maybe ObjectiveFunction) (Bounds a) (Weights a) (LinearConstraints a) (Maybe a) a (Maybe a) a (Maybe a) a
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A set of FluxJS bounds.
--
-- JSON:
--
-- > {
-- >   "fluxVar1": {
-- >     ... // Bound
-- >   },
-- >   "fluxVar2": {
-- >     ... // Bound
-- >   },
-- >   ...,
-- >   "fluxVarN": {
-- >     ... // Bound
-- >   }
-- > }
--
data Bounds a = Bounds (HashMap Text (Bound a))
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A set of FluxJS weights.
--
-- JSON:
--
-- > {
-- >   "fluxVar1": {
-- >     ... // Measurement
-- >   },
-- >   "fluxVar2": {
-- >     ... // Measurement
-- >   },
-- >   ...,
-- >   "fluxVarN": {
-- >     ... // Measurement
-- >   }
-- > }
-- 
data Weights a = Weights (HashMap Text (Measurement a))
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | A set of FluxJS linear constraints.
--
-- JSON:
--
-- > {
-- >   "expression1": {
-- >     ... // Bound
-- >   },
-- >   "expression2": {
-- >     ... // Bound
-- >   },
-- >   ...,
-- >   "expressionN": {
-- >     ... // Bound
-- >   }
-- > }
--
data LinearConstraints a = LinearConstraints (HashMap Text (Bound a))
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | The bounds for a FluxJS measurement or constraint.
--
-- JSON:
--
-- > {
-- >   "=": ...
-- > }
--
-- or
--
-- > {
-- >   ">=": ...,
-- >   "<=": ...
-- > }
--
data Bound a
  = Free
  | EqualTo a
  | GreaterThan a
  | LessThan a
  | Between a a
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A FluxJS objective function.
--
-- JSON:
--
-- > {
-- >   "max": "expression"
-- > }
--
-- or
--
-- > {
-- >   "min": "expression"
-- > }
--
data ObjectiveFunction
  = Maximize Text
  | Minimize Text
  deriving (Eq, Ord, Read, Show)

instance (Fractional a) => Default (Document a) where
  def = Document def def
  {-# INLINE def #-}

instance Default ModelMetadata where
  def = ModelMetadata Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance (Fractional a) => Default (Model a) where
  def = Model Nothing def def def
  {-# INLINE def #-}

instance Default ReactionNetwork where
  def = ReactionNetwork Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance Default (Experiments a) where
  def = Experiments Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance Default (Experiment a) where
  def = Experiment def def
  {-# INLINE def #-}

instance Default (IsotopicDistributions a) where
  def = IsotopicDistributions Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance Default (WeightedMeasurements a) where
  def = WeightedMeasurements Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance (Num a) => Default (Constraints a) where
  def = Constraints
    Nothing
    def
    def
    def
    defLowerBoundMaybe
    defLowerBound
    defUpperBoundMaybe
    defUpperBound
    defWeightMaybe
    defWeight
  {-# INLINE def #-}

instance Default (Bounds a) where
  def = Bounds Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance Default (Weights a) where
  def = Weights Data.HashMap.Strict.empty
  {-# INLINE def #-}

instance Default (LinearConstraints a) where
  def = LinearConstraints Data.HashMap.Strict.empty
  {-# INLINE def #-}

-- | The default lower bound for metabolic flux variables.
--
-- Note: Only used when /zero/ lower bounds are specified.
defLowerBoundMaybe :: (Num a) => Maybe a
defLowerBoundMaybe = Just defLowerBound
{-# INLINE defLowerBoundMaybe #-}

-- | The default lower bound for metabolic flux variables.
--
-- Note: Only used when /one/ or more lower bounds are specified.
defLowerBound :: (Num a) => a
defLowerBound = 0
{-# INLINE defLowerBound #-}

-- | The default upper bound for metabolic flux variables.
--
-- Note: Only used when /zero/ upper bounds are specified.
defUpperBoundMaybe :: (Num a) => Maybe a
defUpperBoundMaybe = Nothing
{-# INLINE defUpperBoundMaybe #-}

-- | The default upper bound for metabolic flux variables.
--
-- Note: Only used when /one/ or more upper bounds are specified.
defUpperBound :: (Num a) => a
defUpperBound = 1000000
{-# INLINE defUpperBound #-}

-- | The default weight for metabolic flux variables.
--
-- Note: Only used when /zero/ weights are specified.
defWeightMaybe :: (Num a) => Maybe a
defWeightMaybe = Nothing
{-# INLINE defWeightMaybe #-}

-- | The default weight for metabolic flux variables.
--
-- Note: Only used when /one/ or more weights are specified.
defWeight :: (Num a) => a
defWeight = 1
{-# INLINE defWeight #-}

instance (Fractional a, FromJSON a) => FromJSON (Document a) where
  parseJSON (Object v) =
        Document
    <$> v .: Data.Text.pack "model_metadata"
    <*> v .: Data.Text.pack "model"
  parseJSON x = Data.Aeson.Types.typeMismatch "Document" x

instance FromJSON ModelMetadata where
  parseJSON x =
        ModelMetadata
    <$> parseJSON x

instance (Fractional a, FromJSON a) => FromJSON (Model a) where
  parseJSON (Object v) =
        Model
    <$> v .:? Data.Text.pack "isotopic_labeling_states_per_atom"
    <*> fmap (maybe def id) (v .:? Data.Text.pack "reaction_network")
    <*> fmap (maybe def id) (v .:? Data.Text.pack "experiments")
    <*> fmap (maybe def id) (v .:? Data.Text.pack "constraints")
  parseJSON x = Data.Aeson.Types.typeMismatch "Model" x

instance FromJSON ReactionNetwork where
  parseJSON x =
        ReactionNetwork
    <$> parseJSON x

instance FromJSON Reaction where
  parseJSON (Object v) =
        Reaction
    <$> v .: Data.Text.pack "type"
    <*> v .: Data.Text.pack "reaction"
  parseJSON x = Data.Aeson.Types.typeMismatch "Reaction" x

instance FromJSON ReactionType where
  parseJSON (String t)
    | t == Data.Text.pack "mfa" = pure MFAReactionType
    | t == Data.Text.pack "fba" = pure FBAReactionType
    | otherwise = fail "expecting \"mfa\" or \"fba\""
  parseJSON x = Data.Aeson.Types.typeMismatch "ReactionType" x

instance (Fractional a, FromJSON a) => FromJSON (Experiments a) where
  parseJSON x =
        Experiments
    <$> parseJSON x

instance (Fractional a, FromJSON a) => FromJSON (Experiment a) where
  parseJSON (Object v) =
        Experiment
    <$> fmap (maybe def id) (v .:? Data.Text.pack "isotopic_distributions")
    <*> fmap (maybe def id) (v .:? Data.Text.pack "measurements")
  parseJSON x = Data.Aeson.Types.typeMismatch "Experiment" x

instance (Fractional a, FromJSON a) => FromJSON (IsotopicDistributions a) where
  parseJSON x =
        IsotopicDistributions
    <$> parseJSON x

instance (Fractional a, FromJSON a) => FromJSON (IsotopicDistribution a) where
  parseJSON (Object v) =
        IsotopicDistribution
    <$> fmap (/ 100) (fmap (maybe 100 id) (v .:? Data.Text.singleton '%'))
    <*> v .: Data.Text.pack "isotopic_distribution"
  parseJSON x = Data.Aeson.Types.typeMismatch "IsotopicDistribution" x

instance (Num a, FromJSON a) => FromJSON (WeightedMeasurements a) where
  parseJSON x =
        WeightedMeasurements
    <$> parseJSON x

instance (Num a, FromJSON a) => FromJSON (WeightedMeasurement a) where
  parseJSON (Object v) =
        WeightedMeasurement
    <$> v .: Data.Text.singleton '='
    <*> fmap (maybe defWeight id) (v .:? Data.Text.pack "weight")
  parseJSON x = Data.Aeson.Types.typeMismatch "WeightedMeasurement" x

instance (FromJSON a) => FromJSON (Measurement a) where
  parseJSON (Object v) =
        Measurement
    <$> v .: Data.Text.singleton '='
  parseJSON x = Data.Aeson.Types.typeMismatch "Measurement" x

instance (Num a, FromJSON a) => FromJSON (Constraints a) where
  parseJSON (Object v) =
        Constraints
    <$> v .:? Data.Text.pack "objective_function"
    <*> fmap (maybe def id) (v .:? Data.Text.pack "bounds")
    <*> fmap (maybe def id) (v .:? Data.Text.pack "weights")
    <*> fmap (maybe def id) (v .:? Data.Text.pack "linear_constraints")
    <*> fmap (maybe defLowerBoundMaybe id) (v .:? Data.Text.pack "default_lower_bound_maybe")
    <*> fmap (maybe defLowerBound id) (v .:? Data.Text.pack "default_lower_bound")
    <*> fmap (maybe defUpperBoundMaybe id) (v .:? Data.Text.pack "default_upper_bound_maybe")
    <*> fmap (maybe defUpperBound id) (v .:? Data.Text.pack "default_upper_bound")
    <*> fmap (maybe defWeightMaybe id) (v .:? Data.Text.pack "default_weight_maybe")
    <*> fmap (maybe defWeight id) (v .:? Data.Text.pack "default_weight")
  parseJSON x = Data.Aeson.Types.typeMismatch "Constraints" x

instance (FromJSON a) => FromJSON (Bounds a) where
  parseJSON x = Bounds <$> parseJSON x

instance (FromJSON a) => FromJSON (Weights a) where
  parseJSON x = Weights <$> parseJSON x

instance (FromJSON a) => FromJSON (LinearConstraints a) where
  parseJSON x = LinearConstraints <$> parseJSON x

instance (FromJSON a) => FromJSON (Bound a) where
  parseJSON (Object v) = case (Data.HashMap.Strict.lookup (Data.Text.singleton '=') v, Data.HashMap.Strict.lookup (Data.Text.pack ">=") v, Data.HashMap.Strict.lookup (Data.Text.pack "<=") v) of
    (Just equ, Nothing, Nothing) ->
          EqualTo
      <$> parseJSON equ
    (Nothing, Just lB, Nothing) ->
          GreaterThan
      <$> parseJSON lB
    (Nothing, Nothing, Just uB) ->
          LessThan
      <$> parseJSON uB
    (Nothing, Just lB, Just uB) ->
          Between
      <$> parseJSON lB
      <*> parseJSON uB
    (Nothing, Nothing, Nothing) ->
      pure Free
    _ -> fail "invalid bounds"
  parseJSON x = Data.Aeson.Types.typeMismatch "Bound" x

instance FromJSON ObjectiveFunction where
  parseJSON (Object v) = case (Data.HashMap.Strict.lookup (Data.Text.pack "max") v, Data.HashMap.Strict.lookup (Data.Text.pack "min") v) of
    (Just text, Nothing) -> Maximize <$> parseJSON text
    (Nothing, Just text) -> Minimize <$> parseJSON text
    _ -> fail "invalid objective function"
  parseJSON x = Data.Aeson.Types.typeMismatch "ObjectiveFunction" x
