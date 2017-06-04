{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.INCA.Conversion.Text
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports functions for conversion of types for the
-- representation of INCA syntax.
-----------------------------------------------------------------------------

module Language.INCA.Conversion.Text
( -- * INCAError type
  INCAError(..)
  -- * Conversion
  -- ** Chemical reactions
, fromINCAMFAReaction
, fromINCAFBAReaction
  -- ** Metabolic flux variables
, fromINCAFluxVar
, fromINCALinearConstraint
  -- ** Metabolite variables
, fromINCAMetaboliteVar
  -- ** Elementary Metabolite Units (EMU)
, fromINCAEMU
  -- ** Fractions
, fromINCAIsotopomerFraction
, fromINCAMassFraction
) where

import           Control.Monad (unless, when)
import           Control.Monad.Error.Class (MonadError())
import qualified Control.Monad.Error.Class
import qualified Data.IntSet
import qualified Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Monoid (Product(..), Sum(..))
import           Data.Text (Text)
import qualified Data.Text
import           Language.INCA.Constants
import           Language.INCA.Types
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.IsotopicLabeling.DSL.Evaluate.Class
import           Science.Chemistry.FluxVar
import           Science.Chemistry.IsotopicLabeling.FractionVector
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.Reaction
import           Science.Chemistry.ReactionNetwork
import           Science.Chemistry.Types

-- | An error that occurs during the parsing or conversion of INCA syntax.
data INCAError a
  = INCAMFAReactionIsInvalid a (INCAReaction (INCAMetaboliteVar a) a) (INCAError a)
  -- ^ 'INCAReaction' is invalid in MFA context.
  | INCAMFAReactionPartHasInvalidMixingProbability Rational (INCAReactionPart (INCAMetaboliteVar a) a)
  -- ^ In the MFA context, the sum of the 'MixingProbability' for an 'INCAReactionPart'
  -- must be @'==' 1@.
  | INCAFBAReactionIsInvalid a (INCAReaction (INCAMetaboliteVar a) a) (INCAError a)
  -- ^ 'INCAReaction' is invalid in FBA context.
  | INCAFluxVarNameIsInvalid (INCAFluxVar a)
  -- ^ The name of an 'INCAFluxVar' is empty or does not begin with \"v\" or
  -- \"b\".
  | INCAFluxVarDirectionIsInvalid (INCAFluxVar a)
  -- ^ The direction of an 'INCAFluxVar' is empty or is not equal to \"f\" or
  -- \"b\".
  | INCAMetaboliteVarNameIsInvalid (INCAMetaboliteVar a)
  -- ^ The name of an 'INCAMetaboliteVar' is empty.
  | INCAMetaboliteVarHasInvalidMixingProbability Rational (INCAMetaboliteVar a)
  -- ^ In the MFA context, the 'MixingProbability' for an 'INCAMetaboliteVar'
  -- must be @'>' 0@ and @'<=' 1@.
  | INCAMetaboliteVarHasInvalidStoichiometricCoefficient Rational (INCAMetaboliteVar a)
  -- ^ In the MFA context, the 'StoichiometricCoefficient' for an
  -- 'INCAMetaboliteVar' must be @'==' 1@.
  | INCAMetaboliteVarHasAtomTransitions (INCAMetaboliteVar a)
  -- ^ In the FBA context, an 'INCAMetaboliteVar' has atom transitions.
  | INCAIsotopomerFractionIndexIsInvalid {-# UNPACK #-} !Int (INCAIsotopomerFraction (INCAEMU (INCAMetaboliteVar a)))
  -- ^ In the MFA context, the index of an 'INCAIsotopomerFraction' is invalid
  -- with respect to the specified base.
  | INCAIsotopomerFractionEMUIsInvalid {-# UNPACK #-} !Int (INCAIsotopomerFraction (INCAEMU (INCAMetaboliteVar a)))
  -- ^ In the MFA context, the components of EMU of an 'INCAIsotopomerFraction'
  -- must be in order.
  | INCAMassFractionIndexIsInvalid {-# UNPACK #-} !Int (INCAMassFraction (INCAEMU (INCAMetaboliteVar a)))
  -- ^ In the MFA context, an 'INCAMassFraction' is invalid with respect to the
  -- specified base.
  | INCAMassFractionEMUIsInvalid {-# UNPACK #-} !Int (INCAMassFraction (INCAEMU (INCAMetaboliteVar a)))
  -- ^ In the MFA context, the components of EMU of an 'INCAMassFraction'
  -- must be in order.
  deriving (Eq, Ord, Read, Show)

-- | Convert an chemical reaction in INCA syntax to a 'ReactionNetwork' in MFA context.
--
-- Note: If the 'INCAReaction' is reversible, then the 'ReactionNetwork' has two 'Reaction's (forwards and backwards).
fromINCAMFAReaction
  :: (MonadError (INCAError Text) m)
  => Text
  -- ^ name for 'INCAFluxVar's
  -> INCAReaction (INCAMetaboliteVar Text) Text
  -- ^ chemical reaction in INCA syntax
  -> m (ReactionNetwork (FluxVar Text Text) (MetaboliteVar Text) Text)
  -- ^ result
fromINCAMFAReaction name mfaReaction@(INCAReaction arr ls rs) = do
  let
    -- | @catchINCAError m@ evaluates the specified computation and wraps any thrown errors with the 'INCAMFAReactionIsInvalid' constructor.
    catchINCAError :: (MonadError (INCAError Text) m) => m a -> m a
    catchINCAError m = m `Control.Monad.Error.Class.catchError` (Control.Monad.Error.Class.throwError . INCAMFAReactionIsInvalid name mfaReaction)
    -- | @mkMFAReactionPart x@ converts an 'INCAReactionPart' to a 'MFAReactionPart'.
    mkMFAReactionPart :: (MonadError (INCAError Text) m) => INCAReactionPart (INCAMetaboliteVar Text) Text -> m (MFAReactionPart (MetaboliteVar Text) Text)
    mkMFAReactionPart mfaReactionPart@(INCAReactionPart c metaboliteVar xs) = do
      -- In the MFA context, the 'StoichiometricCoefficient' must be @'==' 1@.
      unless (c == 1) $ do
        Control.Monad.Error.Class.throwError (INCAMetaboliteVarHasInvalidStoichiometricCoefficient c metaboliteVar)
      -- Convert the 'INCAMetaboliteVar'.
      k <- fromINCAMetaboliteVar metaboliteVar
      -- Convert the list of pairs.
      new_xs <- flip mapM xs $ \(p, atoms) -> do
        -- In the MFA context, every mixing probability must be in the interval @(0,1]@.
        when ((p <= 0) || (p > 1)) $ do
          Control.Monad.Error.Class.throwError (INCAMetaboliteVarHasInvalidMixingProbability p metaboliteVar)
        -- Return the result.
        return (MixingProbability (Product p), AtomKeyList atoms)
      let
        -- @sum_p@ is the sum of the mixing probabilities.
        sum_p :: MixingProbability
        sum_p = sum (map fst new_xs)
      -- In the MFA context, the sum of the mixing probabilities must be @'==' 1@.
      unless (null new_xs || (sum_p == mempty)) $ do
        Control.Monad.Error.Class.throwError (INCAMFAReactionPartHasInvalidMixingProbability (getProduct (getMixingProbability sum_p)) mfaReactionPart)
      -- Return the 'MetaboliteVar' and result.
      return (k, new_xs)
  -- Convert the left-hand side.
  new_ls <- catchINCAError (mapM mkMFAReactionPart (getINCAReactionSide ls))
  -- Convert the right-hand side.
  new_rs <- catchINCAError (mapM mkMFAReactionPart (getINCAReactionSide rs))
  -- Construct and convert the 'INCAFluxVar' for the forwards direction.
  ixForwards <- catchINCAError (fromINCAFluxVar (INCAFluxVar name (Just (Data.Text.singleton cINCAFluxVarDirectionForwards))))
  -- Construct and convert the 'INCAFluxVar' for the bacwkwards direction.
  ixBackwards <- catchINCAError (fromINCAFluxVar (INCAFluxVar name (Just (Data.Text.singleton cINCAFluxVarDirectionBackwards))))
  let
    ascList :: [(FluxVar Text Text, Reaction (MetaboliteVar Text) Text)]
    ascList = case arr of
      INCAArrowL ->
        -- If the arrow is right-to-left, then swap the left- and right-hand sides.
        [ (ixBackwards, MFAReaction new_rs new_ls)
        ]
      INCAArrowR ->
        -- If the arrow is left-to-right, then retain the left- and right-hand sides.
        [ (ixForwards, MFAReaction new_ls new_rs)
        ]
      INCAArrowLR ->
        -- If the arrow is reversible, then give the retained and swapped left- and right-hand sides.
        [ (ixForwards, MFAReaction new_ls new_rs)
        , (ixBackwards, MFAReaction new_rs new_ls)
        ]
  -- Return the 'ReactionNetwork'.
  return (ReactionNetwork (Data.Map.Strict.fromList ascList))

-- | Convert an chemical reaction in INCA syntax to a 'ReactionNetwork' in FBA context.
--
-- Note: If the 'INCAReaction' is reversible, then the 'ReactionNetwork' has two 'Reaction's (forwards and backwards).
fromINCAFBAReaction
  :: (MonadError (INCAError Text) m)
  => Text
  -- ^ name for 'INCAFluxVar's
  -> INCAReaction (INCAMetaboliteVar Text) Text
  -- ^ chemical reaction in INCA syntax
  -> m (ReactionNetwork (FluxVar Text Text) (MetaboliteVar Text) Text)
  -- ^ result
fromINCAFBAReaction name fbaReaction@(INCAReaction arr ls rs) = do
  let
    -- | @catchINCAError m@ evaluates the specified computation and wraps any thrown errors with the 'INCAFBAReactionIsInvalid' constructor.
    catchINCAError :: (MonadError (INCAError Text) m) => m a -> m a
    catchINCAError m = m `Control.Monad.Error.Class.catchError` (Control.Monad.Error.Class.throwError . INCAFBAReactionIsInvalid name fbaReaction)
    -- | @mkFBAReactionPart x@ converts an 'INCAReactionPart' to a 'FBAReactionPart'.
    mkFBAReactionPart :: (MonadError (INCAError Text) m) => INCAReactionPart (INCAMetaboliteVar Text) Text -> m (FBAReactionPart (MetaboliteVar Text) Text)
    mkFBAReactionPart (INCAReactionPart c metaboliteVar xs) = do
      unless (null xs) $ do
        -- In the FBA context, atom transitions are not allowed.
        Control.Monad.Error.Class.throwError (INCAMetaboliteVarHasAtomTransitions metaboliteVar)
      -- Convert the 'INCAMetaboliteVar'
      k <- fromINCAMetaboliteVar metaboliteVar
      -- Return the 'MetaboliteVar' and result.
      return (k, StoichiometricCoefficient (Sum c))
  -- Convert the left-hand side.
  new_ls <- catchINCAError (mapM mkFBAReactionPart (getINCAReactionSide ls))
  -- Convert the right-hand side.
  new_rs <- catchINCAError (mapM mkFBAReactionPart (getINCAReactionSide rs))
  -- Construct and convert the 'INCAFluxVar' for the forwards direction.
  ixForwards <- catchINCAError (fromINCAFluxVar (INCAFluxVar name (Just (Data.Text.singleton cINCAFluxVarDirectionForwards))))
  -- Construct and convert the 'INCAFluxVar' for the backwards direction.
  ixBackwards <- catchINCAError (fromINCAFluxVar (INCAFluxVar name (Just (Data.Text.singleton cINCAFluxVarDirectionBackwards))))
  let
    ascList :: [(FluxVar Text Text, Reaction (MetaboliteVar Text) Text)]
    ascList = case arr of
      INCAArrowL ->
        -- If the arrow is right-to-left, then swap the left- and right-hand sides.
        [ (ixBackwards, FBAReaction new_rs new_ls)
        ]
      INCAArrowR ->
        -- If the arrow is left-to-right, then retain the left- and right-hand sides.
        [ (ixForwards, FBAReaction new_ls new_rs)
        ]
      INCAArrowLR ->
        -- If the arrow is reversible, then give the retained and swaped left- and right-hand sides.
        [ (ixForwards, FBAReaction new_ls new_rs)
        , (ixBackwards, FBAReaction new_rs new_ls)
        ]
  -- Return the 'ReactionNetwork'.
  return (ReactionNetwork (Data.Map.Strict.fromList ascList))

-- | Convert an metabolic flux variable in INCA syntax to a 'FluxVar'.
fromINCAFluxVar
  :: (MonadError (INCAError Text) m)
  => INCAFluxVar Text
  -- ^ metabolic flux variable in INCA syntax
  -> m (FluxVar Text Text)
  -- ^ result
fromINCAFluxVar fluxVar@(INCAFluxVar name directionMaybe)
  | Data.Text.null name = do
      -- The name for a metabolic flux variable must be non-empty.
      Control.Monad.Error.Class.throwError (INCAFluxVarNameIsInvalid fluxVar)
  | Data.Text.head name == cINCAFluxVarExchange = case directionMaybe of
      Nothing -> do
        -- If the name starts with \"v\" and no direction is given, then the metabolic flux variable denotes an 'Irreversible', 'Exchange' chemical reaction.
        return (Exchange name Irreversible)
      Just direction
        | Data.Text.null direction -> do
            -- If the name starts with \"v\" and the direction is given, but is empty, then the metabolic flux variable denotes an 'Irreversible', 'Exchange' chemical reaction.
            return (Exchange name Irreversible)
        | direction == Data.Text.singleton cINCAFluxVarDirectionForwards -> do
            -- If the name starts with \"v\" and the direction is \"f\", then the metabolic flux variable denotes a 'Reversible', 'Exchange' chemical reaction in the forwards direction.
            return (Exchange name_f (Reversible (ForwardsOf name_b)))
        | direction == Data.Text.singleton cINCAFluxVarDirectionBackwards -> do
            -- If the name starts with \"v\" and the direction is \"b\", then the metabolic flux variable denotes a 'Reversible', 'Exchange' chemical reaction in the backwards direction.
            return (Exchange name_b (Reversible (BackwardsOf name_f)))
        | otherwise -> do
            -- Otherwise, the direction is invalid.
            Control.Monad.Error.Class.throwError (INCAFluxVarDirectionIsInvalid fluxVar)
  | Data.Text.head name == cINCAFluxVarTransport = case directionMaybe of
      Nothing -> do
        -- If the name starts with \"b\" and no direction is given, then the metabolic flux variable denotes an 'Irreversible', 'Transport' chemical reaction.
        return (Transport name Irreversible)
      Just direction
        | Data.Text.null direction -> do
            -- If the name starts with \"b\" and the direction is given, but is empty, then the metabolic flux variable denotes an 'Irreversible', 'Transport' chemical reaction.
            return (Transport name Irreversible)
        | direction == Data.Text.singleton cINCAFluxVarDirectionForwards -> do
            -- If the name starts with \"v\" and the direction is \"f\", then the metabolic flux variable denotes a 'Reversible', 'Transport' chemical reaction in the forwards direction.
            return (Transport name_f (Reversible (ForwardsOf name_b)))
        | direction == Data.Text.singleton cINCAFluxVarDirectionBackwards -> do
            -- If the name starts with \"v\" and the direction is \"b\", then the metabolic flux variable denotes a 'Reversible', 'Transport' chemical reaction in the backwards direction.
            return (Transport name_b (Reversible (BackwardsOf name_f)))
        | otherwise -> do
            -- Otherwise, the direction is invalid.
            Control.Monad.Error.Class.throwError (INCAFluxVarDirectionIsInvalid fluxVar)
  | otherwise = do
      -- Otherwise, the name of the metabolic flux variable is invalid.
      Control.Monad.Error.Class.throwError (INCAFluxVarNameIsInvalid fluxVar)
  where
    -- | @name_f@ and @name_b@ give the names of the metabolic flux variables for the forwards and backwards directions.
    name_f, name_b :: Text
    name_f = name `Data.Text.append` Data.Text.singleton cINCAFluxVarSeparator `Data.Text.append` Data.Text.singleton cINCAFluxVarDirectionForwards
    name_b = name `Data.Text.append` Data.Text.singleton cINCAFluxVarSeparator `Data.Text.append` Data.Text.singleton cINCAFluxVarDirectionBackwards

-- | Convert a 'Map' of metabolic flux variables in INCA syntax to a 'Map' of 'FluxVar's.
fromINCALinearConstraint
  :: (MonadError (INCAError Text) m)
  => Map (INCAFluxVar Text) a
  -- ^ map of metabolic flux variables in INCA syntax
  -> m (Map (FluxVar Text Text) a)
  -- ^ result
fromINCALinearConstraint = fmap Data.Map.Strict.fromList . mapM (\ ~(fluxVar, x) -> fmap (flip (,) x) (fromINCAFluxVar fluxVar)) . Data.Map.Strict.toList

-- | Convert an metabolite variable in INCA syntax to a 'MetaboliteVar'.
fromINCAMetaboliteVar
  :: (MonadError (INCAError Text) m)
  => INCAMetaboliteVar Text
  -- ^ metabolite variable in INCA syntax
  -> m (MetaboliteVar Text)
  -- ^ result
fromINCAMetaboliteVar metaboliteVar@(INCAMetaboliteVar name compartmentNameMaybe)
  | Data.Text.null name = do
      -- The name for a metabolite variable must be non-empty.
      Control.Monad.Error.Class.throwError (INCAMetaboliteVarNameIsInvalid metaboliteVar)
  | otherwise = case compartmentNameMaybe of
      Nothing -> do
        -- If the compartment name is not given, then the metabolite variable is 'Intracellular'.
        return (Intracellular name)
      Just compartmentName
        | Data.Text.null compartmentName -> do
            -- If the compartment name is given, but is empty, then the metabolite variable is 'Intracellular'.
            return (Intracellular name)
        | compartmentName == Data.Text.pack cINCAMetaboliteVarCompartmentNameExtracellular -> do
            -- If the compartment name is given and is @'==' \"ext\"@, then the metabolite variable is the concatenation of the given name and the compartment name and 'Extracellular'.
            return (Extracellular new_name)
        | otherwise -> do
            -- If the compartment name is given and is @'/=' \"ext\"@, then the metabolite variable is the concatenation of the given name and the compartment name and 'Intracellular'.
            return (Intracellular new_name)
        where
          -- @new_name@ is the concatenation of the given name and the compartment name.
          new_name :: Text
          new_name = name `Data.Text.append` Data.Text.singleton cINCAMetaboliteVarSeparator `Data.Text.append` compartmentName

-- | Convert an EMU in INCA syntax to an 'EMU' with respect to a base.
fromINCAEMU
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> INCAEMU (INCAMetaboliteVar Text)
  -- ^ EMU in INCA syntax
  -> m (EMU (MetaboliteVar Text))
  -- ^ result
fromINCAEMU base (INCAEMU metaboliteVar atomIxList) = do
  -- Convert the 'INCAMetaboliteVar'.
  k <- fromINCAMetaboliteVar metaboliteVar
  -- Return the 'EMU'.
  return (EMU base [(k, AtomIxSet (Data.IntSet.fromList atomIxList))])

-- | Convert a isotopomer fraction in INCA syntax to an 'EMUExpr' with respect to a base.
fromINCAIsotopomerFraction
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> INCAIsotopomerFraction (INCAEMU (INCAMetaboliteVar Text))
  -- ^ isotopomer fraction in INCA syntax
  -> m (EMUExpr (MetaboliteVar Text))
  -- ^ result
fromINCAIsotopomerFraction base isotopomerFraction@(INCAIsotopomerFraction emuList ixs)
  | Data.List.null ixs || any (>= base) ixs = do
      -- The components of the isotopomer fraction must not exceed the base.
      Control.Monad.Error.Class.throwError (INCAIsotopomerFractionIndexIsInvalid base isotopomerFraction)
  | emuList /= Data.List.sort emuList = do
      -- The components of the EMU of the mass function must be in order.
      Control.Monad.Error.Class.throwError (INCAIsotopomerFractionEMUIsInvalid base isotopomerFraction)
  | otherwise = do
    -- Convert the list of 'INCAEMU's.
    k <- fmap mconcat (mapM (fromINCAEMU base) emuList)
    let
      -- @new_ix@ is the index of the isotopomer fraction vector.
      new_ix :: Int
      new_ix = sum (zipWith (\ix n -> ix * (base ^ n)) ixs (enumFromThen 0 1 :: [Int]))
    -- Return the result.
    return (lookupIsotopomerFractionVector k new_ix)

-- | Convert a mass fraction in INCA syntax to an 'EMUExpr' with respect to a base.
fromINCAMassFraction
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> INCAMassFraction (INCAEMU (INCAMetaboliteVar Text))
  -- ^ mass fraction in INCA syntax
  -> m (EMUExpr (MetaboliteVar Text))
  -- ^ result
fromINCAMassFraction base massFraction@(INCAMassFraction emuList ix)
  | (ix < 0) || (ix > n * (base - 1)) = do
      -- The index of the mass fraction must be non-negative and must not exceed the @n * (base - 1)@, viz., the maximum number of additional neutrons (or mass shifts).
      Control.Monad.Error.Class.throwError (INCAMassFractionIndexIsInvalid base massFraction)
  | emuList /= Data.List.sort emuList = do
      -- The components of the EMU of the mass function must be in order.
      Control.Monad.Error.Class.throwError (INCAMassFractionEMUIsInvalid base massFraction)
  | otherwise = do
      -- Convert the list of 'INCAEMU's.
      k <- fmap mconcat (mapM (fromINCAEMU base) emuList)
      -- Return the result.
      return (lookupMassFractionVector k ix)
  where
    -- @n@ is the total number of atoms.
    n :: Int
    n = sum (map (\(INCAEMU _ xs) -> length xs) emuList)
