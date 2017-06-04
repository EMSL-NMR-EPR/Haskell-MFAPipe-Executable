{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.FBA
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types and functions for flux balance analysis (FBA) in
-- the \"mfaPipe\" executable.
-----------------------------------------------------------------------------

module MFAPipe.FBA
( FBA(..)
, FBAResult(..)
, toFBA
, toArchive
, toArchiveWith
) where

import           Codec.Archive.Zip (Archive(..), Entry)
import qualified Codec.Archive.Zip
import           Control.Applicative (liftA3)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except
import           Data.Bifunctor (bimap, second)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Csv (ToField(), EncodeOptions)
import qualified Data.Csv
import qualified Data.HashMap.Strict
import           Data.LinearProgram.Common (Bounds(..), Constraint(..), Direction(..), LP(..), VarKind(..))
import           Data.LinearProgram.GLPK.Solver (GLPOpts, ReturnCode)
import qualified Data.LinearProgram.GLPK.Solver
import qualified Data.Map.Lazy
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Set
import           Data.Text (Text)
import qualified Data.Text.Lazy
import           Language.FluxJS.Conversion.Text
import qualified Language.FluxJS.Types as FluxJS
import           Language.INCA.Conversion.Text
import           MFAPipe.Csv.Types.Flux (FluxRecords(..))
import qualified MFAPipe.Csv.Types.Flux
import           MFAPipe.Csv.Types.NullspaceMatrix (NullspaceMatrixRecords(..))
import qualified MFAPipe.Csv.Types.NullspaceMatrix
import qualified MFAPipe.Csv.Types.ObjectiveFunction
import qualified MFAPipe.Csv.Types.ReactionNetwork
import           MFAPipe.Csv.Types.Stoichiometry (DenseEncoding(DenseEncodeAll))
import qualified MFAPipe.Csv.Types.Stoichiometry
import           MFAPipe.Utils
import           Numeric.LinearAlgebra.HMatrix (Element())
import qualified Numeric.LinearAlgebra.HMatrix
import           Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan (Nullspace(..))
import qualified Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan
import           Science.Chemistry.FluxVar
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.ReactionNetwork
import           Science.Chemistry.Stoichiometry
import           System.Log.Data (MonadRecord(), Data, Lvl, Msg)
import qualified System.Log.Simple
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text
import qualified Text.Printf

-- | A FBA computation, where:
--
-- * @i@ is the type of chemical reaction indices;
--
-- * @k@ is the type of chemical species;
--
-- * @a@ is the type of atoms; and,
--
-- * @e@ is the type of numeric elements.
--
data FBA i k a e = FBA
  { _fbaReactionNetwork :: ReactionNetwork i k a
  , _fbaReactionNetworkDense :: Dense i k e
  , _fbaReactionNetworkNullspace :: Nullspace i e
  , _fbaLP :: LP i e
  , runFBA :: GLPOpts -> IO (ReturnCode, Maybe (FBAResult i e))
  }

-- | The result of a FBA computation, where:
--
-- * @i@ is the type of chemical reaction indices;
--
-- * @e@ is the type of numeric elements.
--
data FBAResult i e = FBAResult
  { _fbaResultFlux :: Map i e
  , _fbaResultObjectiveFunction :: e
  } deriving (Eq, Ord, Read, Show)

-- | Convert a FluxJS model to a FBA computation.
toFBA
  :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m)
  => FluxJS.Model Double
  -- ^ model
  -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (FBA (FluxVar Text Text) (MetaboliteVar Text) Text Double)
  -- ^ result
toFBA (FluxJS.Model base0 reactionNetwork0 (FluxJS.Experiments experiments0) (FluxJS.Constraints objectiveFunctionMaybe0 (FluxJS.Bounds bounds0) _ (FluxJS.LinearConstraints linearConstraints0) _ lowerBound0 _ upperBound0 _ _)) = do
  when (Data.Maybe.isJust base0) $ do
    System.Log.Simple.warning (Text.Printf.printf "Ignoring \"isotopic_labeling_states_per_atom\" attribute of FluxJS document: %d" (Data.Maybe.fromJust base0))

  unless (Data.HashMap.Strict.null experiments0) $ do
    System.Log.Simple.warning (Text.Printf.printf "Ignoring %s of \"experiments\" attribute of FluxJS document" (pluralizeWith (const "children") (Data.HashMap.Strict.size experiments0) "child"))

  -- Construct the chemical reaction network.
  System.Log.Simple.info "Constructing chemical reaction network"
  reactionNetwork <- fromFluxJSReactionNetwork reactionNetwork0
  System.Log.Simple.info "Chemical reaction network was constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size (getReactionNetwork reactionNetwork)) "chemical reaction"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList (getReactionNetwork reactionNetwork))) $ \(n, (ix, reaction)) -> do
    let
      ix' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty ix)))
      reaction' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty reaction)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %s" n ix' reaction')

  -- Construct the stoichiometric model for the chemical reaction network.
  System.Log.Simple.info "Constructing stoichiometric model"
  let
    reactionNetworkDense :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double
    reactionNetworkDense = toStoichiometricModel reactionNetwork
  System.Log.Simple.info "Stoichiometric model was constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Set.size (fst (_denseIntermediate reactionNetworkDense))) "intracellular metabolite"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Set.toAscList (fst (_denseIntermediate reactionNetworkDense)))) $ \(n, k) -> do
    let
      k' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty k)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s" n k')
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Set.size (fst (_denseReagentProduct reactionNetworkDense))) "extracellular metabolite"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Set.toAscList (fst (_denseReagentProduct reactionNetworkDense)))) $ \(n, k) -> do
    let
      k' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty k)))
    System.Log.Simple.debug (Text.Printf.printf "[%d] %s" n k')

  -- Construct the objective function.
  System.Log.Simple.info "Constructing objective function"
  -- Ensure that an objective function is specified.
  when (Data.Maybe.isNothing objectiveFunctionMaybe0) $ do
    Control.Monad.Trans.Except.throwE (Right FluxJSObjectiveFunctionIsBlank)
  -- Determine the \"direction\" of the LP computation.
  (direction, objective) <- case Data.Maybe.fromJust objectiveFunctionMaybe0 of
    FluxJS.Maximize text -> fmap ((,) Max . Data.Map.Lazy.fromAscList . Data.Map.Strict.toAscList) (fromFluxJSLinearConstraint text)
    FluxJS.Minimize text -> fmap ((,) Min . Data.Map.Lazy.fromAscList . Data.Map.Strict.toAscList) (fromFluxJSLinearConstraint text)
  -- Ensure that objective function has at least one key/value pair.
  when (Data.Map.Strict.null objective) $ do
    Control.Monad.Trans.Except.throwE (Right FluxJSObjectiveFunctionIsBlank)
  System.Log.Simple.info "Objective function was constructed successfully!"
  unless (Data.Map.Strict.null objective) $ do
    let
      objective' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (LinearFunction (Data.Map.Lazy.toAscList objective)))))
    System.Log.Simple.debug (Text.Printf.printf "Objective function: %s" objective')
    System.Log.Simple.debug (Text.Printf.printf "Objective function direction: %s" (case direction of Max -> "Maximize" ; Min -> "Minimize"))

  -- Construct the bounds.
  System.Log.Simple.info "Constructing bounds"
  System.Log.Simple.debug (Text.Printf.printf "Default lower bound: %f" lowerBound0)
  System.Log.Simple.debug (Text.Printf.printf "Default upper bound: %f" upperBound0)
  bounds <-
    let
      cataM
        :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m)
        => ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map (FluxVar Text Text) (Bounds Double))
        -> Text
        -> FluxJS.Bound Double
        -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m (Map (FluxVar Text Text) (Bounds Double))
      cataM macc fluxVarName x = do
        acc <- macc
        fluxVar <- fromFluxJSFluxVar fluxVarName
        let
          fluxVar' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (bimap Data.Text.Lazy.fromStrict Data.Text.Lazy.fromStrict fluxVar))))
        System.Log.Simple.debug (Text.Printf.printf "Processing metabolic flux variable: %s" fluxVar')
        new_bounds <- case x of
          FluxJS.Free -> do
            System.Log.Simple.warning "Free variable (using default lower and upper bounds)"
            return (Bound lowerBound0 upperBound0)
          FluxJS.EqualTo equ -> do
            System.Log.Simple.warning "Equality"
            let
              new_equ :: Double
              new_equ = (equ `max` lowerBound0) `min` upperBound0
            unless (equ == new_equ) $ do
              System.Log.Simple.warning (Text.Printf.printf "Old equality: %f; New equality: %f" equ new_equ)
            return (Equ new_equ)
          FluxJS.GreaterThan lB -> do
            System.Log.Simple.warning "Greater than (using default upper bound)"
            let
              new_lB :: Double
              new_lB = lB `max` lowerBound0
            unless (lB == new_lB) $ do
              System.Log.Simple.warning (Text.Printf.printf "Old lower bound: %f; New lower bound: %f" lB new_lB)
            return (Bound new_lB upperBound0)
          FluxJS.LessThan uB -> do
            System.Log.Simple.warning "Less than (using default lower bound)"
            let
              new_uB :: Double
              new_uB = uB `min` upperBound0
            unless (uB == new_uB) $ do
              System.Log.Simple.warning (Text.Printf.printf "Old upper bound: %f; New upper bound: %f" uB new_uB)
            return (Bound lowerBound0 new_uB)
          FluxJS.Between lB uB -> do
            System.Log.Simple.warning "Bound (using default lower and upper bounds as, respectively, minimum and maximum)"
            let
              new_lB, new_uB :: Double
              new_lB = lB `max` lowerBound0
              new_uB = uB `min` upperBound0
            unless (lB == new_lB) $ do
              System.Log.Simple.warning (Text.Printf.printf "Old lower bound: %f; New lower bound: %f" lB new_lB)
            unless (uB == new_uB) $ do
              System.Log.Simple.warning (Text.Printf.printf "Old upper bound: %f; New upper bound: %f" uB new_uB)
            return (Bound new_lB new_uB)
        return (Data.Map.Strict.insert fluxVar new_bounds acc)
    in
      Data.HashMap.Strict.foldlWithKey' cataM (return Data.Map.Strict.empty) bounds0
  System.Log.Simple.info "Bounds were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (Data.Map.Strict.size bounds) "bound"))
  if Data.Map.Strict.null bounds
    then do
      System.Log.Simple.warning (Text.Printf.printf "Using default lower and upper bounds for all metabolic flux variables (denoted \"v\"): %f <= v <= %f" lowerBound0 upperBound0)
    else do
      forM_ (zip (enumFromThen 1 2 :: [Integer]) (Data.Map.Strict.toAscList bounds)) $ \(n, (ix, x)) -> do
        let
          ix' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (bimap Data.Text.Lazy.fromStrict Data.Text.Lazy.fromStrict ix))))
        case x of
          Free -> do
            System.Log.Simple.warning (Text.Printf.printf "[%d] %s is a free variable" n ix')
          LBound lB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s >= %f" n ix' lB)
          UBound uB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s <= %f" n ix' uB)
          Equ equ -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f" n ix' equ)
          Bound lB uB -> do
            System.Log.Simple.debug (Text.Printf.printf "[%d] %f <= %s <= %f" n lB ix' uB)

  -- Construct the linear constraints.
  System.Log.Simple.info "Constructing linear constraints"
  linearConstraints <-
    let
      cataM
        :: (Monad m)
        => ExceptT (Either (INCAError Text) (FluxJSError Text)) m [Constraint (FluxVar Text Text) Double]
        -> Text
        -> FluxJS.Bound Double
        -> ExceptT (Either (INCAError Text) (FluxJSError Text)) m [Constraint (FluxVar Text Text) Double]
      cataM macc text x = do
        acc <- macc
        m <- fromFluxJSLinearConstraint text
        let
          new_bounds :: Bounds Double
          new_bounds = case x of
            FluxJS.Free -> Free
            FluxJS.EqualTo equ -> Equ equ
            FluxJS.GreaterThan lB -> LBound lB
            FluxJS.LessThan uB -> UBound uB
            FluxJS.Between lB uB -> Bound lB uB
          constraint :: Constraint (FluxVar Text Text) Double
          constraint = Constr Nothing (Data.Map.Lazy.fromAscList (Data.Map.Strict.toAscList m)) new_bounds
        return (constraint:acc)
    in
      Data.HashMap.Strict.foldlWithKey' cataM (return []) linearConstraints0
  System.Log.Simple.info "Linear constraints were constructed successfully!"
  System.Log.Simple.debug (Text.Printf.printf "Found %s" (pluralizeWith (++ "s") (length linearConstraints) "linear constraint"))
  forM_ (zip (enumFromThen 1 2 :: [Integer]) linearConstraints) $ \(n, Constr _ m x) -> do
    let
      m' = Data.Text.Lazy.unpack (Text.PrettyPrint.Leijen.Text.displayT (Text.PrettyPrint.Leijen.Text.renderCompact (pretty (LinearFunction (Data.Map.Strict.toAscList m)))))
    case x of
      Free -> do
        System.Log.Simple.warning (Text.Printf.printf "[%d] %s is a free constraint" n m')
      LBound lB -> do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s >= %f" n m' lB)
      UBound uB -> do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s <= %f" n m' uB)
      Equ equ -> do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %s = %f" n m' equ)
      Bound lB uB -> do
        System.Log.Simple.debug (Text.Printf.printf "[%d] %f <= %s <= %f" n lB m' uB)

  let
    -- | @mkLP dense@ is the LP computation for @dense@.
    mkLP :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double -> LP (FluxVar Text Text) Double
    mkLP = liftA3 (LP direction objective) mkConstraints mkVarBounds mkVarTypes
      where
        -- | @mkConstraints dense@ is the set of linear constraints for @dense@.
        mkConstraints :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double -> [Constraint (FluxVar Text Text) Double]
        mkConstraints Dense{..} = linearConstraints ++ map (flip (Constr Nothing) (Equ 0) . Data.Map.Lazy.fromAscList . filter ((/= 0) . snd) . zip (Data.Set.toAscList _denseReactionIndices) . Numeric.LinearAlgebra.HMatrix.toList) (Numeric.LinearAlgebra.HMatrix.toRows (snd _denseIntermediate))
        -- | @mkVarBounds dense@ is the set of variable bounds for @dense@.
        mkVarBounds :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double -> Data.Map.Lazy.Map (FluxVar Text Text) (Bounds Double)
        mkVarBounds Dense{..} =
          let
            cata :: Data.Map.Lazy.Map (FluxVar Text Text) (Bounds Double) -> FluxVar Text Text -> Data.Map.Lazy.Map (FluxVar Text Text) (Bounds Double)
            cata acc fluxVar = Data.Map.Lazy.insert fluxVar (Data.Map.Strict.findWithDefault (Bound lowerBound0 upperBound0) fluxVar bounds) acc
          in
            Data.Set.foldl' cata Data.Map.Lazy.empty _denseReactionIndices
        -- | @mkVarTypes dense@ is the set of variable types for @dense@.
        mkVarTypes :: Dense (FluxVar Text Text) (MetaboliteVar Text) Double -> Data.Map.Lazy.Map (FluxVar Text Text) VarKind
        mkVarTypes Dense{..} = Data.Map.Lazy.fromAscList (map (\fluxVar -> (fluxVar, ContVar)) (Data.Set.toAscList _denseReactionIndices))
    -- | @lp@ is the LP computation.
    lp :: LP (FluxVar Text Text) Double
    lp@(LP _dir _objectiveFunc _constraints _varBounds _varTypes) = mkLP reactionNetworkDense

  -- Construct the FBA computation.
  let
    fba :: FBA (FluxVar Text Text) (MetaboliteVar Text) Text Double
    fba = FBA
      { _fbaReactionNetwork = reactionNetwork
      , _fbaReactionNetworkDense = reactionNetworkDense
      , _fbaReactionNetworkNullspace =
          let
            ixs = Data.Map.Strict.keysSet (getReactionNetwork reactionNetwork)
            s = snd (_denseIntermediate reactionNetworkDense)
          in
            Numeric.LinearAlgebra.HMatrix.Algorithms.GaussJordan.nullspace ixs s
      , _fbaLP = lp
      , runFBA = \opts -> fmap (second (fmap toFBAResult)) (Data.LinearProgram.GLPK.Solver.glpSolveVars opts lp)
      }

  -- Fin!
  return fba

-- | Convert the result of a LP computation to the result of a FBA computation.
toFBAResult :: (Eq i) => (e, Data.Map.Lazy.Map i e) -> FBAResult i e
toFBAResult (x, m) = FBAResult
  { _fbaResultFlux = Data.Map.Strict.fromAscList (Data.Map.Lazy.toAscList m)
  , _fbaResultObjectiveFunction = x
  }

-- | Convert the result of a FBA computation to a zip archive.
--
-- Efficiently serialize CSV records as lazy 'Data.ByteString.Lazy.ByteString's.
--
-- Directory tree:
--
-- > .
-- > +-- reaction_network
-- > |   +-- nullspace.csv
-- > |   +-- reaction.csv
-- > |   +-- stoichiometry.csv
-- > +-- result
-- >     +-- flux.csv
-- >     +-- objective_function.csv
--
toArchive
  :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m, Ord i, Pretty i, Pretty k, Pretty e, Pretty a, Element e, ToField i, ToField k, ToField e)
  => Integer
  -- ^ modification time (seconds since unix epoch)
  -> FBA i k a e
  -- ^ FBA computation
  -> FBAResult i e
  -- ^ result of FBA computation
  -> m Archive
  -- ^ archive
toArchive = toArchiveWith Data.Csv.defaultEncodeOptions

-- | Like 'toArchive', but lets you customize how the CSV data is encoded.
toArchiveWith
  :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m, Ord i, Pretty i, Pretty k, Pretty e, Pretty a, Element e, ToField i, ToField k, ToField e)
  => EncodeOptions
  -- ^ encoding options for CSV files
  -> Integer
  -- ^ modification time (seconds since unix epoch)
  -> FBA i k a e
  -- ^ FBA computation
  -> FBAResult i e
  -- ^ result of FBA computation
  -> m Archive
  -- ^ archive
toArchiveWith opts modtime FBA{..} FBAResult{..} = do
  System.Log.Simple.info "Serializing CSV documents"

  System.Log.Simple.debug "[1] Chemical reaction network"
  let
    reactionNetworkEntry :: Entry
    reactionNetworkEntry = Codec.Archive.Zip.toEntry reactionNetworkFilePath modtime reactionNetworkCsv
      where
        reactionNetworkFilePath :: FilePath
        reactionNetworkFilePath = "reaction_network/reaction.csv"
        reactionNetworkCsv :: ByteString
        reactionNetworkCsv = MFAPipe.Csv.Types.ReactionNetwork.encodeWith opts _fbaReactionNetwork

  System.Log.Simple.debug "[2] Stoichiometric model"
  let
    reactionNetworkDenseEntry :: Entry
    reactionNetworkDenseEntry = Codec.Archive.Zip.toEntry reactionNetworkDenseFilePath modtime reactionNetworkDenseCsv
      where
        reactionNetworkDenseFilePath :: FilePath
        reactionNetworkDenseFilePath = "reaction_network/stoichiometry.csv"
        reactionNetworkDenseCsv :: ByteString
        reactionNetworkDenseCsv = MFAPipe.Csv.Types.Stoichiometry.encodeWith opts DenseEncodeAll _fbaReactionNetworkDense

  System.Log.Simple.debug "[3] Nullspace"
  let
    nullspaceMatrixEntry :: Entry
    nullspaceMatrixEntry = Codec.Archive.Zip.toEntry nullspaceMatrixFilePath modtime nullspaceMatrixCsv
      where
        nullspaceMatrixFilePath :: FilePath
        nullspaceMatrixFilePath = "reaction_network/nullspace.csv"
        nullspaceMatrixCsv :: ByteString
        nullspaceMatrixCsv = MFAPipe.Csv.Types.NullspaceMatrix.encodeWith opts (NullspaceMatrixRecords (Data.Map.Strict.keysSet (getReactionNetwork _fbaReactionNetwork)) _fbaReactionNetworkNullspace)

  System.Log.Simple.debug "[4] Metabolic fluxes"
  let
    fluxEntry :: Entry
    fluxEntry = Codec.Archive.Zip.toEntry fluxFilePath modtime fluxCsv
      where
        fluxFilePath :: FilePath
        fluxFilePath = "result/flux.csv"
        fluxCsv :: ByteString
        fluxCsv = MFAPipe.Csv.Types.Flux.encodeWith opts (FluxRecords _fbaResultFlux)

  System.Log.Simple.debug "[5] Objective function"
  let
    objectiveFunctionEntry :: Entry
    objectiveFunctionEntry = Codec.Archive.Zip.toEntry objectiveFunctionFilePath modtime objectiveFunctionCsv
      where
        objectiveFunctionFilePath :: FilePath
        objectiveFunctionFilePath = "result/objective_function.csv"
        objectiveFunctionCsv :: ByteString
        objectiveFunctionCsv = MFAPipe.Csv.Types.ObjectiveFunction.encodeWith opts (direction _fbaLP) (objective _fbaLP) _fbaResultObjectiveFunction

  System.Log.Simple.info "CSV documents were serialized successfully!"

  -- "."
  System.Log.Simple.info "Constructing zip archive"
  let
    archive :: Archive
    archive = Archive
      { zEntries =
          [ reactionNetworkEntry
          , reactionNetworkDenseEntry
          , nullspaceMatrixEntry
          , fluxEntry
          , objectiveFunctionEntry
          ]
      , zSignature = Nothing
      , zComment = Data.ByteString.Lazy.empty
      }
  System.Log.Simple.info "Zip archive was constructed successfully!"

  -- Fin!
  return archive
