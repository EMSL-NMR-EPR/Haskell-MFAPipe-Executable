{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.Command
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for modes of the \"mfaPipe\" command line
-- application.
-----------------------------------------------------------------------------

module MFAPipe.Command
( -- * Command type
  Command(..)
, runCommand
  -- * Actions
-- , doMFA
, doFBA
) where

import           Codec.Archive.Zip (Archive)
import qualified Codec.Archive.Zip
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO())
import qualified Control.Monad.IO.Class
import           Control.Monad.Error.Class (MonadError(..))
import qualified Control.Monad.Error.Class
import qualified Control.Monad.Random
import           Control.Monad.Random.Logger.Instances ()
import qualified Control.Monad.Trans.Except
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import           Data.Data (Data())
import           Data.LinearProgram.GLPK.Solver (GLPOpts(..), ReturnCode(..))
import qualified Data.LinearProgram.GLPK.Solver
import           Data.Typeable (Typeable())
import qualified Language.FluxJS.Types as FluxJS
import           MFAPipe.Banner
import qualified MFAPipe.FBA
import qualified MFAPipe.MFA
import           MFAPipe.Warranty
import           Numeric.LevMar (Options(..))
import           System.IO (IOMode(WriteMode))
import qualified System.IO
import qualified System.Exit
import           System.Log.Data (MonadRecord(), Lvl(Lvl), Msg(Msg))
import qualified System.Log.Data
import qualified System.Log.Format
import           System.Log.Level (Level(Debug))
import qualified System.Log.Logger.Base
import qualified System.Log.Logger.Handler
import           System.Log.Logger.Instances ()
import qualified System.Log.Logger.Priority
import           System.Log.Simple (BaseLoggerT, HandlerLoggerT, PriorityLoggerT)
import qualified System.Log.Simple
import           System.Random (RandomGen())
import qualified System.Random
import qualified Text.Printf

-- | A mode of the \"mfaPipe\" command line application.
data Command
  = DoMFAUsingLevMar
    { input :: FilePath
    , output :: FilePath
    , _seed :: Int
    -- ^ Seed for random number generator
    , _itMax :: Int
    -- ^ Maximum iterations.
    , _optScaleInitMu :: Double
    -- ^ Scale factor for initial @mu@.
    , _optStopNormInfJacTe :: Double
    -- ^ Stopping thresholds for @||J^T e||_inf@.
    , _optStopNorm2Dp :: Double
    -- ^ Stopping thresholds for @||Dp||_2@.
    , _optStopNorm2E :: Double
    -- ^ Stopping thresholds for @||e||_2@.
    , _optDelta :: Double
    -- ^ Step used in the difference approximation to the Jacobian. If @optDelta<0@, the Jacobian is approximated with central differences which are more accurate (but slower!) compared to the forward differences employed by default.
    }
  | DoFBAUsingSimplex
    { input :: FilePath
    , output :: FilePath
    , _tmLim :: Int
    , _presolve :: Bool
    }
  | DoFBAUsingMIP
    { input :: FilePath
    , output :: FilePath
    , _tmLim :: Int
    , _presolve :: Bool
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

runCommand :: Command -> IO ()
runCommand (DoMFAUsingLevMar input output seed0 itMax0 optScaleInitMu0 optStopNormInfJacTe0 optStopNorm2Dp0 optStopNorm2E0 optDelta0) = do
  hPrintBanner System.IO.stdout
  hPrintWarranty System.IO.stdout
  runLoggerT $ do
    System.Log.Simple.info (Text.Printf.printf "Reading FluxJS document from file \"%s\"" input)
    bs <- Control.Monad.IO.Class.liftIO (Data.ByteString.Lazy.readFile input)
    System.Log.Simple.info "FluxJS document was read successfully!"
    System.Log.Simple.info "Decoding FluxJS document"
    case Data.Aeson.eitherDecode bs of
      Left err -> do
        -- TODO pretty
        System.Log.Simple.error (show err)
        Control.Monad.IO.Class.liftIO System.Exit.exitFailure
      Right (FluxJS.Document _modelMetadata model) -> do
        System.Log.Simple.info "FluxJS document was decoded successfully!"
        void $ doMFA model (System.Random.mkStdGen seed0) itMax0 (Opts optScaleInitMu0 optStopNormInfJacTe0 optStopNorm2Dp0 optStopNorm2E0 optDelta0) $ \archive -> do
          System.Log.Simple.info (Text.Printf.printf "Writing zip archive to file \"%s\"" output)
          Control.Monad.IO.Class.liftIO (System.IO.withBinaryFile output WriteMode (\hdl -> Data.ByteString.Lazy.hPut hdl (Codec.Archive.Zip.fromArchive archive)))
          System.Log.Simple.info "Zip archive was written successfully!"
        Control.Monad.IO.Class.liftIO System.Exit.exitSuccess
runCommand (DoFBAUsingSimplex input output tmLim0 presolve0) = do
  hPrintBanner System.IO.stdout
  hPrintWarranty System.IO.stdout
  runLoggerT $ do
    System.Log.Simple.info (Text.Printf.printf "Reading FluxJS document from file \"%s\"" input)
    bs <- Control.Monad.IO.Class.liftIO (Data.ByteString.Lazy.readFile input)
    System.Log.Simple.info "FluxJS document was read successfully!"
    System.Log.Simple.info "Decoding FluxJS document"
    case Data.Aeson.eitherDecode bs of
      Left err -> do
        -- TODO pretty
        System.Log.Simple.error (show err)
        Control.Monad.IO.Class.liftIO System.Exit.exitFailure
      Right (FluxJS.Document _modelMetadata model) -> do
        System.Log.Simple.info "FluxJS document was decoded successfully!"
        doFBA model (Data.LinearProgram.GLPK.Solver.simplexDefaults { tmLim = tmLim0 , presolve = presolve0 }) $ \archive -> do
          System.Log.Simple.info (Text.Printf.printf "Writing zip archive to file \"%s\"" output)
          Control.Monad.IO.Class.liftIO (System.IO.withBinaryFile output WriteMode (\hdl -> Data.ByteString.Lazy.hPut hdl (Codec.Archive.Zip.fromArchive archive)))
          System.Log.Simple.info "Zip archive was written successfully!"
        Control.Monad.IO.Class.liftIO System.Exit.exitSuccess
runCommand (DoFBAUsingMIP input output tmLim0 presolve0) = do
  hPrintBanner System.IO.stdout
  hPrintWarranty System.IO.stdout
  runLoggerT $ do
    System.Log.Simple.info (Text.Printf.printf "Reading FluxJS document from file \"%s\"" input)
    bs <- Control.Monad.IO.Class.liftIO (Data.ByteString.Lazy.readFile input)
    System.Log.Simple.info "FluxJS document was read successfully!"
    System.Log.Simple.info "Decoding FluxJS document"
    case Data.Aeson.eitherDecode bs of
      Left err -> do
        -- TODO pretty
        System.Log.Simple.error (show err)
        Control.Monad.IO.Class.liftIO System.Exit.exitFailure
      Right (FluxJS.Document _modelMetadata model) -> do
        System.Log.Simple.info "FluxJS document was decoded successfully!"
        doFBA model (Data.LinearProgram.GLPK.Solver.mipDefaults { tmLim = tmLim0 , presolve = presolve0 }) $ \archive -> do
          System.Log.Simple.info (Text.Printf.printf "Writing zip archive to file \"%s\"" output)
          Control.Monad.IO.Class.liftIO (System.IO.withBinaryFile output WriteMode (\hdl -> Data.ByteString.Lazy.hPut hdl (Codec.Archive.Zip.fromArchive archive)))
          System.Log.Simple.info "Zip archive was written successfully!"
        Control.Monad.IO.Class.liftIO System.Exit.exitSuccess

doMFA :: (RandomGen g, MonadIO m, MonadError e m, MonadRecord (System.Log.Data.Data Lvl, (System.Log.Data.Data Msg, ())) m, Show e) => FluxJS.Model Double -> g -> Int -> Options Double -> (Archive -> m b) -> m (b, g)
doMFA model g0 itMax opts k = do
  System.Log.Simple.info "Constructing MFA simulation"
  (e, g1) <- flip Control.Monad.Random.runRandT g0 (Control.Monad.Trans.Except.runExceptT (MFAPipe.MFA.toMFASpec model) `Control.Monad.Error.Class.catchError` \err -> do
    -- TODO pretty
    System.Log.Simple.error (show err)
    Control.Monad.IO.Class.liftIO System.Exit.exitFailure)
  case e of
    Left err -> do
      -- TODO pretty
      System.Log.Simple.error (show err)
      Control.Monad.IO.Class.liftIO System.Exit.exitFailure
    Right mfaSpec -> do
      System.Log.Simple.info "MFA simulation was constructed successfully!"
      System.Log.Simple.info "Executing MFA simulation"
      e' <- return (MFAPipe.MFA.runMFA (MFAPipe.MFA.toMFA mfaSpec) itMax opts) `Control.Monad.Error.Class.catchError` \err -> do
        -- TODO pretty
        System.Log.Simple.error (show err)
        Control.Monad.IO.Class.liftIO System.Exit.exitFailure
      case e' of
        Left err -> do
          -- TODO pretty
          System.Log.Simple.error (show err)
          Control.Monad.IO.Class.liftIO System.Exit.exitFailure
        Right mfaResult -> do
          mfaResult `seq` System.Log.Simple.info "MFA simulation was executed successfully!"
          archive <- MFAPipe.MFA.toArchive 0 mfaSpec mfaResult
          fmap (\x -> (x, g1)) (k archive)

doFBA :: (MonadIO m, MonadError e m, MonadRecord (System.Log.Data.Data Lvl, (System.Log.Data.Data Msg, ())) m, Show e) => FluxJS.Model Double -> GLPOpts -> (Archive -> m b) -> m b
doFBA model opts k = do
  System.Log.Simple.info "Constructing FBA simulation"
  e <- Control.Monad.Trans.Except.runExceptT (MFAPipe.FBA.toFBA model) `Control.Monad.Error.Class.catchError` \err -> do
    -- TODO pretty
    System.Log.Simple.error (show err)
    Control.Monad.IO.Class.liftIO System.Exit.exitFailure
  System.Log.Simple.info "FBA simulation was constructed successfully!"
  case e of
    Left err -> do
      -- TODO pretty
      System.Log.Simple.error (show err)
      Control.Monad.IO.Class.liftIO System.Exit.exitFailure
    Right fba -> do
      System.Log.Simple.info "Executing FBA simulation"
      (returnCode, resultMaybe) <- Control.Monad.IO.Class.liftIO (MFAPipe.FBA.runFBA fba opts) `Control.Monad.Error.Class.catchError` \err -> do
        -- TODO pretty
        System.Log.Simple.error (show err)
        Control.Monad.IO.Class.liftIO System.Exit.exitFailure
      case returnCode of
        Success -> do
          case resultMaybe of
            Just fbaResult -> do
              System.Log.Simple.info "FBA simulation was executed successfully!"
              archive <- MFAPipe.FBA.toArchive 0 fba fbaResult
              k archive
            Nothing -> do
              -- TODO pretty
              System.Log.Simple.panic "No result"
              Control.Monad.IO.Class.liftIO System.Exit.exitFailure
        _ -> do
          -- TODO pretty
          System.Log.Simple.error (Text.Printf.printf "Invalid return code: \"%s\"" (show returnCode))
          Control.Monad.IO.Class.liftIO System.Exit.exitFailure

type LoggerT m = PriorityLoggerT (HandlerLoggerT (BaseLoggerT (System.Log.Data.Data Lvl, (System.Log.Data.Data Msg, ())) m))

runLoggerT :: (MonadIO m) => LoggerT m a -> m a
runLoggerT m = f (System.Log.Logger.Handler.addHandler (System.Log.Logger.Handler.printHandler Nothing) >> m)
  where
    f =
      System.Log.Logger.Base.runBaseLoggerT (Lvl, Msg)
        .
      System.Log.Logger.Handler.runHandlerLoggerT System.Log.Format.defaultFormatter
        .
      System.Log.Logger.Priority.runPriorityLoggerT Debug
