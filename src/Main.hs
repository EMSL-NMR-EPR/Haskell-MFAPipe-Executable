-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Entry point for the \"mfaPipe\" command line application.
-----------------------------------------------------------------------------

import qualified Data.Default
import           Data.LinearProgram.GLPK.Solver (GLPOpts(..))
import qualified Data.LinearProgram.GLPK.Solver
import qualified Data.Version
import           MFAPipe.Command (Command(..))
import qualified MFAPipe.Command
import qualified MFAPipe.Version
import           Numeric.LevMar (Options(..))
import qualified Numeric.LevMar
import           System.Console.CmdArgs.Implicit (CmdArgs, Mode, (&=))
import qualified System.Console.CmdArgs.Implicit
import qualified Text.Printf

main :: IO ()
main = System.Console.CmdArgs.Implicit.cmdArgsRun mode >>= MFAPipe.Command.runCommand
  where
    mode :: Mode (CmdArgs Command)
    mode = System.Console.CmdArgs.Implicit.cmdArgsMode $
      System.Console.CmdArgs.Implicit.modes
        [ let
            seed :: Int
            seed = 0
            itMax :: Int
            itMax = 100
            opts :: Options Double
            opts = Numeric.LevMar.defaultOpts
          in
            DoMFAUsingLevMar
              { input = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , output = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , _seed = seed
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "seed"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Seed for random number generator (default: %d)." seed)
              , _itMax = itMax
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "max-iterations"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Maximum iterations (default: %d)." itMax)
              , _optScaleInitMu = optScaleInitMu opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "scale-init-mu"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Scale factor for initial mu (default: %f)." (optScaleInitMu opts))
                  &= System.Console.CmdArgs.Implicit.groupname "Minimization options"
              , _optStopNormInfJacTe = optStopNormInfJacTe opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "stop-norm-inf-jac-te"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Stopping thresholds for ||J^T e||_inf (default: %f)." (optStopNormInfJacTe opts))
                  &= System.Console.CmdArgs.Implicit.groupname "Minimization options"
              , _optStopNorm2Dp = optStopNorm2Dp opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "stop-norm2-dp"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Stopping thresholds for ||Dp||_2 (default: %f)." (optStopNorm2Dp opts))
                  &= System.Console.CmdArgs.Implicit.groupname "Minimization options"
              , _optStopNorm2E = optStopNorm2E opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "stop-norm2-e"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Stopping thresholds for ||e||_2 (default: %f)." (optStopNorm2E opts))
                  &= System.Console.CmdArgs.Implicit.groupname "Minimization options"
              , _optDelta = optDelta opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "delta"
                  &= System.Console.CmdArgs.Implicit.help (Text.Printf.printf "Step used in the difference approximation to the Jacobian (default: %f). If delta<0, the Jacobian is approximated with central differences which are more accurate (but slower!) compared to the forward differences employed by default." (optDelta opts))
                  &= System.Console.CmdArgs.Implicit.groupname "Minimization options"
              }
              &= System.Console.CmdArgs.Implicit.name "mfa-levmar"
              &= System.Console.CmdArgs.Implicit.help "Metabolic Flux Analysis (MFA) using the Levenberg-Marquardt algorithm"
        , let
            opts :: GLPOpts
            opts = Data.LinearProgram.GLPK.Solver.simplexDefaults
          in
            DoFBAUsingSimplex
              { input = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , output = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , _tmLim = tmLim opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "tm-lim"
              , _presolve = presolve opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "presolve"
              }
              &= System.Console.CmdArgs.Implicit.name "fba-simplex"
              &= System.Console.CmdArgs.Implicit.help "Flux Balance Analysis (FBA) using the Simplex algorithm"
        , let
            opts :: GLPOpts
            opts = Data.LinearProgram.GLPK.Solver.mipDefaults
          in
            DoFBAUsingMIP
              { input = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , output = Data.Default.def
                  &= System.Console.CmdArgs.Implicit.typFile
              , _tmLim = tmLim opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "tm-lim"
              , _presolve = presolve opts
                  &= System.Console.CmdArgs.Implicit.explicit
                  &= System.Console.CmdArgs.Implicit.name "presolve"
              }
              &= System.Console.CmdArgs.Implicit.name "fba-mip"
              &= System.Console.CmdArgs.Implicit.help "Flux Balance Analysis (FBA) using Mixed Integer Programming (MIP)"
        ]
        &= System.Console.CmdArgs.Implicit.help "MFAPipe - a command line application for parallel labeling, steady state Metabolic Flux Analysis (MFA) and Flux Balance Analysis (FBA), using the stoichiometric paradigm and Elementary Metabolite Units (EMU) method"
        &= System.Console.CmdArgs.Implicit.program "mfaPipe"
        &= System.Console.CmdArgs.Implicit.summary (Text.Printf.printf "MFAPipe v%s, (C) 2016 Pacific Northwest National Laboratory" (Data.Version.showVersion MFAPipe.Version.version))
