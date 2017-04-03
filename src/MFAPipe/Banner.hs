-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.Banner
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports functions that are related to the banner for the
-- \"mfaPipe\" command line application.
-----------------------------------------------------------------------------

module MFAPipe.Banner
( -- * Text output
  hPrintBanner
  -- * Special cases for standard output
, printBanner
) where

import           System.IO (Handle)
import qualified System.IO

-- | Computation @'hPrintBanner' hdl@ writes the banner to the file or channel managed by @hdl@.
hPrintBanner :: Handle -> IO ()
hPrintBanner hdl = do
  -- Generated using <http://patorjk.com/software/taag/>
  --
  -- Font: ANSI Shadow
  -- Character Width: Default
  -- Character Height: Default
  System.IO.hPutStrLn hdl ""
  System.IO.hPutStrLn hdl "             ███╗   ███╗███████╗ █████╗ ██████╗ ██╗██████╗ ███████╗"
  System.IO.hPutStrLn hdl "             ████╗ ████║██╔════╝██╔══██╗██╔══██╗██║██╔══██╗██╔════╝"
  System.IO.hPutStrLn hdl "             ██╔████╔██║█████╗  ███████║██████╔╝██║██████╔╝█████╗"
  System.IO.hPutStrLn hdl "             ██║╚██╔╝██║██╔══╝  ██╔══██║██╔═══╝ ██║██╔═══╝ ██╔══╝"
  System.IO.hPutStrLn hdl "             ██║ ╚═╝ ██║██║     ██║  ██║██║     ██║██║     ███████╗"
  System.IO.hPutStrLn hdl "             ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚══════╝"
  System.IO.hPutStrLn hdl "                                Version: 0.1.0.0"
  System.IO.hPutStrLn hdl ""
  System.IO.hPutStrLn hdl "                              Flux Balance Analysis"
  System.IO.hPutStrLn hdl "                             Metabolic Flux Analysis"
  System.IO.hPutStrLn hdl "                           Elementary Metabolite Units"
  System.IO.hPutStrLn hdl "                          Isotopomer and Mass Fractions"
  System.IO.hPutStrLn hdl ""
  return ()

-- | Write the banner to the standard output device (same as 'hPrintBanner' 'System.IO.stdout').
printBanner :: IO ()
printBanner = hPrintBanner System.IO.stdout
