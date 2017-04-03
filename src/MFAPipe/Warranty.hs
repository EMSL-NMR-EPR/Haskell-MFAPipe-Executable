-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.Warranty
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports functions that are related to the warranty statement
-- for the \"mfaPipe\" command line application.
-----------------------------------------------------------------------------

module MFAPipe.Warranty
( -- * Text output
  hPrintWarranty
  -- * Special cases for standard output
, printWarranty
) where

import           System.IO (Handle)
import qualified System.IO

-- | Computation @'hPrintWarranty' hdl@ writes the warranty statement to the file or channel managed by @hdl@.
hPrintWarranty :: Handle -> IO ()
hPrintWarranty hdl = do
  System.IO.hPutStrLn hdl "================================================================================"
  System.IO.hPutStrLn hdl "This material was prepared as an account of work sponsored by an agency of the"
  System.IO.hPutStrLn hdl "United States Government.  Neither the United States Government nor the United"
  System.IO.hPutStrLn hdl "States Department of Energy, nor Battelle, nor any of their employees, nor any"
  System.IO.hPutStrLn hdl "jurisdiction or organization that has cooperated in the development of these"
  System.IO.hPutStrLn hdl "materials, makes any warranty, express or implied, or assumes any legal"
  System.IO.hPutStrLn hdl "liability or responsibility for the accuracy, completeness, or usefulness or any"
  System.IO.hPutStrLn hdl "information, apparatus, product, software, or process disclosed, or represents"
  System.IO.hPutStrLn hdl "that its use would not infringe privately owned rights."
  System.IO.hPutStrLn hdl ""
  System.IO.hPutStrLn hdl "Reference herein to any specific commercial product, process, or service by"
  System.IO.hPutStrLn hdl "trade name, trademark, manufacturer, or otherwise does not necessarily"
  System.IO.hPutStrLn hdl "constitute or imply its endorsement, recommendation, or favoring by the United"
  System.IO.hPutStrLn hdl "States Government or any agency thereof, or Battelle Memorial Institute. The"
  System.IO.hPutStrLn hdl "views and opinions of authors expressed herein do not necessarily state or"
  System.IO.hPutStrLn hdl "reflect those of the United States Government or any agency thereof."
  System.IO.hPutStrLn hdl ""
  System.IO.hPutStrLn hdl "                      PACIFIC NORTHWEST NATIONAL LABORATORY"
  System.IO.hPutStrLn hdl "                                   operated by"
  System.IO.hPutStrLn hdl "                                    BATTELLE"
  System.IO.hPutStrLn hdl "                                     for the"
  System.IO.hPutStrLn hdl "                       UNITED STATES DEPARTMENT OF ENERGY"
  System.IO.hPutStrLn hdl "                        under Contract DE-AC05-76RL01830"
  System.IO.hPutStrLn hdl "================================================================================"
  System.IO.hPutStrLn hdl ""
  return ()

-- | Write the warranty statement to the standard output device (same as 'hPrintWarranty' 'System.IO.stdout').
printWarranty :: IO ()
printWarranty = hPrintWarranty System.IO.stdout
