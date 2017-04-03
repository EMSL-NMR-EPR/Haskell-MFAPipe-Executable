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
-- This module exports the 'Version' of the \"mfaPipe\" executable.
-----------------------------------------------------------------------------

module MFAPipe.Version
( version
) where

import           Data.Version (Version)
import qualified Data.Version

-- | The version of the \"mfaPipe\" executable.
version :: Version
version = Data.Version.makeVersion [0,1,0,0]
