-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.Text.Instances
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan instances of 'Pretty'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.PrettyPrint.Leijen.Text.Instances () where

import qualified Data.Text
import qualified Data.Text.Lazy
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))

instance Pretty Data.Text.Text where
  pretty = pretty . Data.Text.Lazy.fromStrict
  {-# INLINE pretty #-}
