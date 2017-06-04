-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.Isotopomer
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types for the representation of isotopomers.
-----------------------------------------------------------------------------

module Science.Chemistry.IsotopicLabeling.Isotopomer
( -- * Isotopomer type
  Isotopomer(..) , getIsotopomer
  -- * Utilities
, isotopomers
) where

import           Control.Applicative (liftA2)
import           Data.Csv (ToField(toField))
import qualified Data.List
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import qualified Text.PrettyPrint.Leijen.Text

-- | An isotopomer.
data Isotopomer = Isotopomer {-# UNPACK #-} !Int [Int]
  deriving (Eq, Ord, Read, Show)

getIsotopomer :: Isotopomer -> [Int]
getIsotopomer (Isotopomer _base xs) = xs
{-# INLINE getIsotopomer #-}

instance Pretty Isotopomer where
  pretty = Text.PrettyPrint.Leijen.Text.braces . Text.PrettyPrint.Leijen.Text.hcat . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char ',') . map Text.PrettyPrint.Leijen.Text.int . getIsotopomer

instance ToField Isotopomer where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

-- | @isotopomers base n@ is the list of 'Isotopomers' of @n@ atoms with
-- respect to @base@.
isotopomers
  :: Int
  -- ^ base
  -> Int
  -- ^ number of atoms
  -> [Isotopomer]
isotopomers base n = map (Isotopomer base) $ foldr (liftA2 (++)) <*> replicate (n - 1) $ map pure (enumFromThenTo 0 1 (base - 1))
