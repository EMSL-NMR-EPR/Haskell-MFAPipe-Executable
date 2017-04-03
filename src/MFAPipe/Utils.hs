{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MFAPipe.Utils
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports utility functions for the \"mfaPipe\" command line
-- application.
-----------------------------------------------------------------------------

module MFAPipe.Utils
( LinearFunction(..)
, pluralizeWith
) where

import           Data.Csv (ToField(toField))
import qualified Data.List
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<+>))
import qualified Text.PrettyPrint.Leijen.Text
import qualified Text.Printf

newtype LinearFunction i e = LinearFunction { getLinearFunction :: [(i, e)] }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Pretty i, Pretty e) => Pretty (LinearFunction i e) where
  pretty = Text.PrettyPrint.Leijen.Text.hsep . Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char '+') . map (uncurry (\ix x -> pretty x <+> pretty ix)) . getLinearFunction

instance (Pretty i, Pretty e) => ToField (LinearFunction i e) where
  toField = toField . Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . pretty

pluralizeWith :: (String -> String) -> Int -> String -> String
pluralizeWith f count singular = Text.Printf.printf "%d %s" count ((if count == 1 then id else f) singular)

-- logList_ :: (Monad m) => (String -> m ()) -> [String] -> m ()
-- logList_ k xs = mapM_ (uncurry ((k .) . Text.Printf.printf "[%d] %s")) (zip (enumFromThen 1 2 :: [Integer]) xs)
