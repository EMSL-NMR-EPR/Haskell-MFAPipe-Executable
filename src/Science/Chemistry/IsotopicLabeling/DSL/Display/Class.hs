{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.IsotopicLabeling.DSL.Display.Class
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports types and functions for displaying domain-specific
-- language (DSL) types.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Science.Chemistry.IsotopicLabeling.DSL.Display.Class
( Display(..)
, display
, displayT
) where

import           Data.Csv (ToField(toField))
import           Data.Functor.Sum (Sum(..))
import           Data.Injective (Fix, FloatingF(..), FractionalF(..), NumF(..))
import qualified Data.Injective
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import           Text.PrettyPrint.Leijen.Text (Pretty(..), Doc, (<>), (<+>))
import qualified Text.PrettyPrint.Leijen.Text

class (Functor f) => Display f where
  displayAlg :: f Doc -> Doc

display :: (Display f) => Fix f -> Doc
display = Data.Injective.cata displayAlg

displayT :: (Display f) => Fix f -> Text
displayT = Text.PrettyPrint.Leijen.Text.displayT . Text.PrettyPrint.Leijen.Text.renderCompact . display

instance (Display f, Display g) => Display (f `Sum` g) where
  displayAlg (InL x) = displayAlg x
  displayAlg (InR x) = displayAlg x

instance Display NumF where
  displayAlg (Add xL xR) = Text.PrettyPrint.Leijen.Text.parens (xL <+> Text.PrettyPrint.Leijen.Text.char '+' <+> xR)
  displayAlg (Subtract xL xR) = Text.PrettyPrint.Leijen.Text.parens (xL <+> Text.PrettyPrint.Leijen.Text.char '-' <+> xR)
  displayAlg (Multiply xL xR) = Text.PrettyPrint.Leijen.Text.parens (xL <+> Text.PrettyPrint.Leijen.Text.char '*' <+> xR)
  displayAlg (Negate x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "negate") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Abs x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "abs") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Signum x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "signum") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (FromInteger x) = Text.PrettyPrint.Leijen.Text.integer x

instance Display FractionalF where
  displayAlg (Divide xL xR) = Text.PrettyPrint.Leijen.Text.parens (xL <+> Text.PrettyPrint.Leijen.Text.char '/' <+> xR)
  displayAlg (Recip x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "recip") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (FromRational x) = Text.PrettyPrint.Leijen.Text.double (fromRational x)

instance Display FloatingF where
  displayAlg Pi = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "pi") <> Text.PrettyPrint.Leijen.Text.parens Text.PrettyPrint.Leijen.Text.empty
  displayAlg (Exp x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "exp") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Log x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "log") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Sqrt x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "sqrt") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Power xL xR) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "pow") <> Text.PrettyPrint.Leijen.Text.parens (xL <> Text.PrettyPrint.Leijen.Text.comma <+> xR)
  displayAlg (LogBase xL xR) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "logBase") <> Text.PrettyPrint.Leijen.Text.parens (xL <> Text.PrettyPrint.Leijen.Text.comma <+> xR)
  displayAlg (Sin x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "sin") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Cos x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "cos") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Tan x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "tan") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcSin x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "asin") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcCos x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "acos") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcTan x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "atan") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Sinh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "sinh") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Cosh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "cosh") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (Tanh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "tanh") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcSinh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "asinh") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcCosh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "acosh") <> Text.PrettyPrint.Leijen.Text.parens x
  displayAlg (ArcTanh x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack "atanh") <> Text.PrettyPrint.Leijen.Text.parens x

instance (Display f) => Pretty (Fix f) where
  pretty = display

instance (Display f) => ToField (Fix f) where
  toField = toField . displayT
