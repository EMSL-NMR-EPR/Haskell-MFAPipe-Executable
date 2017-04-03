{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Chemistry.Parser.Text
-- Copyright   :  2016 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports parsers for 'Text' strings in INCA syntax and utility
-- functions for conversion of the resulting data structures.
-----------------------------------------------------------------------------

module Science.Chemistry.Parser.Text
( -- * Non-terminal productions
  -- ** Chemical reactions
  parseMFAReaction , parseFBAReaction
  -- ** Metabolic flux variables
, parseFluxVar
, parseLinearConstraint
  -- ** Metabolite variables
, parseMetaboliteVar
  -- ** Elementary Metabolite Units (EMU)
, parseEMU
  -- ** Fractions
, parseIsotopomerFraction
, parseMassFraction
  -- ** Domain-specific language (DSL)
, parseEMUExpr
) where

import           Control.Applicative (liftA2)
import           Control.Monad.Error.Class (MonadError())
import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text
import           Language.INCA.Conversion.Text
import           Language.INCA.Parser.Text
import           Science.Chemistry.EMU.EMU
import           Science.Chemistry.EMU.IsotopicLabeling.DSL.Evaluate.Class
import           Science.Chemistry.FluxVar
import           Science.Chemistry.MetaboliteVar
import           Science.Chemistry.ReactionNetwork
import qualified Text.Printf

-- | Parse 'MFAReaction' in INCA syntax.
parseMFAReaction
  :: (MonadError (INCAError Text) m)
  => Text
  -- ^ name for metabolic flux variables
  -> Parser (m (ReactionNetwork (FluxVar Text Text) (MetaboliteVar Text) Text))
  -- ^ parser
parseMFAReaction name = fmap (fromINCAMFAReaction name) parseINCAReaction

-- | Parse 'FBAReaction' in INCA syntax.
parseFBAReaction
  :: (MonadError (INCAError Text) m)
  => Text
  -- ^ name for metabolic flux variables
  -> Parser (m (ReactionNetwork (FluxVar Text Text) (MetaboliteVar Text) Text))
  -- ^ parser
parseFBAReaction name = fmap (fromINCAFBAReaction name) parseINCAReaction

-- | Parse metabolic flux variable in INCA syntax.
parseFluxVar
  :: (MonadError (INCAError Text) m)
  => Parser (m (FluxVar Text Text))
  -- ^ parser
parseFluxVar = fmap fromINCAFluxVar parseINCAFluxVar

-- | Parse linear constraint in INCA syntax.
parseLinearConstraint
  :: (MonadError (INCAError Text) m)
  => Parser (m (Map (FluxVar Text Text) Double))
  -- ^ parser
parseLinearConstraint = fmap fromINCALinearConstraint parseINCALinearConstraint

-- | Parse metabolite variable in INCA syntax.
parseMetaboliteVar
  :: (MonadError (INCAError Text) m)
  => Parser (m (MetaboliteVar Text))
  -- ^ parser
parseMetaboliteVar = fmap fromINCAMetaboliteVar parseINCAMetaboliteVar

-- | Parse EMU in INCA syntax with respect to a base.
parseEMU
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> Parser (m (EMU (MetaboliteVar Text)))
  -- ^ parser
parseEMU base = fmap (fromINCAEMU base) parseINCAEMU

-- | Parse isotopomer fraction in INCA syntax with respect to a base.
parseIsotopomerFraction
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> Parser (m (EMUExpr (MetaboliteVar Text)))
  -- ^ parser
parseIsotopomerFraction base = fmap (fromINCAIsotopomerFraction base) parseINCAIsotopomerFraction

-- | Parse mass fraction in INCA syntax with respect to a base.
parseMassFraction
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> Parser (m (EMUExpr (MetaboliteVar Text)))
  -- ^ parser
parseMassFraction base = fmap (fromINCAMassFraction base) parseINCAMassFraction

-- | Parse an EMU expression with respect to a base.
--
-- Happy:
--
-- > expr
-- >   : addExpr { $1 }
-- >
-- > addExpr
-- >   : multiplyExpr '+' addExpr { $1 + $2 }
-- >   | multiplyExpr '-' addExpr { $1 - $2 }
-- >   | multiplyExpr             { $1 }
-- >
-- > multiplyExpr
-- >   : negateExpr '*' multiplyExpr { $1 * $2 }
-- >   | negateExpr '/' multiplyExpr { $1 / $2 }
-- >   | negateExpr                  { $1 }
-- >
-- > negateExpr
-- >   : '-' negateExpr { negate $2 }
-- >   |     atom       { $1 }
-- >
-- > atom
-- >   : massFraction    { $1 }
-- >   | isotopomerFraction { $1 }
-- >   | double          { $1 }
-- >   | '(' expr ')'    { $2 }
--
-- See also: 'Data.Attoparsec.Text.double'.
--
parseEMUExpr
  :: (MonadError (INCAError Text) m)
  => Int
  -- ^ base
  -> Parser (m (EMUExpr (MetaboliteVar Text)))
  -- ^ parser
parseEMUExpr base = parseEMUExprUsing (Data.Attoparsec.Text.choice [parseMassFraction base, parseIsotopomerFraction base])

parseEMUExprUsing
  :: (MonadError (INCAError Text) m)
  => Parser (m (EMUExpr (MetaboliteVar Text)))
  -> Parser (m (EMUExpr (MetaboliteVar Text)))
parseEMUExprUsing = parseAdd
  where
    parseAdd :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseAdd parser = do
      exprL <- parseMultiply parser
      Data.Attoparsec.Text.option exprL $ do
        Data.Attoparsec.Text.skipSpace
        Data.Attoparsec.Text.choice
          [ do
              _ <- Data.Attoparsec.Text.char '+' <?> "expected \"+\""
              Data.Attoparsec.Text.skipSpace
              exprR <- parseAdd parser
              return (liftA2 (+) exprL exprR)
          , do
              _ <- Data.Attoparsec.Text.char '-' <?> "expected \"-\""
              Data.Attoparsec.Text.skipSpace
              exprR <- parseAdd parser
              return (liftA2 (-) exprL exprR)
          ]
    parseMultiply :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseMultiply parser = do
      exprL <- parseNegate parser
      Data.Attoparsec.Text.option exprL $ do
        Data.Attoparsec.Text.skipSpace
        Data.Attoparsec.Text.choice
          [ do
              _ <- Data.Attoparsec.Text.char '*' <?> "expected \"*\""
              Data.Attoparsec.Text.skipSpace
              exprR <- parseMultiply parser
              return (liftA2 (*) exprL exprR)
          , do
              _ <- Data.Attoparsec.Text.char '/' <?> "expected \"/\""
              Data.Attoparsec.Text.skipSpace
              exprR <- parseMultiply parser
              return (liftA2 (/) exprL exprR)
          ]
    parseNegate :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseNegate parser = do
      m <- Data.Attoparsec.Text.option Nothing $ do
        _ <- Data.Attoparsec.Text.char '-' <?> "expected \"-\""
        Data.Attoparsec.Text.skipSpace
        return (Just ())
      expr <- parseAtom parser
      return (maybe id (\() -> fmap negate) m expr)
    parseAtom :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseAtom parser = Data.Attoparsec.Text.choice
      [ parser
      , parseNumber
      , parseFunction parser
      , parseParen parser
      ]
    parseNumber :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text)))
    parseNumber = fmap (return . fromRational . toRational) (Data.Attoparsec.Text.double)
    parseFunction :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseFunction parser = Data.Attoparsec.Text.choice
      [ parseFunction1 parser "negate" negate
      , parseFunction1 parser "abs" abs
      , parseFunction1 parser "signum" signum
      , parseFunction1 parser "recip" recip
      , parseFunction0 parser "pi" pi
      , parseFunction1 parser "exp" exp
      , parseFunction1 parser "log" log
      , parseFunction1 parser "sqrt" sqrt
      , parseFunction2 parser "pow" (**)
      , parseFunction2 parser "logBase" logBase
      , parseFunction1 parser "sin" sin
      , parseFunction1 parser "cos" cos
      , parseFunction1 parser "tan" tan
      , parseFunction1 parser "asin" asin
      , parseFunction1 parser "acos" acos
      , parseFunction1 parser "atan" atan
      , parseFunction1 parser "sinh" sinh
      , parseFunction1 parser "cosh" cosh
      , parseFunction1 parser "tanh" tanh
      , parseFunction1 parser "asinh" asinh
      , parseFunction1 parser "acosh" acosh
      , parseFunction1 parser "atanh" atanh
      ]
    parseFunction0 :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> String -> EMUExpr (MetaboliteVar Text) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseFunction0 _parser name x = do
      _ <- Data.Attoparsec.Text.string (Data.Text.pack name) <?> Text.Printf.printf "expected \"%s\"" name
      _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
      return (return x)
    parseFunction1 :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> String -> (EMUExpr (MetaboliteVar Text) -> EMUExpr (MetaboliteVar Text)) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseFunction1 parser name f = do
      _ <- Data.Attoparsec.Text.string (Data.Text.pack name) <?> Text.Printf.printf "expected \"%s\"" name
      _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
      Data.Attoparsec.Text.skipSpace
      expr <- parseAdd parser
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
      return (fmap f expr)
    parseFunction2 :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> String -> (EMUExpr (MetaboliteVar Text) -> EMUExpr (MetaboliteVar Text) -> EMUExpr (MetaboliteVar Text)) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseFunction2 parser name f = do
      _ <- Data.Attoparsec.Text.string (Data.Text.pack name) <?> Text.Printf.printf "expected \"%s\"" name
      _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
      Data.Attoparsec.Text.skipSpace
      exprL <- parseAdd parser
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
      Data.Attoparsec.Text.skipSpace
      exprR <- parseAdd parser
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
      return (liftA2 f exprL exprR)
    parseParen :: (MonadError (INCAError Text) m) => Parser (m (EMUExpr (MetaboliteVar Text))) -> Parser (m (EMUExpr (MetaboliteVar Text)))
    parseParen parser = do
      _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
      Data.Attoparsec.Text.skipSpace
      expr <- parseAdd parser
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
      return expr
