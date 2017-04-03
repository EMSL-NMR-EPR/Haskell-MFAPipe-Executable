-----------------------------------------------------------------------------
-- |
-- Module      :  Language.INCA.Parser.Text
-- Copyright   :  2016-17 Pacific Northwest National Laboratory
-- License     :  ECL-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports a parser for 'Text' strings in INCA syntax.
-----------------------------------------------------------------------------

module Language.INCA.Parser.Text
( -- * Terminal productions
  -- ** Symbols
  parseSymbol
  -- ** Arrows
, parseINCAArrow
  -- * Non-terminal productions
  -- ** Chemical reactions
, parseINCAReaction , parseINCAReactionSide , parseINCAReactionPart
  -- ** Metabolic flux variables
, parseINCAFluxVar
, parseINCALinearConstraint
  -- ** Metabolite variables
, parseINCAMetaboliteVar
  -- ** Elementary Metabolite Units (EMU)
, parseINCAEMU , parseINCAEMUList
  -- ** Fractions
, parseINCAIsotopomerFraction
, parseINCAMassFraction
) where

import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict
import           Data.Text (Text)
import qualified Data.Text
import           Language.INCA.Types

-- | Symbol (denoted @SYM@).
--
-- Alex:
--
-- > {
-- > module Language.INCA.Lexer
-- > ( Token(..)
-- > , alexScanTokens
-- > ) where
-- > }
-- >
-- > %wrapper "basic"
-- >
-- > $alpha = [a-zA-Z]
-- > $digit = [0-9]
-- >
-- > tokens :-
-- >   $white+                                ;
-- >   [$alpha $digit] [$alpha $digit \- \_]* { \string -> Symbol string }
-- >
-- > {
-- >   data Token = Symbol String
-- >     deriving (Eq, Ord, Read, Show)
-- > }
--
-- Happy:
--
-- > {
-- > module Language.INCA.Parser
-- > ( parseSymbol
-- > ) where
-- >
-- > import Language.INCA.Lexer
-- > }
-- >
-- > %name happyParseSymbol symbol
-- >
-- > %tokentype { Token }
-- > %token
-- >   SYM { Symbol $$ }
-- >
-- > %%
-- >
-- > symbol
-- >   SYM { $1 }
-- >
-- > {
-- > happyError :: a
-- > happyError = error "parse error"
-- >
-- > parseSymbol :: String -> String
-- > parseSymbol = happyParseSymbol . alexScanTokens
-- > }
--
parseSymbol :: Parser Text
parseSymbol = do
  x <- Data.Attoparsec.Text.satisfy (Data.Attoparsec.Text.inClass "a-zA-Z0-9")
  xs <- Data.Attoparsec.Text.takeWhile (Data.Attoparsec.Text.inClass "a-zA-Z0-9-_")
  return (Data.Text.cons x xs)

-- | An arrow in INCA syntax.
--
-- Happy:
--
-- > arrow
-- >   : '<-'  { INCAArrowL }
-- >   | '->'  { INCAArrowR }
-- >   | '<->' { INCAArrowLR }
--
parseINCAArrow :: Parser INCAArrow
parseINCAArrow = do
  lMaybe <- Data.Attoparsec.Text.option Nothing (fmap Just (Data.Attoparsec.Text.char '<' <?> "expected \"<\""))
  _ <- Data.Attoparsec.Text.char '-' <?> "expected \"-\""
  rMaybe <- Data.Attoparsec.Text.option Nothing (fmap Just (Data.Attoparsec.Text.char '>' <?> "expected \">\""))
  case (lMaybe, rMaybe) of
    (Just _, Just _) -> return INCAArrowLR
    (Just _, Nothing) -> return INCAArrowL
    (Nothing, Just _) -> return INCAArrowR
    (Nothing, Nothing) -> fail "expected \"<-\", \"->\" or \"<->\""

-- | A chemical reaction in INCA syntax.
--
-- Happy:
--
-- > reaction
-- >   : reactionSide arrow reactionSide { INCAReaction $2 $1 $3 }
--
parseINCAReaction :: Parser (INCAReaction (INCAMetaboliteVar Text) Text)
parseINCAReaction = do
  ls <- parseINCAReactionSide <?> "expected chemical reaction side in INCA syntax"
  Data.Attoparsec.Text.skipSpace
  arr <- parseINCAArrow <?> "expected arrow in INCA syntax"
  Data.Attoparsec.Text.skipSpace
  rs <- parseINCAReactionSide <?> "expected chemical reaction side in INCA syntax"
  return (INCAReaction arr ls rs)

-- | A chemical reaction side in INCA syntax.
--
-- Happy:
--
-- > reactionSide
-- >   : sepBy1( reactionPart , '+' ) { INCAReactionSide $1 }
--
-- See also: 'Data.Attoparsec.Text.sepBy1'.
--
parseINCAReactionSide :: Parser (INCAReactionSide (INCAMetaboliteVar Text) Text)
parseINCAReactionSide = do
  reactionPartList <- Data.Attoparsec.Text.sepBy1 (parseINCAReactionPart <?> "expected chemical reaction part in INCA syntax") $ do
    Data.Attoparsec.Text.skipSpace
    _ <- Data.Attoparsec.Text.char '+' <?> "expected \"+\""
    Data.Attoparsec.Text.skipSpace
    return ()
  return (INCAReactionSide reactionPartList)

-- | A chemical reaction part in INCA syntax.
--
-- Happy:
--
-- > reactionPart
-- >   : double metaboliteVar mixingList { INCAReactionPart $1 $2 $3 }
-- >   |        metaboliteVar mixingList { INCAReactionPart 1 $2 $3 }
-- >
-- > mixingList
-- >   : '(' sepBy1( mixing , '+' ) ')' { $2 }
-- >   | {- empty -}                    { [] }
-- >
-- > mixing
-- >   : double atomNameList { ($1, $2) }
-- >   |        atomNameList { (1, $1) }
-- >
-- > atomNameList
-- >   : '{' sepBy1( SYM , ',' ) '}' { $2 }
-- >   | SYM                         { map (\char -> [char]) $1 }
--
-- See also: 'Data.Attoparsec.Text.double' and 'Data.Attoparsec.Text.sepBy1'.
--
parseINCAReactionPart :: Parser (INCAReactionPart (INCAMetaboliteVar Text) Text)
parseINCAReactionPart = do
  stoichiometricC <- Data.Attoparsec.Text.option 1 $ do
    stoichiometricC' <- fmap toRational Data.Attoparsec.Text.double <?> "expected rational number"
    void (Data.Attoparsec.Text.satisfy Data.Attoparsec.Text.isHorizontalSpace)
    Data.Attoparsec.Text.skipSpace
    return stoichiometricC'
  metaboliteVar <- parseINCAMetaboliteVar
  Data.Attoparsec.Text.skipSpace
  mixingList <- Data.Attoparsec.Text.option [] $ do
    _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
    Data.Attoparsec.Text.skipSpace
    xs <- Data.Attoparsec.Text.sepBy parseINCAReactionPart' $ do
      Data.Attoparsec.Text.skipSpace
      _ <- Data.Attoparsec.Text.char '+' <?> "expected \"+\""
      Data.Attoparsec.Text.skipSpace
      return ()
    Data.Attoparsec.Text.skipSpace
    _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
    return xs
  return (INCAReactionPart stoichiometricC metaboliteVar mixingList)

parseINCAReactionPart' :: Parser (Rational, [Text])
parseINCAReactionPart' = do
  mixingP <- Data.Attoparsec.Text.option 1 $ do
    mixingP' <- fmap toRational Data.Attoparsec.Text.double <?> "expected rational number"
    void (Data.Attoparsec.Text.satisfy Data.Attoparsec.Text.isHorizontalSpace)
    Data.Attoparsec.Text.skipSpace
    return mixingP'
  atomNameList <- Data.Attoparsec.Text.choice
    [ do
        _ <- Data.Attoparsec.Text.char '{' <?> "expected \"{\""
        Data.Attoparsec.Text.skipSpace
        symbolList <- Data.Attoparsec.Text.sepBy1 (parseSymbol <?> "expected symbol") $ do
          Data.Attoparsec.Text.skipSpace
          _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
          Data.Attoparsec.Text.skipSpace
          return ()
        Data.Attoparsec.Text.skipSpace
        _ <- Data.Attoparsec.Text.char '}' <?> "expected \"}\""
        return symbolList
    , do
        symbol <- parseSymbol <?> "expected symbol"
        let symbolList = map Data.Text.singleton (Data.Text.unpack symbol)
        return symbolList
    ]
  return (mixingP, atomNameList)

-- | A metabolic flux variable in INCA syntax.
--
-- Happy:
--
-- > fluxVar
-- >   : SYM         { INCAFluxVar $1 Nothing }
-- >   | SYM '.' SYM { INCAFluxVar $1 (Just $3) }
--
parseINCAFluxVar :: Parser (INCAFluxVar Text)
parseINCAFluxVar = do
  name <- parseSymbol <?> "expected symbol"
  dirNameMaybe <- Data.Attoparsec.Text.option Nothing $ do
    _ <- Data.Attoparsec.Text.char '.'
    dirName <- parseSymbol <?> "expected symbol"
    return (Just dirName)
  return (INCAFluxVar name dirNameMaybe)

-- | A linear construct; a linear function of metabolic flux variables in INCA syntax.
--
-- Happy:
--
-- > linearConstraint
-- >   : addExpr { $1 }
-- >
-- > addExpr
-- >   : negateExpr '+' addExpr { $1 + $2 }
-- >   | negateExpr '-' addExpr { $1 - $2 }
-- >   | negateExpr             { $1 }
-- >
-- > negateExpr
-- >   : '-' negateExpr { negate $2 }
-- >   |     atom       { $1 }
-- >
-- > atom
-- >   | double fluxVar { $1 * $2 }
-- >   |        fluxVar { $1 }
--
-- See also: 'Data.Attoparsec.Text.double'.
--
parseINCALinearConstraint :: Parser (Map (INCAFluxVar Text) Double)
parseINCALinearConstraint = parseAdd False Data.Map.Strict.empty
  where
    parseAdd :: Bool -> Map (INCAFluxVar Text) Double -> Parser (Map (INCAFluxVar Text) Double)
    parseAdd isNegated acc = do
      (fluxVar, x) <- parseAtom isNegated
      let new_acc = Data.Map.Strict.insert fluxVar x acc
      Data.Attoparsec.Text.option new_acc $ do
        Data.Attoparsec.Text.skipSpace
        Data.Attoparsec.Text.choice
          [ do
              _ <- Data.Attoparsec.Text.char '+' <?> "expected \"+\""
              Data.Attoparsec.Text.skipSpace
              parseAdd False new_acc
          , do
              _ <- Data.Attoparsec.Text.char '-' <?> "expected \"-\""
              Data.Attoparsec.Text.skipSpace
              parseAdd True new_acc
          ]
    parseAtom :: Bool -> Parser (INCAFluxVar Text, Double)
    parseAtom isNegated = do
      x <- Data.Attoparsec.Text.option 1 $ do
        x' <- Data.Attoparsec.Text.double
        void (Data.Attoparsec.Text.satisfy Data.Attoparsec.Text.isHorizontalSpace)
        Data.Attoparsec.Text.skipSpace
        return x'
      fluxVar <- parseINCAFluxVar
      return (fluxVar, if isNegated then negate x else x)

-- | A metabolite variable in INCA syntax.
--
-- Happy:
--
-- > metaboliteVar
-- >   : SYM         { INCAMetaboliteVar $1 Nothing }
-- >   | SYM '.' SYM { INCAMetaboliteVar $1 (Just $3) }
--
parseINCAMetaboliteVar :: Parser (INCAMetaboliteVar Text)
parseINCAMetaboliteVar = do
  name <- parseSymbol <?> "expected symbol"
  compartmentNameMaybe <- Data.Attoparsec.Text.option Nothing $ do
    _ <- Data.Attoparsec.Text.char '.'
    compartmentName <- parseSymbol <?> "expected symbol"
    return (Just compartmentName)
  return (INCAMetaboliteVar name compartmentNameMaybe)

-- | An EMU in INCA syntax.
--
-- Happy:
--
-- > emu
-- >   : metaboliteVar '#' atomIxList { INCAEMU $1 $3 }
-- >
-- > atomIxList
-- >   : '{' sepBy1( decimal , ',' ) '}' { $2 }
-- >   | decimal                         { map (\char -> read [char]) (show $1) }
--
-- See also: 'Data.Attoparsec.Text.decimal' and 'Data.Attoparsec.Text.sepBy1'.
--
parseINCAEMU :: Parser (INCAEMU (INCAMetaboliteVar Text))
parseINCAEMU = do
  metaboliteVar <- parseINCAMetaboliteVar <?> "expected metabolite variable in INCA syntax"
  _ <- Data.Attoparsec.Text.char '#' <?> "expected \"#\""
  atomIxList <- Data.Attoparsec.Text.choice
    [ do
        _ <- Data.Attoparsec.Text.char '{' <?> "expected \"{\""
        Data.Attoparsec.Text.skipSpace
        intList <- Data.Attoparsec.Text.sepBy1 (Data.Attoparsec.Text.decimal <?> "expected integer") $ do
          Data.Attoparsec.Text.skipSpace
          _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
          Data.Attoparsec.Text.skipSpace
          return ()
        Data.Attoparsec.Text.skipSpace
        _ <- Data.Attoparsec.Text.char '}' <?> "expected \"}\""
        return intList
    , do
        text <- Data.Attoparsec.Text.takeWhile1 (Data.Attoparsec.Text.inClass "0-9") <?> "expected integer"
        let intList = map (\char -> read [char]) (Data.Text.unpack text)
        return intList
    ]
  return (INCAEMU metaboliteVar atomIxList)

-- | A list of EMUs in INCA syntax.
--
--
-- Happy:
--
-- > emuList
-- >   : '(' sepBy1( emu , 'x' ) ')' { $2 }
-- >   | emu                         { [$1] }
--
-- See also: 'Data.Attoparsec.Text.sepBy1'.
--
parseINCAEMUList :: Parser [INCAEMU (INCAMetaboliteVar Text)]
parseINCAEMUList = do
  Data.Attoparsec.Text.choice
    [ do
        _ <- Data.Attoparsec.Text.char '(' <?> "expected \"(\""
        Data.Attoparsec.Text.skipSpace
        emuList <- Data.Attoparsec.Text.sepBy1 (parseINCAEMU <?> "expected EMU in INCA syntax") $ do
          Data.Attoparsec.Text.skipSpace
          _ <- Data.Attoparsec.Text.char 'x' <?> "expected \"x\""
          Data.Attoparsec.Text.skipSpace
          return ()
        Data.Attoparsec.Text.skipSpace
        _ <- Data.Attoparsec.Text.char ')' <?> "expected \")\""
        return emuList
    , fmap (\emu -> [emu]) (parseINCAEMU <?> "expected EMU in INCA syntax")
    ]

-- | A isotopomer fraction in INCA syntax
--
-- Happy:
--
-- > isotopomerFraction
-- >   : emuList ',' isotopomer { INCAIsotopomerFraction $1 $3 }
-- >
-- > isotopomer
-- >   : '{' sepBy1( decimal , ',' ) '}' { $2 }
-- >   | many1( decimal )                { $1 }
--
-- See also: 'Data.Attoparsec.Text.decimal', 'Data.Attoparsec.Text.many1' and 'Data.Attoparsec.Text.sepBy1'.
--
parseINCAIsotopomerFraction :: Parser (INCAIsotopomerFraction (INCAEMU (INCAMetaboliteVar Text)))
parseINCAIsotopomerFraction = do
  emuList <- parseINCAEMUList <?> "expected EMU in INCA syntax"
  _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
  isotopomer <- Data.Attoparsec.Text.choice
    [ do
        _ <- Data.Attoparsec.Text.char '{' <?> "expected \"{\""
        Data.Attoparsec.Text.skipSpace
        intList <- Data.Attoparsec.Text.sepBy1 (Data.Attoparsec.Text.decimal <?> "expected integer") $ do
          Data.Attoparsec.Text.skipSpace
          _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
          Data.Attoparsec.Text.skipSpace
          return ()
        Data.Attoparsec.Text.skipSpace
        _ <- Data.Attoparsec.Text.char '}' <?> "expected \"}\""
        return intList
    , do
        text <- Data.Attoparsec.Text.takeWhile1 (Data.Attoparsec.Text.inClass "0-9") <?> "expected integer"
        let intList = map (\char -> read [char] :: Int) (Data.Text.unpack text)
        return intList
    ]
  return (INCAIsotopomerFraction emuList isotopomer)

-- | A mass fraction in INCA syntax.
--
-- Happy:
--
-- > massFraction
-- >   : emuList ',' 'M' '+' decimal { INCAMassFraction $1 $5 }
--
-- See also: 'Data.Attoparsec.Text.decimal'.
--
parseINCAMassFraction :: Parser (INCAMassFraction (INCAEMU (INCAMetaboliteVar Text)))
parseINCAMassFraction = do
  emuList <- parseINCAEMUList <?> "expected EMU in INCA syntax"
  _ <- Data.Attoparsec.Text.char ',' <?> "expected \",\""
  _ <- Data.Attoparsec.Text.char 'M' <?> "expected \"M\""
  _ <- Data.Attoparsec.Text.char '+' <?> "expected \"+\""
  ix <- Data.Attoparsec.Text.decimal <?> "expected integer"
  return (INCAMassFraction emuList ix)
