module Program.Lex where

import Data.Char
import Control.Applicative

import Program.Common.Data
import Program.Common.ParserCombinators

step = parse programTokenizer

programTokenizer :: Parser [Token]
programTokenizer = sequenceParser spacingParser expressionTokenizer

expressionTokenizer
    =   naturalTokenizer
    <|> stringTokenizer
    <|> symbolTokenizer

naturalTokenizer :: Parser Token
naturalTokenizer = (TokenNatural . read) <$> (nonemptyParser $ spanParser isDigit)

stringTokenizer :: Parser Token
stringTokenizer = TokenString <$> pairParser '"' '"' (many (strCharParser <|> escCharParser))
  where
    strCharParser = charParser ((&&) <$> (/= '"') <*> (/= '\\'))
    escCharParser
        =   ('"'  <$ phraseParser "\\\"")
        <|> ('\\' <$ phraseParser "\\\\")
        <|> ('\b' <$ phraseParser "\\b")
        <|> ('\f' <$ phraseParser "\\f")
        <|> ('\n' <$ phraseParser "\\n")
        <|> ('\r' <$ phraseParser "\\r")
        <|> ('\t' <$ phraseParser "\\t")

symbolTokenizer :: Parser Token
symbolTokenizer = TokenSymbol <$> (nonemptyParser $ spanParser isLetter)

stackTokenizer = undefined -- TODO

integerTokenizer  = undefined -- TODO
rationalTokenizer = undefined -- TODO
realTokenizer     = undefined -- TODO
complexTokenizer  = undefined -- TODO
