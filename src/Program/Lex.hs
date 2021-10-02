module Program.Lex where

import Data.Char
import Control.Applicative

import Program.Common.Data
import Program.Common.ParserCombinators

step = parse programTokenizer

programTokenizer :: Parser [Token]
programTokenizer = sequenceParser spacingParser expressionTokenizer

expressionTokenizer
    =   unsignedIntegerTokenizer
    <|> stringTokenizer
    <|> labelTokenizer
    <|> symbolTokenizer

unsignedIntegerTokenizer :: Parser Token
unsignedIntegerTokenizer = (TokenUnsignedInteger . read) <$> (nonemptyParser $ spanParser isDigit)

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
symbolTokenizer = TokenSymbol <$> (nonemptyParser $ spanParser isLetter)

signedIntegerTokenizer = undefined -- TODO

labelTokenizer = TokenLabel <$> (charParser (== ':') *> (nonemptyParser $ spanParser isLetter))
