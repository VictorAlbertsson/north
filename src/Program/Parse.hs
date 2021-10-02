--{-# LANGUAGE FlexibleContexts #-}
module Program.Parse where

import Data.Function
import Data.Functor.Identity
import Data.Char
import Control.Applicative

newtype Parser a = Parser
    { parse :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) =
      Parser $ \i -> do
        (i', y) <- p i
        Just (i', f y)

instance Applicative Parser where
    pure x = Parser $ \i -> Just (i, x) -- Constant parser
    (Parser p1) <*> (Parser p2) = -- Parser chaining
      Parser $ \i -> do
        (i', f)  <- p1 i
        (i'', x) <- p2 i'
        Just (i'', f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing -- Failing parser
    (Parser p1) <|> (Parser p2) = -- Option parser
        Parser $ \i -> p1 i <|> p2 i

-- TODO: Implement comment handling, either as a `TokenComment` field or as a `Maybe`
-- TODO: Better and more extensive documentation
-- TODO: Add character literal parsing
data Token
    -- Labels control type relevant behaviour, for more information see README
    = TokenLabel           String
    | TokenSymbol          String
    | TokenString          String
    | TokenUnsignedInteger Word
    | TokenSignedInteger   Int
    deriving (Show, Read, Eq)

-- TODO: Track line and column during parsing for better error messages
-- NOTE: Might be unnecessry / could be inlined into `Parser` monad
newtype TokenWrapper = TokenWrapper (String, Int, Int, Token)

step = parse programTokenizer

programTokenizer :: Parser [Token]
programTokenizer = sequenceParser spacingParser expressionTokenizer
  -- <|> isSymbolTokenizer -- BUG: Hangs the prompt when included

expressionTokenizer
    =   unsignedIntegerTokenizer
    <|> stringTokenizer
    <|> labelTokenizer

unsignedIntegerTokenizer :: Parser Token
unsignedIntegerTokenizer = (TokenUnsignedInteger . read) <$> (nonemptyParser $ spanParser isDigit)

stringTokenizer :: Parser Token
stringTokenizer = TokenString <$> pairParser '"' '"' (many (nrmCharParser <|> escCharParser))

symbolTokenizer = TokenSymbol <$> (nonemptyParser $ spanParser isLetter)

signedIntegerTokenizer = undefined -- TODO

labelTokenizer = TokenLabel <$> (charParser (== ':') *> (nonemptyParser $ spanParser isLetter))

sequenceParser :: Parser a -> Parser b -> Parser [b]
sequenceParser s e = (:) <$> e <*> many (s *> e) <|> pure []

-- The problem with the parser hanging has its origin here
-- Combining `sequenceParser` with `spacingParser` results in the expression `(many (many ...` which hangs the parser
spacingParser :: Parser String
spacingParser = spanParser isSpace

spanParser :: (Char -> Bool) -> Parser String
spanParser = many . charParser

nonemptyParser :: Parser [a] -> Parser [a]
nonemptyParser (Parser p) =
    Parser $ \i -> do
      (i', xs) <- p i
      if null xs
        then Nothing
        else Just (i', xs)

pairParser :: Char -> Char -> Parser String -> Parser String
pairParser c1 c2 p
    =  charParser (== c1)
    *> p
    <* charParser (== c2)

charParser :: (Char -> Bool) -> Parser Char
charParser f = Parser p
  where
    p (x:xs)
      | f x       = Just (xs, x)
      | otherwise = Nothing
    p [] = Nothing -- TODO: FIX: Irregular handling of empty input

nrmCharParser :: Parser Char
nrmCharParser = charParser ((&&) <$> (/= '"') <*> (/= '\\'))

escCharParser :: Parser Char
escCharParser
    =   ('"'  <$ phraseParser "\\\"")
    <|> ('\\' <$ phraseParser "\\\\")
    <|> ('\b' <$ phraseParser "\\b")
    <|> ('\f' <$ phraseParser "\\f")
    <|> ('\n' <$ phraseParser "\\n")
    <|> ('\r' <$ phraseParser "\\r")
    <|> ('\t' <$ phraseParser "\\t")

phraseParser :: String -> Parser String
phraseParser p = fmap (charParser . (==)) p & sequenceA

--------------------------------------

--import Text.Parsec as Parsec

--data Token
--    -- Labels control type relevant behaviour, for more information see README
--    = ToLabelToken           String
--    | ToSymbolToken          String
--    | ToStringToken          String
--    | ToUnsignedIntegerToken Integer
--    | ToSignedIntegerToken   Integer
--    deriving (Show, Read, Eq)

-- --parseProgram :: String -> Maybe (String, [Token])
-- parseProgram p = Parsec.parse program "(unknown)" p
-- 
-- program :: ParsecT String u Identity [Token]
-- program = expressionTokens `Parsec.sepBy` Parsec.spaces
-- 
-- expressionTokens
--     =   {-unsignedIntegerToken
--     <|> -}stringToken
--     <|> labelToken
--     <|> symbolToken
--     -- TODO: Add error reporting with `<?>`
-- 
-- 
-- --unsignedIntegerToken = ToUnsignedIntegerToken <$> natural
-- 
-- stringToken = ToStringToken <$> Parsec.between start end body
--   where
--     start = Parsec.char '\"'
--     end   = Parsec.char '\"'
--     body  = Parsec.many (strChar <|> escChar) -- TODO: Add error reporting with `<?>`
-- 
-- labelToken = ToLabelToken <$> (Parsec.char ':' *> (Parsec.many $ Parsec.satisfy isLetter))
-- 
-- symbolToken = ToSymbolToken <$> (Parsec.many $ Parsec.satisfy isLetter)
-- 
-- strChar = Parsec.satisfy $ (&&) <$> (/= '"') <*> (/= '\\')
-- 
-- escChar
--     =   ('"'  <$ Parsec.string "\\\"")
--     <|> ('\\' <$ Parsec.string "\\\\")
--     <|> ('\b' <$ Parsec.string "\\b")
--     <|> ('\f' <$ Parsec.string "\\f")
--     <|> ('\n' <$ Parsec.string "\\n")
--     <|> ('\r' <$ Parsec.string "\\r")
--     <|> ('\t' <$ Parsec.string "\\t")
