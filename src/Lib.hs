module Lib where

import Data.Function
import Data.Char
import Control.Applicative

---------------------------------------
-- MAIN -------------------------------
---------------------------------------

--loadProgram :: String -> [String]
loadProgram f = f
    & parseProgram
    & expandProgram
    & checkProgram
    & compileProgram

---------------------------------------
-- PARSING ----------------------------
---------------------------------------

parseProgram :: String -> Maybe (String, [Token])
parseProgram p = parse programTokenizer p

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
data Token
    = TokenLabel           String -- Labels control type relevant behaviour, for more information see README
    | TokenSymbol          String
    | TokenString          String
    | TokenUnsignedInteger Word
    | TokenSignedInteger   Int
    deriving (Show, Read, Eq)

-- TODO: Track line and column during parsing for better error messages
-- NOTE: Might be unnecessry / could be inlined into `Parser` monad
newtype TokenWrapper = TokenWrapper (String, Int, Int, Token)

programTokenizer :: Parser [Token]
programTokenizer = sequenceParser spacingParser
    $   unsignedIntegerTokenizer
    <|> stringTokenizer
    <|> labelTokenizer
    <|> symbolTokenizer

unsignedIntegerTokenizer :: Parser Token
unsignedIntegerTokenizer = (TokenUnsignedInteger . read) <$> (nonemptyParser $ spanParser isDigit)

stringTokenizer :: Parser Token
stringTokenizer = TokenString <$> pairParser '"' '"' (many (nrmCharParser <|> escCharParser))

symbolTokenizer = TokenSymbol <$> spanParser (not . isSpace)

signedIntegerTokenizer = undefined -- TODO

labelTokenizer = TokenLabel <$> (charParser (== ':') *> spanParser (not . isSpace))

sequenceParser :: Parser a -> Parser b -> Parser [b]
sequenceParser seperator element = (:) <$> element <*> many (seperator *> element) <|> pure []

spacingParser :: Parser String
spacingParser = spanParser isSpace

spanParser :: (Char -> Bool) -> Parser String
spanParser f =
    Parser $ \i ->
      let (token, rest) = span f i
      in Just (rest, token)

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

---------------------------------------
-- EXPANSION --------------------------
---------------------------------------

expandProgram p = p

---------------------------------------
-- TYPING -----------------------------
---------------------------------------

-- TODO: Check the type correctness of the program
checkProgram p = p

---------------------------------------
-- COMPILING --------------------------
---------------------------------------

-- TODO: Generate assembly
compileProgram p = p

intrinsics :: [(String, String)]
intrinsics =
    [ ("dup", "some assembly")
    , ("swp", "some more assembly")
    ]
