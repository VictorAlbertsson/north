module Program.Common.ParserCombinators where

import Data.Char
import Control.Applicative

import Program.Common.Data

-- TODO: Track line and column during parsing for better error messages
-- NOTE: Might be unnecessry / could be inlined into `Parser` monad
newtype TokenWrapper = TokenWrapper (String, Int, Int, Token)

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

sequenceParser :: Parser a -> Parser b -> Parser [b]
sequenceParser s e = (:) <$> e <*> many (s *> e) <|> pure []

spacingParser :: Parser String
spacingParser = spanParser isSpace

spanParser :: (Char -> Bool) -> Parser String
spanParser = many . charParser

-- NOTE: Nesting multiple calls to `many` hangs the program
-- since `many` applied to a empty parser is undefined behaviour.
-- Therefore the solution is to apply `nonemptyParser` inbetween
-- calls to `many`.
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

phraseParser :: String -> Parser String
phraseParser p = sequenceA $ fmap (charParser . (==)) p

