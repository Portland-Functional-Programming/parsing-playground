module Parser where

import Data.List.NonEmpty (NonEmpty(..))

-- |The main Parser type. It's a function alias for now but we may want to wrap it later.
type Parser a = String -> Either String (a, String)

-- |A Parser that parses the given character from the input String.
char :: Char -> Parser Char
char c = \s -> case s of
                 [] -> Left "Unexpected end-of-input."
                 (c':rest) -> if c == c'
                              then Right (c, rest)
                              else Left ("Expected '" <> show c <> "' but got '" <> show c' <> "'.")

-- |Creates a Parser that always succeeds with the given value and consumes no
-- input.
always :: a -> Parser a
always x = \s -> Right (x, s)

-- |Creates a Parser that parses zero or more of what the given Parser parses.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = \s -> zeroOrMore' [] s
  where zeroOrMore' acc s = case p s of
                              Left _ -> Right (reverse acc, s)
                              Right (x, rest) -> zeroOrMore' (x:acc) rest

-- |Creates a Parser that parses one or more of what the given Parser parses.
oneOrMore :: Parser a -> Parser (NonEmpty a)
oneOrMore p = \s -> case p s of
  Left err -> Left err
  Right (v, rest) -> case (zeroOrMore p) rest of
    Left _ -> Right (v :| [], rest)
    Right (vs, rest') -> Right (v :| vs, rest')

-- |Creates a Parser that attempts to parse the input using the first parser. If
-- that succeeds that parse result is the result; if it fails the result is the
-- result of parsing the input using the second parser.
either :: Parser a -> Parser a -> Parser a
either p1 p2 = \s -> case p1 s of
  Left _ -> p2 s
  Right result -> Right result

andThen :: Parser a -> Parser b -> Parser b
andThen p1 p2 = \s -> case p1 s of
  Left err -> Left err
  Right (_, rest) -> p2 rest
