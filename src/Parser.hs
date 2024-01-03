module Parser (Token (..), mainParser) where

import Text.Parsec (char, digit, many, many1, noneOf, skipMany1, spaces, try, (<|>))
import Text.Parsec.Char (space)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

data Token
  = WordToken !String
  | IntegerToken !Integer
  | StringToken !String
  deriving (Show)

wordParser :: Parser Token
wordParser = do
  rest <- many1 (noneOf " ")
  return $ WordToken rest

numberParser :: Parser Token
numberParser = do
  numStr <- many1 digit
  case readMaybe numStr of
    Just num -> return $ IntegerToken num
    Nothing -> fail "Invalid number"

stringLiteralParser :: Parser Token
stringLiteralParser = do
  _ <- char 's'
  _ <- char '"'
  _ <- many1 space
  content <- many (noneOf "\"")
  _ <- char '"'
  return $ StringToken content

tokenParser :: Parser Token
tokenParser = try numberParser <|> try stringLiteralParser <|> wordParser

mainParser :: Parser [Token]
mainParser = spaces *> sepEndBy tokenParser (skipMany1 space)