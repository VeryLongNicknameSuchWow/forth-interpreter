module Parser (Token (..), mainParser) where

import Text.Parsec (char, digit, many, many1, noneOf, optionMaybe, skipMany1, spaces, string, try, (<|>))
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
  maybeMinus <- optionMaybe (char '-')
  numStr <- many1 digit
  let fullNumStr = maybe "" (const "-") maybeMinus ++ numStr
  case readMaybe fullNumStr of
    Just num -> return $ IntegerToken num
    Nothing -> fail "Invalid number"

stringParser :: Parser Token
stringParser = do
  _ <- char '"'
  content <- many character
  _ <- char '"'
  return $ StringToken content
  where
    character :: Parser Char
    character = try (string "\\\"" >> return '"') <|> noneOf "\""

tokenParser :: Parser Token
tokenParser = try numberParser <|> try stringParser <|> wordParser

mainParser :: Parser [Token]
mainParser = spaces *> sepEndBy tokenParser (skipMany1 space)