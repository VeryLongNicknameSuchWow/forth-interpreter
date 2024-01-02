module Parser (Token, mainParser) where

import Text.Parsec
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
  _ <- many $ char ' '
  content <- many (noneOf "\"")
  _ <- char '"'
  return $ StringToken content

tokenParser :: Parser Token
tokenParser = try numberParser <|> stringLiteralParser <|> wordParser

mainParser :: Parser [Token]
mainParser = sepBy tokenParser spaces