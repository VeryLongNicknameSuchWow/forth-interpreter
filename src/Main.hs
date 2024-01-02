module Main (main) where

import Control.Monad (unless)
import Parser (mainParser)
import Text.Parsec (parseTest)

main :: IO ()
main = do
  input <- getLine
  unless (input == "bye") $ parseTest mainParser input >> main
