module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT (runStateT))
import Interpreter (VMState, defaultVMState, executeMany)
import Parser (parseString)

run :: VMState -> IO ()
run currentState = do
  input <- getLine
  let parseResult = parseString input
  case parseResult of
    Left err -> do
      putStrLn $ "Error parsing: " ++ err
      run currentState
    Right tokens -> do
      (result, newState) <- runStateT (runExceptT (executeMany tokens)) currentState
      case result of
        Left err -> putStrLn $ "Runtime error: " ++ err
        Right _ -> return ()
      run newState

main :: IO ()
main = run defaultVMState
