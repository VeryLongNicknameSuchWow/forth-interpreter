{-# LANGUAGE InstanceSigs #-}

module Interpreter (VMState, defaultVMState, executeMany) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.Foldable (find)
import Parser (Token (..))
import System.Exit (exitSuccess)

data VMState = VMState
  { stack :: ![Integer],
    dictionary :: ![(String, ForthInterpreter ())],
    buffer :: !String,
    mode :: !VMMode
  }

instance Show VMState where
  show :: VMState -> String
  show vm =
    unlines
      [ "VMState:",
        "  Stack=" ++ show (stack vm) ++ ",",
        "  Buffer=" ++ show (buffer vm) ++ ",",
        "  Dictionary=" ++ show ((map fst . dictionary) vm) ++ ",",
        "  Mode=" ++ show (mode vm)
      ]

data VMMode = Interpretation | Compilation
  deriving (Show, Eq)

type ForthInterpreter a = ExceptT String (StateT VMState IO) a

defaultDict :: [(String, ForthInterpreter ())]
defaultDict =
  [ ("bye", liftIO exitSuccess),
    (".", liftIO . print =<< pop)
  ]

defaultVMState :: VMState
defaultVMState =
  VMState
    { stack = [],
      dictionary = defaultDict,
      buffer = "",
      mode = Interpretation
    }

pop :: ForthInterpreter Integer
pop = do
  vm <- get
  case stack vm of
    [] -> throwError "Pop from an empty stack"
    (x : xs) -> do
      put vm {stack = xs}
      return x

push :: Integer -> ForthInterpreter ()
push x = modify pushOntoStack
  where
    pushOntoStack vm = vm {stack = x : stack vm}

lookupWord :: String -> [(String, ForthInterpreter ())] -> Maybe (ForthInterpreter ())
lookupWord w = fmap snd . find match
  where
    match (key, _) = key == w

interpretWord :: String -> ForthInterpreter ()
interpretWord w = do
  vm <- get
  case lookupWord w (dictionary vm) of
    Just action -> action
    Nothing -> throwError $ "Word \'" ++ w ++ "\' not in dictionary"

execute :: Token -> ForthInterpreter ()
execute (IntegerToken number) = push number
execute (WordToken word) = interpretWord word
execute _ = return ()

executeMany :: [Token] -> ForthInterpreter ()
executeMany = mapM_ execute