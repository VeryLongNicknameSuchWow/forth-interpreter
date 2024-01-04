{-# LANGUAGE InstanceSigs #-}

module Interpreter (VMState, defaultVMState, executeMany) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.Char (toLower)
import Data.Foldable (find)
import Parser (Token (..))
import System.Exit (exitSuccess)

data StackItem = Integer !Integer | String !String
  deriving (Show, Eq)

type WordDefinition = (String, Interpreter ())

data VMMode = Interpretation | Compilation
  deriving (Show, Eq)

data VMState = VMState
  { stack :: ![StackItem],
    dictionary :: ![WordDefinition],
    mode :: !VMMode
  }

instance Show VMState where
  show :: VMState -> String
  show vm =
    unlines
      [ "VMState:",
        "  Stack=" ++ show (reverse $ stack vm) ++ ",",
        "  Dictionary=" ++ show ((map fst . dictionary) vm) ++ ",",
        "  Mode=" ++ show (mode vm)
      ]

type Interpreter a = ExceptT String (StateT VMState IO) a

defaultDict :: [WordDefinition]
defaultDict =
  [ ("bye", byeCallback),
    (".", dotCallback),
    (".s", dotSCallback),
    ("debug", debugCallback),
    ("words", wordsCallback)
  ]
  where
    byeCallback = liftIO exitSuccess

    dotCallback = liftIO . print =<< pop

    dotSCallback = liftIO . print . reverse . stack =<< get

    debugCallback = liftIO . print =<< get

    wordsCallback = liftIO . print . (map fst . dictionary) =<< get

defaultVMState :: VMState
defaultVMState =
  VMState
    { stack = [],
      dictionary = defaultDict,
      mode = Interpretation
    }

pop :: Interpreter StackItem
pop = do
  vm <- get
  case stack vm of
    [] -> throwError "Pop from an empty stack"
    (x : xs) -> do
      put vm {stack = xs}
      return x

push :: StackItem -> Interpreter ()
push x = modify pushOntoStack
  where
    pushOntoStack vm = vm {stack = x : stack vm}

lookupWord :: String -> [WordDefinition] -> Maybe (Interpreter ())
lookupWord w = fmap snd . find match
  where
    match (key, _) = map toLower key == map toLower w

interpretWord :: String -> Interpreter ()
interpretWord w = do
  vm <- get
  case lookupWord w (dictionary vm) of
    Just action -> action
    Nothing -> throwError $ "Word \'" ++ w ++ "\' not in dictionary"

execute :: Token -> Interpreter ()
execute (WordToken word) = interpretWord word
execute (IntegerToken int) = push $ Integer int
execute (StringToken str) = push $ String str

executeMany :: [Token] -> Interpreter ()
executeMany = mapM_ execute