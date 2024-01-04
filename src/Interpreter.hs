{-# LANGUAGE InstanceSigs #-}

module Interpreter (VMState, defaultVMState, executeMany) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.Char (toLower)
import Data.Foldable (find)
import Data.List (genericLength)
import Data.Maybe (listToMaybe)
import Parser (Token (..), mainParser)
import System.Exit (exitSuccess)
import Text.Parsec (parse)

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
    ("words", wordsCallback),
    (".p", dotPCallback),
    ("cr", crCallback),
    ("+", plusCallback),
    ("-", minusCallback),
    ("/", divisionCallback),
    ("mod", moduloCallback),
    ("*", multiplicationCallback),
    ("exec", execCallback),
    ("if", ifCallback),
    ("define", defineCallback),
    ("pick", pickCallback),
    ("=", equalsCallback),
    (">", moreThanCallback),
    ("roll", rollCallback),
    ("drop", dropCallback),
    ("undefine", undefineCallback)
  ]
  where
    byeCallback = liftIO exitSuccess

    dotCallback = liftIO . print =<< pop

    dotSCallback = liftIO . print . reverse . stack =<< get

    debugCallback = liftIO . print =<< get

    wordsCallback = liftIO . print . (map fst . dictionary) =<< get

    dotPCallback = do
      item <- pop
      case item of
        (Integer i) -> liftIO $ putStr $ show i
        (String s) -> liftIO $ putStr s

    crCallback = liftIO $ putStrLn ""

    plusCallback = do
      item1 <- pop
      item2 <- pop
      case (item2, item1) of
        (Integer a, Integer b) -> push $ Integer (a + b)
        (String a, Integer b) -> push $ String (a ++ show b)
        (Integer a, String b) -> push $ String (show a ++ b)
        (String a, String b) -> push $ String (a ++ b)

    minusCallback = do
      item1 <- pop
      item2 <- pop
      case (item2, item1) of
        (Integer a, Integer b) -> push $ Integer (a - b)
        _ -> throwError "Operation not implemented"

    divisionCallback = do
      item1 <- pop
      item2 <- pop
      case (item2, item1) of
        (Integer _, Integer 0) -> throwError "Division by zero"
        (Integer a, Integer b) -> push $ Integer (a `div` b)
        _ -> throwError "Operation not implemented"

    moduloCallback = do
      item1 <- pop
      item2 <- pop
      case (item2, item1) of
        (Integer _, Integer 0) -> throwError "Division by zero"
        (Integer a, Integer b) -> push $ Integer (a `mod` b)
        _ -> throwError "Operation not implemented"

    multiplicationCallback = do
      item1 <- pop
      item2 <- pop
      case (item2, item1) of
        (Integer a, Integer b) -> push $ Integer (a * b)
        _ -> throwError "Operation not implemented"

    execCallback = do
      item <- pop
      case item of
        String s -> parseAndExecute s
        _ -> throwError "Cannot execute an Integer"

    ifCallback = do
      condition <- pop
      execIfTrue <- pop
      execIfFalse <- pop
      case (condition, execIfTrue, execIfFalse) of
        (String "", _, String s) -> parseAndExecute s
        (Integer 0, _, String s) -> parseAndExecute s
        (_, String s, _) -> parseAndExecute s
        (_, _, _) -> throwError "Not a valid conditional"

    defineCallback = do
      newWord <- pop
      definition <- pop
      case (newWord, definition) of
        (String w, String d) -> addWord w d
        _ -> throwError "Cannot register definition"

    pickCallback = do
      index <- pop
      case index of
        Integer n -> do
          item <- pick n
          push item
        _ -> throwError "Top of stack was not a valid index"

    equalsCallback = do
      item1 <- pop
      item2 <- pop
      if item1 == item2
        then push $ Integer 1
        else push $ Integer 0

    moreThanCallback = do
      item1 <- pop
      item2 <- pop
      case (item1, item2) of
        (Integer i1, Integer i2) ->
          if i1 < i2
            then push $ Integer 1
            else push $ Integer 0
        _ -> throwError "Operation not supported"

    rollCallback = do
      index <- pop
      case index of
        Integer n -> do
          item <- roll n
          push item
        _ -> throwError "Top of stack was not a valid index"

    dropCallback = void pop

    undefineCallback = do
      word <- pop
      case word of
        String w -> rmWord w
        _ -> throwError "Cannot register definition"

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

pick :: Integer -> Interpreter StackItem
pick x = do
  vm <- get
  if x >= 0 && x < genericLength (stack vm)
    then case stackItemAt x (stack vm) of
      Just item -> return item
      Nothing -> throwError "Failed to access stack item"
    else throwError "Index out of bounds"
  where
    stackItemAt index = listToMaybe . drop (fromIntegral index)

roll :: Integer -> Interpreter StackItem
roll x = do
  vm <- get
  if x >= 0 && x < genericLength (stack vm)
    then case removeStackItemAt x (stack vm) of
      (Just item, newStack) -> do
        put vm {stack = newStack}
        return item
      _ -> throwError "Failed to access stack item"
    else throwError "Index out of bounds"
  where
    removeStackItemAt index stk =
      let (left, right) = splitAt (fromIntegral index) stk
       in case right of
            (item : rest) -> (Just item, left ++ rest)
            _ -> (Nothing, stk)

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

addWord :: String -> String -> Interpreter ()
addWord word action = modify appendToDict
  where
    appendToDict vm = vm {dictionary = (word, parseAndExecute action) : dictionary vm}

rmWord :: String -> Interpreter ()
rmWord word = modify removeFromDict
  where
    removeFromDict vm =
      vm {dictionary = filter ((/= word) . fst) (dictionary vm)}

execute :: Token -> Interpreter ()
execute (WordToken word) = interpretWord word
execute (IntegerToken int) = push $ Integer int
execute (StringToken str) = push $ String str

executeMany :: [Token] -> Interpreter ()
executeMany = mapM_ execute

parseAndExecute :: String -> Interpreter ()
parseAndExecute str = do
  let parseResult = parse mainParser "" str
  case parseResult of
    Left err -> throwError $ "error parsing " ++ show err
    Right tokens -> executeMany tokens