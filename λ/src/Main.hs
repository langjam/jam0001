module Main where

import System.Environment(getArgs)

import Preprocess(comments, codeBlocks, applyComments, Code, inlineComments)
import Parser(program, parse)
import Val
import Eval
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad (join)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering), stdout)

getCode :: IO String
getCode = do
  (path : _) <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

runOnce :: Args ->  Code -> ExceptT EvalError IO (Maybe Code)
runOnce args input = do
  comms <- parse comments input
  codes <- parse codeBlocks input

  let code = inlineComments comms codes
  expr <- parse program code

  (newComms, val) <- evalWithComments args comms expr
  case val of
    Unit -> pure ()
    _ -> liftIO $ print val

  pure
    if comms == newComms
    then Nothing
    else Just $ applyComments newComms codes

runAll :: Args -> Code -> IO ()
runAll args code = do
  result <- runExceptT $ runOnce args code
  case result of
    Left "Halt" -> pure ()
    Left err -> putStrLn err
    Right Nothing -> pure ()
    Right (Just newCode) -> runAll args newCode

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  join $ runAll <$> (tail <$> getArgs) <*> getCode
