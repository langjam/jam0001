module Main where

import Data.Text.IO as TIO
import Eval (evalModule)
import Parser (fileParser)
import Syntax
import System.Environment

main :: IO ()
main = do
    (fileName : _) <- getArgs
    fileContent <- TIO.readFile fileName
    case fileParser fileName fileContent of
        Left error -> print error
        Right ast -> mapM_ print $reverse $ evalModule ast
