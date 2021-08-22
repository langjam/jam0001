module Main where

import Data.Text.IO as TIO
import Parser (fileParser)
import Syntax
import System.Environment

main :: IO ()
main = do
    (fileName : _) <- getArgs
    fileContent <- TIO.readFile fileName
    let ast = fileParser fileName fileContent
    print ast
