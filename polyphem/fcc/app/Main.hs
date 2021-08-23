{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Text.Megaparsec hiding (State,count)
import Text.Pretty.Simple
import System.Environment

import Parser
import TypeSystem
import Pretty
import Eval


inferFor :: T.Text -> IO ()
inferFor x = 
       case runParser exprP "" x of
           Left bundle -> pPrint (errorBundlePretty bundle)
           Right r -> case inferExpr emptyTyenv r of
              Left l -> pPrint l
              Right r' -> pPrint $ ppsignature (T.unpack x,r')


runFile :: String -> IO ()
runFile fn = do
      src <- readFile fn
      case runParser progP fn $ T.pack src of
           Left bundle -> pPrint (errorBundlePretty bundle)
           Right ast -> case inferAST emptyTyenv ast of
              Left l -> pPrint l
              Right r' -> do res <- evalAST ast 
                             case res of
                                  Left s -> pPrint s
                                  Right v -> pPrint v 


main :: IO ()
main = do 
          runTests
          args <- getArgs
          case args of
            [fname] -> runFile fname
            _ -> putStrLn "Call fcc <filename>"

runTests :: IO ()
runTests
  = do {-parseTest progP "module [Foo|This is the Foo Module] where"
       parseTest progP
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = \n -2"
       parseTest progP
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = \n 2.33"
       parseTest progP
         "module [Foo|This is the Foo Module] where\n\n[foo|A bndr] = 'Ä'"
       parseTest progP
         "module [Foo|This is the Foo Module] where\n\n[foo a b e |A bndr] = False"
-}

       parseTest apP "a b"
       parseTest apP "[\\a -> a|lamda] b"
       parseTest apP "[\\a b -> a b|lamda] c" 
       inferFor "[\\a -> let [x|x] = a in x|lamda]" 
       inferFor "[\\a -> let [x|x] = a in x|lamda] 5.5" 
       inferFor "[\\a b-> let [x|x] = a in x|lamda] 5.5" 
       inferFor "[\\a b c -> let [x|x] = a in x|lamda] 5.5" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in a+b|lamda]" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in a|lamda]" 
       inferFor "[\\a b-> let [x|x] = a in x|lamda]" 
       inferFor "let [x|x] = 1 [y|y] = 3 in x+y" 
       inferFor "let [x|x] = 1 in x+x" 
       inferFor "let [x|x] = 1 in x" 
       inferFor "let [x|x] = 1 in [\\a -> a+a|lamda] x" 
       inferFor "1+1" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in a b|lamda]" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x y|lamda]" 
       inferFor "[\\a b -> let [x u|x]  = a u [y|y] = b in x y|lamda]" 
       inferFor "1-1" 
       inferFor "1*1" 
       inferFor "1.3+1.3" 
       inferFor "[\\a b -> let [x|x] = 3 [y|y] = 4 in x+y|lamda] 5 6" 
       inferFor "[\\a b ->  a+b|lamda] 5 6" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x+x|lamda] 5.5 " 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x+y|lamda] 5.5 6.6" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x+x|lamda] 5.5 6.6" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x+x|lamda] 5 6" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in x+y|lamda] 5.5  6.6 " 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in 5+x|lamda] 5 6" 
       inferFor "[\\a b -> let [x|x] = a [y|y] = b in 5+6|lamda] 5 6" 
       inferFor "1" 

{-
       f <- T.readFile "meep.fcc"
       case runParser progP "meep.fcc" f of
           Left bundle -> putStr (errorBundlePretty bundle)
           Right r -> pPrint r -}
