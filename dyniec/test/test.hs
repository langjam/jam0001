{-# LANGUAGE OverloadedStrings #-}
import Syntax
import Parser
import Test.Tasty ( testGroup, defaultMain, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Text.Parsec (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

parserTests :: TestTree
parserTests = testGroup "Parsing test"
  [ testCase "Parses integer correctly" $
      parser "234"  @?=  Right ( ExNum 234)
    , testCase "Parses negative integer correctly" $
      parser "-234"  @?= Right ( ExNum (-234))
    , testCase "Parses let bindings correctly" $
      parser "let x = 5 in 5"  @?= Right ( ExLet "x" (ExNum 5) (ExNum 5))
    , testCase "Parses strings correctly" $
      parser "\"asdads\"" @?= Right (ExString "asdads")
    , testCase "Parses escaped quotation marks in strings correctly" $
      parser "\"a\\\"sdads\"" @?= Right (ExString "asdads") -- Thats a string `"a\"sdads"` but it needs to be escaped in haskell
  ]
  where 
      parser = parse exprParser "testfile"
