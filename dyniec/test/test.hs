{-# LANGUAGE OverloadedStrings #-}

import Parser
import Syntax
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

parserTests :: TestTree
parserTests =
    testGroup
        "Parsing test"
        [ testCase "Parses integer correctly" $
            parser "234" @?= Right (ExNum 234)
        , testCase "Parses negative integer correctly" $
            parser "-234" @?= Right (ExNum (-234))
        , testCase "Parses let bindings correctly" $
            parser "let x = unit in unit" @?= Right (ExLet "x" ExUnit ExUnit)
        , testCase "Parses strings correctly" $
            parser "\"asdads\"" @?= Right (ExString "asdads")
        , testCase "Parses escaped quotation marks in strings correctly" $
            parser "\"a\\\"sdads\"" @?= Right (ExString "a\"sdads") -- Thats a string `"a\"sdads"` but it needs to be escaped in haskell
        , testCase "Parses abstractions correctly" $
            parser "x => unit" @?= Right (ExAbst "x" ExUnit)
        , testCase "Parses annotations correctly" $
            parser "unit @ unit" @?= Right (ExAnnot ExUnit ExUnit)
        , testCase "Parses a application correctly" $
            parser "unit unit" @?= Right (ExApp ExUnit ExUnit)
        , testCase "Parses a application as left associative" $
            parser "1 2 3" @?= Right (ExApp (ExApp (ExNum 1)(ExNum 2)) (ExNum 3))
        , testCase "Parses parentheses" $
            parser "(1 2) 3" @?= Right (ExApp (ExApp (ExNum 1)(ExNum 2)) (ExNum 3))
        , testCase "Parses parentheses (2)" $
            parser "1 (2 3)" @?= Right (ExApp (ExNum 1) (ExApp (ExNum 2) (ExNum 3)))
        ]
  where
    parser = parse exprParser "testfile"
