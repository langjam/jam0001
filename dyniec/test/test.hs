{-# LANGUAGE OverloadedStrings #-}

import Parser
import Syntax
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [exprParserTests, typeParserTests, moduleParserTests]

exprParserTests :: TestTree
exprParserTests =
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
        , testCase "Parses a application as left associative" $ -- welp, maybe later I will have a time to fix that
            parser "1 2 3" @?= Right (ExApp (ExApp (ExNum 1) (ExNum 2)) (ExNum 3))
        , testCase "Parses parentheses" $
            parser "(1 2) 3" @?= Right (ExApp (ExApp (ExNum 1) (ExNum 2)) (ExNum 3))
        , testCase "Parses parentheses (2)" $
            parser "1 (2 3)" @?= Right (ExApp (ExNum 1) (ExApp (ExNum 2) (ExNum 3)))
        , testCase "Parses type annotations " $
            parser "1 :: Int" @?= Right (ExTyAnnot (ExNum 1) TyNum)
        ]
  where
    parser = parse exprParser "testfile"
typeParserTests :: TestTree
typeParserTests =
    testGroup
        "Parsing test"
        [ testCase "Parses unit type correctly" $
            parser "Unit" @?= Right TyUnit
        , testCase "Parses int type correctly" $
            parser "Int" @?= Right TyNum
        , testCase "Parses string type correctly" $
            parser "String" @?= Right TyStr
        , testCase "Parses arrow type correctly" $
            parser "Unit -> Unit" @?= Right (TyArrow TyUnit TyUnit)
        , testCase "Parses arrow type with right associativity" $
            parser "Unit -> Int -> String" @?= Right (TyArrow TyUnit (TyArrow TyNum TyStr))
        ]
  where
    parser = parse typeParser "testfile"
moduleParserTests :: TestTree
moduleParserTests =
    testGroup
        "Parsing test"
        [ testCase "Parses single declaration" $
            parser "x = 3" @?= Right (Module [("x", ExNum 3)])
        , testCase "Parses multiple declarations" $
            parser "x = 3;y = 5 " @?= Right (Module [("x", ExNum 3), ("y", ExNum 5)])
        , testCase "Parses multiple declarations with trailing whitespace" $
            parser "x = 3;y = 5\n\n\n\n\n    " @?= Right (Module [("x", ExNum 3), ("y", ExNum 5)])
        ]
  where
    parser = parse moduleParser "testfile"
