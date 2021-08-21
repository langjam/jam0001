{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Functor.Identity (Identity)
import Data.Maybe (maybeToList)
import Data.Text as T (Text, pack, unpack)
import Syntax (Expr(..))
import Text.Parsec hiding (spaces)

-- import Text.Parsec.Char
import qualified Text.Parsec.Token as P

type Parser a = ParsecT T.Text () Identity a
keywords :: [T.Text]
keywords = ["in", "let"]
primops :: [T.Text]
primops = ["add"]
integer :: Parser Integer
integer = do
    sign <- optionMaybe $ char '-'
    digits <- many1 digit
    return $ read $ maybeToList sign ++ digits

identifier :: Parser T.Text
identifier = do
    x <- T.pack <$> many1 letter
    if x `elem` keywords
        then fail "it's a keyword, cannot be used as identifier"
        else if x `elem ` primops
        then fail "it's a primop, cannot be used as identifier"
        else return x

spaces :: Parser ()
spaces = skipMany $ char ' '
baseExpr :: Parser Expr
baseExpr =
    (string "unit" >> return ExUnit) -- unit
        <|> fmap ExNum integer -- integer
        <|> do
            -- let binding
            try (string "let")
            spaces
            var <- identifier
            spaces
            string "="
            spaces
            e1 <- exprParser
            spaces
            string "in"
            spaces
            ExLet var e1 <$> exprParser
        <|> do
            -- bad string
            char '"'
            s <- many $ noneOf "\""
            char '"'
            return $ ExString $ T.pack s
        <|> try
            ( do
                var <- identifier
                spaces
                string "=>"
                spaces
                ExAbst var <$> exprParser
            )
        <|> do
            p <- choice $ map (string. T.unpack) primops
            return $ ExPrimop $ T.pack p
        <|> ExVar <$> identifier
        <|> do
            char ')'
            e <- exprParser
            char ')'
            return e
exprParser :: Parser Expr
exprParser = do
    e <- baseExpr
    do
        try $ do
            spaces >> char '@'
            spaces
            ExAnnot e <$> exprParser
        <|> try (spaces >> ExApp e <$> exprParser)
        <|> return e

-- do
--     e1 <- baseExpr
--     spaces
--     ExApp e1 <$> exprParser
--     <|> do
--         e1 <- baseExpr
--         spaces
--         char '@'
--         spaces
--         ExAnnot e1 <$> exprParser
--     <|> baseExpr
