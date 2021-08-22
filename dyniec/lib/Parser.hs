{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Functor.Identity (Identity)
import Data.Maybe (maybeToList)
import Data.Text as T (Text, pack, unpack)
import Syntax (Expr (..), Module (..), Type (..))
import Text.Parsec hiding (spaces)

-- import Text.Parsec.Char
import qualified Text.Parsec.Token as P

type Parser a = ParsecT T.Text () Identity a
keywords :: [T.Text]
keywords = ["in", "let"]
primops :: [T.Text]
primops = ["succ","add"]
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
        else
            if x `elem` primops
                then fail "it's a primop, cannot be used as identifier"
                else return x

spaces :: Parser ()
spaces = skipMany $ oneOf " \n\t"
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
            p <- choice $ map (string . T.unpack) primops
            return $ ExPrimop $ T.pack p
        <|> ExVar <$> identifier
        <|> do
            char '('
            e <- exprParser
            char ')'
            return e
exprParser :: Parser Expr
exprParser = do
    e <- baseExpr
    do
        try
            ( do
                spaces
                char '@'
                spaces
                ExAnnot e <$> exprParser
            )
        <|> try
            ( do
                spaces
                string "::"
                spaces
                ExTyAnnot e <$> typeParser
            )
        <|> try (spaces >> ExApp e <$> exprParser)
        <|> return e

baseTypeParser :: Parser Type
baseTypeParser =
    do
        string "Int"
        return TyNum
        <|> do
            string "String"
            return TyStr
        <|> do
            string "Unit"
            return TyUnit
typeParser :: Parser Type
typeParser = do
    t <- baseTypeParser
    try $
        do
            spaces
            string "->"
            spaces
            TyArrow t <$> typeParser
            <|> return t

moduleParser :: Parser Module
moduleParser = do
    declarations <- declarationParser `sepBy` string ";"
    spaces
    eof
    return $ Module declarations
  where
    declarationParser :: Parser (T.Text, Expr)
    declarationParser = do
        spaces
        name <- identifier
        spaces
        string "="
        spaces
        e <- exprParser
        return (name, e)

fileParser :: SourceName -> Text -> Either ParseError Module
fileParser = parse moduleParser
