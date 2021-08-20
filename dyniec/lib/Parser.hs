module Parser where

import Data.Functor.Identity (Identity)
import Data.Maybe (maybeToList)
import Data.Text as T (Text,pack,unpack)
import Syntax
import Text.Parsec hiding (spaces)
-- import Text.Parsec.Char
import qualified Text.Parsec.Token as P

type Parser a = ParsecT T.Text () Identity a

integer :: Parser Integer
integer = do
    sign <- optionMaybe $ char '-'
    digits <- many1 digit
    return $ read $ maybeToList sign ++ digits

identifier :: Parser T.Text
identifier = T.pack <$> many1 letter

spaces :: Parser ()
spaces = skipMany $ char ' '

exprParser :: Parser Expr
exprParser =
    (string "unit" >> return ExUnit) -- unit
        <|> fmap ExNum integer -- integer
        <|> do -- let binding
              string "let"
              spaces
              var <- identifier
              spaces
              string "="
              spaces
              e1 <- exprParser
              spaces
              string "in"
              spaces
              ExLet var e1  <$> exprParser
        <|> do -- bad string
                char '"'
                s <- many $ noneOf "\""
                char '"'
                return $ ExString $ T.pack s
        <|> ExVar <$> identifier

