{-# LANGUAGE TupleSections #-}
module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe

import Core
import Lexer

type Parser = Parsec String ()

pmanydecl :: Parser [Decl] 
pmanydecl = do
    -- top level declarations
    cs <- many $ do
        reservedOp "#"
        n <- identifier
        reservedOp "#"
        c <- many $ satisfy ('#' /=)
        reservedOp "##"
        return $ Decl n (Commt c)
    ls <- many $ do
        n <- identifier
        reservedOp ":"
        e <- pexp
        reservedOp "!"
        return $ Decl n e
    do
        reserved "main"
        reservedOp ":"
        (((cs++ls)++) . (:[])) . Decl "main" . Presentation <$> many (pvar <|> parens pexp)

pexp, pexp' :: Parser Expr
pexp = do
     x <- pnotapp
     (do 
         xs <- many1 pnotapp
         return (foldl App x xs)) <|> return x
pexp' = do
     x <- pnotapp'
     (do 
         xs <- many1 pnotapp'
         return (foldl App x xs)) <|> return x

pnotapp, plam, pint, pvar, plist :: Parser Expr
pnotapp = opexpr <|> pnotapp'
pnotapp' = plam <|> plist <|> pcaselist <|> pint <|> pchar <|> pvar <|> parens pexp

plam = do
     reserved "with"
     n <- identifier
     reserved "do"
     e <- pexp
     reserved "please"
     return $ Lambda n e
pint = Lit . I <$> natural
pvar = Var <$> identifier
pchar = do
    char '\''
    c <- letter
    char '\''
    return $ Lit $ C c
plist = choice [try $ reservedOp "[" >> reservedOp "]" >> return (List []), do
    reservedOp "["
    ls <- many $ do
        e <- pexp
        optional $ reservedOp ","
        return e
    reservedOp "]"
    return $ List ls]
pcaselist = do
    reserved "when"
    e0 <- pexp
    reserved "is"
    reservedOp "["
    reservedOp "]"
    reserved "do"
    e1 <- pexp
    reserved "please"
    reserved "for"
    n1 <- identifier
    reservedOp ":"
    n2 <- identifier
    reserved "do"
    e2 <- pexp
    reserved "please"
    return $ CaseList e0 e1 n1 n2 e2

opexpr = buildExpressionParser table pexp'
    where
         table = [ 
             [binary ":" Prepend AssocRight] 
           , [binary "*" (\x y -> Arith $ Mult x y) AssocLeft ]
           , [binary "+" (\x y -> Arith $ Add x y) AssocLeft, binary "-" (\x y -> Arith $ Sub x y) AssocLeft ]
           ]

         binary name fun = Infix (reservedOp name >> return fun)


