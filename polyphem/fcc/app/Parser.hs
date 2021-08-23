{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State,count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State

import AST

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

operators :: [[Operator Parser Expr]]
operators
  = [[Prefix (Neg <$ symbol "-")],
     [InfixL (BinOp Mul <$ symbol "*"), InfixL (BinOp Div <$ symbol "/")],
     [InfixL (BinOp Add <$ symbol "+"), InfixL (BinOp Sub <$ symbol "-")]]

exprP :: Parser Expr
exprP = makeExprParser termP operators

termP :: Parser Expr
termP = choice [literalP, letP, funP, parens exprP]

literalP :: Parser Expr
literalP = Lit (Cmt "") <$> (charP <|> numberP <|> boolP)

funP :: Parser Expr
funP = try apP <|> try lamP <|> varP

varP :: Parser Expr
varP = Var (Cmt "") <$> nameP

lamP :: Parser Expr
lamP
  = do ((ps, b), c) <- cmtP $
                         do _ <- symbol "\\"
                            ps <- many nameP
                            _ <- symbol "->"
                            b <- bodyP
                            return (ps, b)
       return $ Lam c ps b

apP :: Parser Expr
apP
  = do e <- eitherP lamP varP
       space
       case e of
           Left e' -> g e'
           Right e' -> g e'

  where g e
          = do es <- some exprP
               return $ Ap e es

letP :: Parser Expr
letP
  = do 
       _ <- symbol "let"
       --bs <- L.indentBlock scn p
       bs <- many (lexeme binderP)
       _ <- symbol "in"
       Let bs <$> bodyP

--  where p = do return (L.IndentSome Nothing return binderP)

binderP :: Parser Binder
binderP
  = do ilevel <- L.indentLevel
       ((x, params), c) <- cmtP $
                             do n <- nameP
                                ps <- many nameP
                                return (n, ps)
       _ <- symbol "="
       _ <- L.indentGuard sc GT ilevel
       b <- bodyP
       space
       return $ Binder c x params b

integerP :: Parser Integer
integerP = lexeme L.decimal

floatP :: Parser Double
floatP = lexeme L.float

signedIntP :: Parser Literal
signedIntP = LInt <$> L.signed sc integerP

signedFloatP :: Parser Literal
signedFloatP = LFloat <$> L.signed sc floatP

numberP :: Parser Literal
numberP = try signedFloatP <|> signedIntP

charP :: Parser Literal
charP
  = lexeme $
      do 
         _ <- char '\''
         c <- latin1Char
         _ <- char '\''
         return $ LChar c

boolP :: Parser Literal
boolP
  = lexeme $
      do x <- symbol "True" <|> symbol "False"
         return $ if x == "True" then LBool True else LBool False

rws :: [String]
rws = ["in"]

nameP :: Parser Identifier
nameP = I . T.pack <$> (p >>= check)

  where p = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "Identifier")
        check x
          = if x `elem` rws then
              fail $ "keyword " ++ show x ++ " cannot be an identifier" else
              return x

symbol :: Text -> Parser Text
symbol = L.symbol scn

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

cmtStringP :: Parser Cmt
cmtStringP
  = Cmt . T.pack <$>
      lexeme (many (alphaNumChar <|> spaceChar) <?> "Comment String")

cmtP :: Parser a -> Parser (a, Cmt)
cmtP p
  = do brackets $
         do x <- lexeme p
            _ <- symbol "|"
            c <- lexeme cmtStringP
            return (x, c)

progP :: Parser Prog
progP
  = do (c, i) <- moduleP
       --ds <- many declP
       bs <- many (tlbndrsP <?> "Top level Binder")
       eof
       return $ Prog c i (CSTS bs)


moduleP :: Parser (Cmt, Identifier)
moduleP
  = do 
       _ <- symbol "module"
       (n, c) <- cmtP nameP
       _ <- symbol "where"
       return (c, n)

tlbndrsP :: Parser CST
tlbndrsP
  = L.nonIndented scn $
      do b <- binderP
         space
         return $ TLBindr b

bodyP :: Parser Body
bodyP
  = do e <- exprP <?> "body"
       return $ Body e




