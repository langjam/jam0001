{-# LANGUAGE OverloadedStrings #-}
module AST where

import Data.Text (Text)


newtype Identifier = I Text
                       deriving (Show,Ord,Eq)

data Prog = Prog Cmt Identifier CST
              deriving (Show)

data CST = Declaration Cmt Decl
         | TLBindr Binder
         | CSTS [CST]
             deriving (Show)

data Binder = Binder Cmt Identifier [Identifier] Body
                deriving (Show)

data Decl = Decl
              deriving (Show)


newtype Cmt = Cmt Text
                deriving (Show)

newtype Body = Body Expr
                 deriving (Show)

data Literal = LInt Integer
             | LFloat Double
             | LBool Bool
             | LChar Char
             | LList [Literal]
             | LPair (Literal, Literal)
                 deriving (Show)

data Op = Mul
        | Div
        | Add
        | Sub
        | Equ
            deriving Show

data Expr = Lit Cmt Literal
          | Neg Expr
          | BinOp Op Expr Expr
          | Var Cmt Identifier
          | Ap Expr [Expr]
          | Lam Cmt [Identifier] Body
          | Bndr Binder
          | Let [Binder] Body
          | If Expr Expr Expr
          | Fix Expr
              deriving (Show)


