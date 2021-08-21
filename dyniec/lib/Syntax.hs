module Syntax where

import qualified Data.Text as T

data Expr
    = ExPrimop T.Text
    | ExString T.Text
    | ExNum Integer
    | ExAbst T.Text Expr
    | ExApp Expr Expr
    | ExAnnot Expr Expr
    | ExLet T.Text Expr Expr
    | ExVar T.Text
    | ExUnit
    | ExTyAnnot Expr Type
    deriving (Show, Eq)
data Type
    = TyStr
    | TyNum
    | TyArrow Type Type
    | TyUnit
    deriving (Show, Eq)
newtype Module = Module
    { declarations :: [(T.Text, Expr)]
    }
    deriving (Show, Eq)
