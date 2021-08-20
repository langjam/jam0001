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
    | ExUnit
    | ExTyAnnot Expr Type
data Type
    = TyStr
    | TyNum
    | TyArrow Type Type
    | TyUnit
data Module = Module
    { exports :: [T.Text]
    , declarations :: [(T.Text, Expr)]
    }
