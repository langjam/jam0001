module Syntax where

import Text.PrettyPrint.GenericPretty

type Ident = String

data Expr
  = NumLit Integer
  | BoolLit Bool
  | StrLit String
  | UnitLit
  | ListLit [Expr]
  | RecLit [(Ident, Expr)]
  | RecMember Expr Ident
  | Index Expr Expr
  | Var Ident
  | Let [(Ident, Expr)] Expr
  | Lam Ident Expr
  | App Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Assign Expr Expr
  | If Expr Expr Expr
  | Seq Expr Expr
  deriving stock Generic
  deriving anyclass Out

showExpr :: Expr -> String
showExpr = pretty

printExpr :: Expr -> IO ()
printExpr = pp
