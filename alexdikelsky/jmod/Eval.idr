module Eval

import Types
import Functions

import Data.Nat
import Data.List



symValue : String -> Context -> Either Expr String
symValue s ctx = 
  case lookup s ctx of
       Just v => Left v
       Nothing => Right $ "Couldn't find " ++ s

mutual
  exprListValue : List Expr -> Context -> Either (List Expr) String
  exprListValue Nil ctx = Left Nil
  exprListValue (x :: xs) ctx =
    case (exprValue x ctx, exprListValue xs ctx) of
         (Left e, Left r) => Left $ e :: r
         _ => Right "Sorry."
  
  exprValue : Expr -> Context -> Either Expr String
  exprValue (Array0 k) ctx = Left $ Array0 k
  exprValue (Array1 k) ctx = Left $ Array1 k
  exprValue (Array2 k) ctx = Left $ Array2 k

  exprValue (Symbol x) ctx = symValue x ctx
  exprValue (Quoted x) ctx = Left x
  exprValue (Function f) ctx = Left $ Function f
  
  exprValue (ConsList Nil) ctx = Right "Called ()"
  exprValue (ConsList (x::xs)) ctx =
    case (exprValue x ctx, exprListValue xs ctx) of
         (Left (Function f), Left args) => f (ConsList args)
         _ => Right "Tried to call a non-function"


public export
jValue : Expr -> Either Expr String
jValue k =
  exprValue k [
    ("+", Function add),
    ("*", Function mult),
    ("%", Function div)]
