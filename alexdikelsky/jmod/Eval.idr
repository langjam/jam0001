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
  exprValue (Natural k) ctx = Left $ Natural k
  exprValue (Finite k j) ctx = Left $ Finite (k `mod` j) j
  exprValue (Symbol x) ctx = symValue x ctx
  exprValue (Quoted x) ctx = Left x
  
  exprValue (Array Nil) ctx = Right "Called ()"
  exprValue (Array (x::xs)) ctx =
    case (exprValue x ctx, exprListValue xs ctx) of
         (Left (Function f), Left args) => f $ Left $ Array $ args
         _ => Right "Tried to call a non-function"

  exprValue (Function f) ctx = Left $ Function f

public export
jValue : Expr -> Either Expr String
jValue k =
  exprValue k [("+", Function add)]
