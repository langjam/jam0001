module Eval

import Types
import Functions
import Data.Nat
import Data.List

import Debug.Trace

toStrings : Expr -> Either (List String) String
toStrings (ConsList syms) = 
  mapUnless 
    (\x => case x of
      Symbol s => Left s
      z => Right $ (show z) ++ " is not a symbol")
    syms
toStrings a = Right $ (show a) ++ "is not a list"


lookupName : String -> Expr -> Expr -> Either Expr String
lookupName sym symList (ConsList valueList) =
  case toStrings symList of
    Left symbols => case lookup sym (zip symbols valueList) of
       Just v => Left v
       Nothing => Right $ "Failed to find " ++ (show sym)
    Right s => Right s
lookupName s _ _ = Right $ "Incorrect args for lookup of " ++ (show s)

makeClosure : Expr -> Expr -> Expr -> Expr -> Expr
makeClosure bindings body syms values = 
  (ConsList [Symbol "__closure", bindings, body, syms, values])

mutual
  exprListValue : List Expr -> Expr -> Expr -> Either (List Expr) String
  exprListValue Nil _ _ = Left Nil
  exprListValue (x :: xs) syms values =
    case (exprValue x syms values, exprListValue xs syms values) of
         (Left e, Left r) => Left $ e :: r
         _ => Right $ "Unable to evaluate " ++ (show (x :: xs))
  
  exprValue : Expr -> Expr -> Expr -> Either Expr String
  exprValue (Array0 k) syms values = Left $ Array0 k
  exprValue (Array1 k) syms values = Left $ Array1 k
  exprValue (Array2 k) syms values = Left $ Array2 k

  exprValue (Symbol x) syms values = lookupName x syms values
  exprValue (Quoted x) syms values = Left x
  exprValue (Function f) syms values = Left $ Function f
  
  exprValue (ConsList [Symbol "λ", bindings, body]) syms values =
    Left $ makeClosure bindings body syms values

  exprValue (ConsList [Symbol "let", (ConsList bindings), body]) syms values =
     case (mapUnless (\x => case x of
                          ConsList [Symbol a, b] => Left (Symbol a, b)
                          _ => Right "Failed to read let") bindings) of
            (Left bounds) => let (names, vals) = unzip bounds in
                applyValue (makeClosure (ConsList names) body syms values) (ConsList vals) syms values
            (Right _) => Right "Panic"

  exprValue (ConsList (x :: xs)) syms value =
    case (exprValue x syms value, exprListValue xs syms value) of
         (Left f, Left arglist) => applyValue f (ConsList arglist) syms value
         (Right s, _) => Right $ "Failed at exprvalue, op was " ++ (show s)
         _ => Right "Failed when applying"

  exprValue _ _ _ = Right "Failed to apply"

  applyValue : Expr -> Expr -> Expr -> Expr -> Either Expr String
  applyValue (ConsList [Symbol "__closure", (ConsList symList), body, ConsList oldSyms, ConsList oldValues]) (ConsList arglist) _ _ =
    exprValue body (ConsList (symList ++ oldSyms)) (ConsList (arglist ++ oldValues))
  applyValue (Function f) (ConsList arglist) syms values =
    case exprListValue arglist syms values of
         Left args => f (ConsList args)
         Right s => Right $ "Unable to apply function to " ++ (show arglist) ++ " got " ++ (show s)
  applyValue _ _ _ _ = Right "Failed to apply"

  

public export
jValue : Expr -> Either Expr String
jValue k =
  exprValue k (ConsList (map Symbol   ["+", "*", "%", "i.",  "/", "transpose", "=", "zip", "is-zero", "lift", "car", "cdr", "not"]))
              (ConsList (map Function [add, mult, div, iota, foldUse, trans, eq, zipA, isZero, lift, car, cdr,  notN]))
