module Core where

import Data.List

data Decl = Decl { dname :: String, dexp :: Expr }
    deriving (Show)


data Expr = Commt String
          | Presentation [Expr]

          | Lit Literal
          | Var String
          | Lambda String Expr
          | App Expr Expr

          | List [Expr]
          | CaseList Expr Expr String String Expr
          | Prepend Expr Expr
        
          | Arith ArithExp

data Literal = I Integer | C Char deriving (Show)

data ArithExp = Add Expr Expr | Mult Expr Expr | Sub Expr Expr

instance Show Expr where
    show (Commt s) = s
    show (Presentation s) = show s

    show (Lit (I i)) = show i
    show (Lit (C c)) = show c
    show (Var x) = x
    show (Lambda x e) = "with " ++ x ++ " do " ++ show e ++ " please"
    show (App x y) = show x ++ " " ++ show y

    show (List x) = "[" ++ intercalate ", " (map show x) ++ "]"
    show (CaseList a e1 b c e2) = "when " ++ show a ++ " is [] do " ++ show e1 ++ ". for " ++ b ++ ":" ++ c ++ " do " ++ show e2
    show (Prepend x y) = show x ++ ":" ++ show y

    show (Arith (Add a b)) = show a ++ " + " ++ show b
    show (Arith (Sub a b)) = show a ++ " - " ++ show b
    show (Arith (Mult a b)) = show a ++ " - " ++ show b


dToPair d = (dname d, dexp d)
