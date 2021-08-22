{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Eval where
import Syntax
import Data.Text as T

data Value = VInt Integer | VString T.Text | VAbstr T.Text Expr 
            | VComment Expr | VUnit |  VPrimop Primop
            deriving Show
data Primop = Ar1 (Value -> Value) | Ar2 (Value -> Value -> Value)
instance Show Primop
    where show p = "primop"
primopsList ::[(T.Text,Primop)]
primopsList = [("succ",Ar1 $ \case (VInt n) -> VInt (n+1) ),
                ("add", Ar2 $ \case (VInt n) -> (\case (VInt m) -> VInt (n+m)))]

lookupJust :: (Eq a) => a -> [(a, b)] -> b 
lookupJust a l = case lookup a l of
    Just b -> b
    Nothing -> error "variable not in scope"
type Env = [(T.Text,Value)]
eval :: Expr -> Env -> Value

eval (ExPrimop t) env = VPrimop $ lookupJust t primopsList
eval (ExString s) env= VString s
eval (ExNum n) env= VInt n
eval (ExAbst t e) env = VAbstr t e
eval (ExLet t e1 e2) env = eval (ExApp (ExAbst t e2) e1) env
eval (ExVar t ) env = lookupJust t env
eval ExUnit env =VUnit
eval (ExTyAnnot e ty) env = eval e env
eval (ExAnnot e e2) env = eval e env
eval (ExApp e1 e2) env = case eval e1 env of
    (VAbstr t e') ->  eval e' ((t,eval e2 env):env)
    (VPrimop (Ar1 l)) -> l (eval e2 env)
    (VPrimop (Ar2 l)) -> VPrimop$ Ar1 $ l (eval e2 env)
    _ -> error "Application of non-function"

evalModule :: Module -> [(T.Text,Value)]
evalModule (Module l) = go l []
    where 
        go [] r = r
        go ((name,expr):l) r = go l ((name,eval expr r):r)