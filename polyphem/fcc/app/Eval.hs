{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Eval where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Text as T
import Text.Pretty.Simple
import AST

data VNumber = NInt Integer
             | NFloat Double


instance (Show) VNumber where
  show (NInt n) = show n
  show (NFloat n) = show n



data Value = VNum VNumber
           | VBool Bool
           | VChar Char
           | VClosure Cmt [Identifier] Expr TermEnv
  deriving (Show)

newtype TermEnv = TermEnv { te :: M.Map String Value }
  deriving Show

type Eval = StateT TermEnv IO


eval :: Expr -> Eval Value
eval expr = do
    --pPrint m
    val <- case expr of
              Lit (Cmt _) l -> pure $ case l of
                LInt i -> VNum $ NInt i
                LFloat i -> VNum $ NFloat i
                LBool b -> VBool b
                LChar c -> VChar c
                LList _ -> undefined
                LPair _ -> undefined
              Neg _ -> undefined 
              BinOp op e1 e2 -> do
                e1' <- eval e1
                e2' <- eval e2
                case e1' of
                  VNum (NInt a) -> case e2' of
                              VNum (NInt b) -> case op of
                                            Mul -> pure $ VNum $ NInt $ a * b
                                            Add -> pure $ VNum $ NInt $ a + b
                                            Sub -> pure $ VNum $ NInt $ a - b
                                            Div -> pure $ VNum $ NFloat $ fromIntegral a / fromIntegral b
                                            Equ -> pure $ VBool $ a == b
                  VNum (NFloat a) -> case e2' of
                              VNum (NFloat b) -> case op of
                                            Mul -> pure $ VNum $ NFloat $  a * b
                                            Add -> pure $ VNum $ NFloat $ a + b
                                            Sub -> pure $ VNum $ NFloat $ a - b
                                            Div -> pure $ VNum $ NFloat $ a / b
                                            Equ -> pure $ VBool $ a == b
              Var c@(Cmt t) (I v) -> do 
                m <- gets te
                let Just val = M.lookup (T.unpack v) m
                case val of
                  VClosure c' [] b clo -> do
                                            pPrint c'
                                            eval b

                  _ -> do 
                      unless (null $ T.unpack t) $ pPrint c
                      return val

              Ap e1 es -> do
                VClosure c ps b clo <- eval e1
                pPrint c
                nenv <- foldM (\(TermEnv lm) (I p,e) -> do
                                          val <- (eval e)
                                          return $ TermEnv $ M.insert (T.unpack p) val lm 
                        ) clo $ zip ps es
                withStateT (const nenv) $ eval b

              Lam c ps (Body e) -> do
                clo <- get
                return $ VClosure c ps e clo

              Bndr (Binder c (I n) ps (Body b)) -> do 
                e <- get 
                let v = VClosure c ps b e  
                put $ TermEnv $ M.insert (T.unpack n) v (te e) 
                return $ v

              Let bs (Body e) -> do
                mapM_ (eval . Bndr) bs
                eval e

              If eb et ef -> undefined
              Fix e -> undefined

    return $ val 

evalDef :: TermEnv -> (String,Expr) -> IO TermEnv
evalDef env (n,e) = do
  (_,env') <- runEval env (I $ T.pack n) e
  return env'

runEval :: TermEnv -> Identifier -> Expr -> IO (Value,TermEnv)
runEval env (I n) e = do 
  (res,env') <- runStateT (eval e) env
  return (res , TermEnv $ M.insert (T.unpack n) res (te env'))


evalAST :: Prog -> IO (Either String (Value,TermEnv))
evalAST (Prog (Cmt c) (I mn) (CSTS cs)) = do
              pPrint $ (T.unpack mn) ++ ": " ++ (T.unpack c)
              let bndrs = filter (\b -> case b of (TLBindr _) -> True ; _ -> False) cs
                  defs = map (\(TLBindr b@(Binder _ (I n) _ _)) -> (T.unpack n,Bndr b)) bndrs
              env <- do
                nenv <- foldM evalDef (TermEnv M.empty) defs   
                --pPrint nenv
                return nenv
              case lookup "main" defs of
                  Nothing -> return $ Left $ (T.unpack mn) ++ ": No 'main' entrypoint defined"
                  Just e -> Right <$> runEval env (I "main") (Ap e [])   
              
evalAST (Prog _ _ _) = undefined 
