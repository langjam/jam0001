module Eval where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import AST


data Value = VInt Integer
           | VFloat Double
           | VBool Bool
           | VChar Char
           | VClosure [Identifier] Expr TermEnv
  deriving (Show)

newtype TermEnv = TermEnv { te :: M.Map String Value }
  deriving Show

type Eval = ReaderT TermEnv IO


eval :: Expr -> Eval Value
eval expr = do
    env <- ask
    val <- case expr of
              Lit (Cmt c) l -> pure $ case l of
                LInt i -> VInt i
                LFloat f -> VFloat f
                LBool b -> VBool b
                LChar c -> VChar c
                LList _ -> undefined
                LPair _ -> undefined
              Neg e -> undefined 
              BinOp op e1 e2 -> undefined
              Var (Cmt c) v -> undefined 
              Ap e1 es -> do
                VClosure ps b clo <- eval e1
                nenv <- foldM (\(TermEnv m) (I p,e) -> do
                                          val <- (eval e)
                                          return $ TermEnv $ M.insert (T.unpack p) val m
                        ) clo $ zip ps es
                local (const nenv) $ eval b

              Lam (Cmt c) ps (Body e) -> undefined
              Bndr (Binder (Cmt c) (I n) ps (Body e)) -> return $ VClosure ps e env
              Let bs (Body e) -> undefined
              If eb et ef -> undefined
              Fix e -> undefined

    return $ val 

evalAST :: Prog -> IO (Either String Value)
evalAST (Prog (Cmt c) (I mn) (CSTS cs)) = do
              putStrLn $ (T.unpack mn) ++ ": " ++ (T.unpack c)
              let bndrs = filter (\b -> case b of (TLBindr _) -> True ; _ -> False) cs
                  m = M.fromList $ map (\(TLBindr b@(Binder _ (I n) _ _)) -> (T.unpack n,Bndr b)) bndrs
              case M.lookup "main" m of
                    Nothing -> return $ Left $ (T.unpack mn) ++ ": No 'main' entrypoint defined"
                    Just e -> Right <$> runReaderT (eval (Ap e [])) (TermEnv m)  
              
evalAST (Prog _ _ _) = undefined 
