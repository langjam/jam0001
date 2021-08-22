module Eval where

import Control.Monad.Except (throwError, ExceptT (ExceptT), runExceptT, liftEither)
import Data.List (isPrefixOf)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Reader (MonadReader (ask, local), asks, ReaderT, runReaderT)
import Data.Functor (($>))
import Data.Bifunctor (first)

import Syntax
import Val
import Intrinsics
import Preprocess (Comment)

type Env = Map Ident Val
type EvalCtx = ReaderT Env (ExceptT EvalError IO)

type Args = [String]

runEval :: Env -> EvalCtx a -> IO (Either EvalError a)
runEval env m = runExceptT (runReaderT m env)

liftCtx :: IO (Either EvalError a) -> EvalCtx a
liftCtx m = liftEither =<< liftIO m

(!?) :: (Num i, Ord i) => [a] -> i -> Maybe a
(a : _) !? 0 = Just a
(_ : as) !? n | n > 0 = as !? (n - 1)
_ !? _ = Nothing

replaceAt :: (Num i, Ord i) => [a] -> i -> a -> Maybe [a]
replaceAt (_ : as) 0 v = Just $ v : as
replaceAt (a : as) i v | i > 0 = (a :) <$> replaceAt as (i - 1) v
replaceAt _ _ _ = Nothing

ref :: MonadIO m => a -> m (IORef a)
ref = liftIO . newIORef

unref :: MonadIO m => IORef a -> m a
unref = liftIO . readIORef

eval' :: Expr -> EvalCtx Val
eval' (NumLit n) = pure $ Num n
eval' (BoolLit b) = pure $ Bool b
eval' (StrLit s) = pure $ Str s
eval' UnitLit = pure Unit
eval' (ListLit es) = List <$> (ref =<< traverse eval' es)
eval' (RecLit fs) = Rec <$> (ref . M.fromList =<< traverse (traverse eval') fs)

eval' (RecMember r f) = do
  record <- eval' r
  case record of
    Rec m' -> do
      m <- unref m'
      case m M.!? f of
        Just e -> pure e
        Nothing -> throwError $ "Inexistent record field  " ++ show f
    _ -> throwError "Tried to access member of a non-record"

eval' (Index e idx) = do
  list <- eval' e
  i <- eval' idx

  case i of
    Num n ->
      case list of
        List vs' -> do
          vs <- unref vs'
          case vs !? n of
            Just v -> pure v
            Nothing -> throwError "List index out of range"
        _ -> throwError "Tried to index into a non-list"
    _ -> throwError "List index is not of type Num"

eval' (Var i) = do
  var <- asks (M.!? i)
  case var of
    Just v -> pure v
    Nothing -> throwError $ "Variable " ++ show i ++ " is not defined"

eval' (Let bs e) = mdo
  let
    isLam Lam{} = True
    isLam _ = False

    fs = filter (isLam . snd) bs
    vs = filter (not . isLam . snd) bs

  fs' <- local (M.union fs') $ traverse eval' $ M.fromList fs

  local (M.union fs') do
      vs' <- traverse eval' $ M.fromList vs
      local (M.union vs') $ eval' e

eval' (Lam i e) = do
  env <- ask
  pure $ Fn \v -> runEval (M.insert i v env) $ eval' e

eval' (App f a) = do
  f' <- eval' f
  a' <- eval' a

  case f' of
    Fn fn -> liftCtx $ fn a'
    _ -> throwError "Tried to apply a non-function"

eval' (And a b) = do
  a' <- eval' a

  if not $ truthy a'
  then pure a'
  else eval' b

eval' (Or a b) = do
  a' <- eval' a

  if truthy a'
  then pure a'
  else eval' b

eval' (Assign (Index e i) v) = do
  list <- eval' e
  i' <- eval' i
  v' <- eval' v

  case list of
    List l' ->
      case i' of
        Num n -> do
          l <- liftIO $ readIORef l'
          case replaceAt l n v' of
            Just newL -> liftIO (writeIORef l' newL) $> Unit
            Nothing -> throwError "Tried to assign to out-of-rangee index"
        _ -> throwError "Tried to assign to non-numeric index"
    _ -> throwError "Tried to assign an index in a non-list"

eval' (Assign (RecMember e f) v) = do
  r <- eval' e
  v' <- eval' v

  case r of
    Rec fs' -> do
      fs <- liftIO $ readIORef fs'
      liftIO (writeIORef fs' $ M.insert f v' fs) $> Unit
    _ -> throwError "Tried to assign a field in a non-record"

eval' (Assign _ _) = throwError "PANIC: Non-member expression in Assign"

eval' (If c t e) = do
  c' <- eval' c
  eval'
    if truthy c'
    then t
    else e

eval' (Seq a b) = eval' a *> eval' b

ofComm :: Val -> Comment
ofComm (Str s) = s
ofComm v = show v

showEvalError :: EvalError  -> String
showEvalError "Halt" = "Halt"
showEvalError ee
  | "User-thrown" `isPrefixOf` ee = ee
  | otherwise = "Runtime error: " ++ ee

evalWithComments :: Args -> [Comment] -> Expr -> ExceptT EvalError IO ([Comment], Val)
evalWithComments args comms expr = do
  commsVal@(List commsRef) <- pure $ toVal comms
  let argsVal = toVal args

  let vars = M.fromList [("args", argsVal), ("comments", commsVal)]

  v <- ExceptT $ first showEvalError <$> runEval (M.union vars intrinsics) (eval' expr)
  newComms :: [Comment] <- liftIO $ (ofComm <$>) <$> readIORef commsRef
  pure (newComms, v)
