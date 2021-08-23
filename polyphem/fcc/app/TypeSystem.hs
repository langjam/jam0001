{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module TypeSystem where

import qualified Data.Text as T
import Data.List (nub)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Set as Set

import AST
import Pretty



-- Type System -------------------------------------------------------------------
extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env


newtype Unique = Unique { count :: Int }

type Infer a = ExceptT TypeError (State Unique) a

type Subst = Map.Map TVar Type


runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res


closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV $ T.pack (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Op -> Type
--ops Add = typeInt `TArr` typeInt `TArr` typeInt
ops Add = ((TVar . TV $ "a") `TArr` (TVar . TV $ "a")) `TArr` (TVar . TV $ "a")
ops Mul = typeInt `TArr` typeInt `TArr` typeInt
ops Sub = typeInt `TArr` typeInt `TArr` typeInt
ops Div = ((TVar . TV $ "a") `TArr` (TVar . TV $ "a")) `TArr` (TVar . TV $ "b")
ops Equ = typeInt `TArr` typeInt `TArr` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)


inferParamList :: TypeEnv -> [Var] -> Expr -> Infer (Subst,Type)
inferParamList env ps ex  = do
  tvs <- mapM (const fresh) ps
  let psts = zip ps tvs
      ext lenv (p,tv) = return $ lenv `extend` ( p , Forall [] tv)
  env' <- foldM ext env psts
  (s1,t1) <- infer env' ex
  return (s1, foldr (TArr) t1 $ map (apply s1) tvs)


infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var _ x -> lookupEnv env x

  Neg e -> infer env e

  Lam _ ps (Body e) -> inferParamList env ps e 

  Bndr (Binder _ _ ps (Body e)) -> inferParamList env ps e 

{-
  Lam _ ps e -> forM ps $ \x -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)
  Ap e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    psts <- mapM (infer (apply s1 env)) e2
    let (s2,t2) = foldr1 (\(a,b)(c,d) -> (compose a c, TArr b d)) psts
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

-}
  Ap e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    --psts <- mapM (infer env) e2
    psts <- mapM (infer (apply s1 env)) e2
    let (s2,t2) = foldr (\(a,b)(c,d) -> (compose c a, TArr b d)) (s1,tv) psts
    s3       <- unify (apply s2 t1) t2 
    --s3       <- unify (apply s2 t1) t2 
    return (s3 `compose` s2 `compose` s1, apply s3 tv)


  Let xs (Body e2) -> do
    xsts <- mapM  (\(Binder _ x ps (Body e1)) -> (x,) <$> inferParamList env ps e1) xs
    let genA (x,(s,t)) (lenv,s0) =    
          let lenv' = apply s lenv
              t'   = generalize lenv' t
              lenv''= lenv' `extend` (x,t')
          in (lenv'',s `compose` s0)
        (env',s') = foldr genA (env,nullSubst) xsts

    (s2, t2) <- infer env' e2
    return (s2 `compose` s', t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArr` tv) `TArr` tv)

  BinOp op e1 e2 -> do
  --  inferPrim env [e1, e2] (ops op)
      (s1,t1) <- infer env e1
      (s2,t2) <- infer env e2
      tv  <- fresh
      let u1 = t1 `TArr` t2 `TArr` tv
          u2 = ops op
      s3 <- unify u1 u2
      return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Lit _ (LInt _)  -> return (nullSubst, typeInt)
  Lit _ (LBool _) -> return (nullSubst, typeBool)
  Lit _ (LFloat _) -> return (nullSubst, typeFloat)
  Lit _ (LChar _) -> return (nullSubst, typeChar)
  Lit _ (LList _) -> undefined
  Lit _ (LPair _) -> undefined

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) lexp = do
    (ls, lt) <- infer (apply s env) lexp
    return (ls `compose` s, tf . (TArr lt))

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (I $ T.pack name, ty)) xs

inferAST :: TypeEnv -> Prog -> Either TypeError TypeEnv
inferAST env (Prog _ _ (CSTS cs)) = 
                let bndrs = filter (\b -> case b of (TLBindr _) -> True ; _ -> False) cs
                    nes = map (\(TLBindr b@(Binder _ (I n) _ _)) -> (T.unpack n,Bndr b)) bndrs
                in inferTop env nes
inferAST _ (Prog _ _ _) = undefined
  

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap (TV . T.pack) letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

