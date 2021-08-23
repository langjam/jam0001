{-#LANGUAGE FlexibleInstances, FunctionalDependencies, LambdaCase, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards #-}

module Core where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

newtype Ident = Ident Text
  deriving (Eq, Show)

instance IsString Ident where
  fromString = Ident . Text.pack

data SimpleValue
  = SimpleBoolV Bool
  | SimpleIntV Int
  | SimpleStringV String
  | SimpleCommentV Text
  deriving Eq

instance Show SimpleValue where
  show (SimpleBoolV x)    = show x
  show (SimpleIntV x)     = show x
  show (SimpleStringV x)  = show x
  show (SimpleCommentV x) = Text.unpack x

data ValueM m
  = SimpleValue SimpleValue
  | Error 
    { traceback :: [ValueM m]
    , errorMessage :: Text
    }
  | Closure (m (ValueM m) -> m (ValueM m))

pattern BoolV x    = SimpleValue (SimpleBoolV x)
pattern IntV x     = SimpleValue (SimpleIntV x)
pattern StringV x  = SimpleValue (SimpleStringV x)
pattern CommentV x = SimpleValue (SimpleCommentV x)

instance Eq (ValueM m) where
  SimpleValue x == SimpleValue y = x == y
  _ == _ = False

instance Show (ValueM m) where
  show (SimpleValue x) = show x
  show (Error {..})    = "Error: " ++ Text.unpack errorMessage ++ "\nat:\n" ++ (unlines . reverse . map show $ traceback)
  show (Closure _)     = "Closure"

data Expr
  = Constant SimpleValue
  | Variable Ident
  | Lambda Ident Expr
  | App Expr Expr
  | CommentTag Expr Expr
  | LiftToComment Expr
  | ConcatComment Expr Expr
  deriving Show

data DeclM m = Decl
  { declIdent :: Ident
  , declValue :: m (ValueM m)
  }

type EnvM m = [DeclM m]

class Monad m => EvalMonad m where
  evalCommentTag :: Expr -> m (ValueM m) -> EnvM m -> m (ValueM m)
  ignore :: m a -> a

data Release a = Release { runRelease :: a }

instance Functor Release where
  fmap f (Release x) = Release $ f x

instance Applicative Release where
  pure = Release
  (Release f) <*> (Release x) = Release $ f x

instance Monad Release where
  (Release x) >>= f = f x

instance EvalMonad Release where
  evalCommentTag _ x _ = x
  ignore = runRelease

data ErrorTraceback a = ErrorTraceback { runErrorTraceback :: a }

instance Functor ErrorTraceback where
  fmap f (ErrorTraceback x) = ErrorTraceback $ f x

instance Applicative ErrorTraceback where
  pure = ErrorTraceback
  (ErrorTraceback f) <*> (ErrorTraceback x) = ErrorTraceback $ f x

instance Monad ErrorTraceback where
  (ErrorTraceback x) >>= f = f x

instance EvalMonad ErrorTraceback where
  evalCommentTag comment x env = x >>= \case
    Error {..} -> pure $ Error {traceback = runErrorTraceback (eval comment env) : traceback, ..}
    x' -> pure x'
  ignore = runErrorTraceback

data Tracing a = Tracing { runTracing :: State (Int, [Text]) a }

instance Functor Tracing where
  fmap f (Tracing x) = Tracing $ fmap f x

instance Applicative Tracing where
  pure = Tracing . pure
  (Tracing f) <*> (Tracing x) = Tracing $ f <*> x

instance Monad Tracing where
  (Tracing x) >>= f = Tracing $ x >>= (runTracing . f)

instance EvalMonad Tracing where
  evalCommentTag comment x env = do
    indent <- Tracing $ fst <$> get
    Tracing $ modify $ (1 +) *** ((Text.replicate indent "| " <> "Begin: " <> tshow comment) :)
    x' <- x
    Tracing $ modify $ subtract 1 *** ((Text.replicate indent "| " <> "End: "   <> tshow comment) :)
    pure x'
  ignore m = fst $ runState (runTracing m) (0, [])

eval :: EvalMonad m => Expr -> EnvM m -> m (ValueM m)
eval (Constant x) _   = pure $ SimpleValue x
eval (Variable i) env =
  case filter ((==) i . declIdent) env of
    [Decl {..}] -> declValue
    [] -> pure $ Error [] $ "Could not find variable " <> tshow i
    (_:_:_)  -> pure $ Error [] $ "Multiple definitions of variable " <> tshow i <> ". Have you been shadowing again?"
eval (Lambda declIdent e) env = pure $ Closure $ \declValue -> eval e $ Decl {..} : env
eval (App f arg) env =
  eval f env >>= \case
    e@(Error {}) -> pure e
    Closure f' -> f' $ eval arg env
    _ -> pure $ Error [] "Left side of function application must be a function"
eval (CommentTag comment e) env = evalCommentTag comment (eval e env) env
eval (LiftToComment e) env =
  eval e env >>= \case
    e'@(Error {}) -> pure e'
    x -> pure $ CommentV $ tshow x
eval (ConcatComment e1 e2) env =
  liftA2 (,) (eval e1 env) (eval e2 env) >>= \case
    (e@(Error {}), _) -> pure e
    (_, e@(Error {})) -> pure e
    (CommentV x, CommentV y) -> pure $ CommentV $ x <> y
    (x, y) -> pure $ Error [] $ "Cannot concatentate non-comments: " <> tshow x <> " and " <> tshow y

tracingEval' :: Int -> Expr -> EnvM (State [Text]) -> State [Text] (ValueM (State [Text]))
tracingEval' indent (Constant x) _   = pure $ SimpleValue x
tracingEval' indent (Variable i) env =
  case filter ((==) i . declIdent) env of
    [Decl {..}] -> declValue
    [] -> pure $ Error [] $ "Could not find variable " <> tshow i
    (_:_:_)  -> pure $ Error [] $ "Multiple definitions of variable " <> tshow i <> ". Have you been shadowing again?"
tracingEval' indent (Lambda declIdent e) env = pure $ Closure $ \declValue -> tracingEval' indent e $ Decl {..} : env
tracingEval' indent (App f arg) env =
  tracingEval' indent f env >>= \case
    e@(Error {}) -> pure e
    Closure f' -> f' $ tracingEval' indent arg env
    _ -> pure $ Error [] "Left side of function application must be a function"
tracingEval' indent (CommentTag comment e) env = do
  modify ((Text.replicate indent "| " <> "Begin: " <> tshow comment) :)
  x <- tracingEval' indent e env
  modify ((Text.replicate indent "| " <> "End: "   <> tshow comment) :)
  pure x
tracingEval' indent (LiftToComment e) env =
  tracingEval' indent e env >>= \case
    e'@(Error {}) -> pure e'
    x -> pure $ CommentV $ tshow x
tracingEval' indent (ConcatComment e1 e2) env =
  liftA2 (,) (tracingEval' indent e1 env) (tracingEval' indent e2 env) >>= \case
    (e@(Error {}), _) -> pure e
    (_, e@(Error {})) -> pure e
    (CommentV x, CommentV y) -> pure $ CommentV $ x <> y
    (x, y) -> pure $ Error [] $ "Cannot concatentate non-comments: " <> tshow x <> " and " <> tshow y

tracingEval :: Expr -> EnvM (State [Text]) -> State [Text] (ValueM (State [Text]))
tracingEval = tracingEval' 0

decl :: EvalMonad m => Ident -> [Ident] -> Expr -> EnvM m -> DeclM m
decl i = (((Decl i .) . eval) .) . lambda
  where
    lambda :: [Ident] -> Expr -> Expr
    lambda [] e = e
    lambda (arg:args) e = Lambda arg $ lambda args e

class ToValueStrict m a | a -> m where
  toValueStrict :: a -> ValueM m

instance ToValueStrict m (ValueM m) where
  toValueStrict = id

instance (Applicative m, ToValueStrict m a) => ToValueStrict m (ValueM m -> a) where
  toValueStrict f = Closure $ fmap $ toValueStrict . f

builtins :: Monad m => EnvM m
builtins =
  [ Decl "true"   $ pure $ BoolV True
  , Decl "false"  $ pure $ BoolV False
  , Decl "eq"     $ pure $ toValueStrict eq
  , Decl "add"    $ pure $ toValueStrict add
  , Decl "sub"    $ pure $ toValueStrict sub
  , Decl "mul"    $ pure $ toValueStrict mul
  , Decl "abs"    $ pure $ toValueStrict abs'
  , Decl "signum" $ pure $ toValueStrict signum'
  , Decl "negate" $ pure $ toValueStrict negate'
  , Decl "ifElse" $ pure $ ifElse'
  ]
  where
    eq :: ValueM m -> ValueM m -> ValueM m
    eq e@(Error {}) _ = e
    eq _ e@(Error {}) = e
    eq x y = BoolV $ x == y

    ifElse :: Monad m => m (ValueM m) -> m (ValueM m) -> m (ValueM m) -> m (ValueM m)
    ifElse c t f = c >>= \case
      (BoolV True)  -> t
      (BoolV False) -> f
      (Error {})    -> c
      c             -> pure $ Error [] $ "Invalid conditional: " <> tshow c

    ifElse' :: Monad m => ValueM m
    ifElse' = Closure $ \c -> pure $ Closure $ pure . Closure . ifElse c

    add :: ValueM m -> ValueM m -> ValueM m
    add (IntV x) (IntV y) = IntV $ x + y
    add e@(Error {}) _ = e
    add _ e@(Error {}) = e
    add x y = Error [] $ "Invalid arguments to add: " <> tshow x <> ", " <> tshow y

    sub :: ValueM m -> ValueM m -> ValueM m
    sub (IntV x) (IntV y) = IntV $ x - y
    sub e@(Error {}) _ = e
    sub _ e@(Error {}) = e
    sub x y = Error [] $ "Invalid arguments to sub: " <> tshow x <> ", " <> tshow y

    mul :: ValueM m -> ValueM m -> ValueM m
    mul (IntV x) (IntV y) = IntV $ x * y
    mul e@(Error {}) _ = e
    mul _ e@(Error {}) = e
    mul x y = Error [] $ "Invalid arguments to mul: " <> tshow x <> ", " <> tshow y

    abs' :: ValueM m -> ValueM m
    abs' (IntV x) = IntV $ abs x
    abs' e@(Error {}) = e
    abs' x = Error [] $ "Invalid argument to abs: " <> tshow x

    signum' :: ValueM m -> ValueM m
    signum' (IntV x) = IntV $ signum x
    signum' e@(Error {}) = e
    signum' x = Error [] $ "Invalid argument to signum: " <> tshow x

    negate' :: ValueM m -> ValueM m
    negate' (IntV x) = IntV $ negate x
    negate' e@(Error {}) = e
    negate' x = Error [] $ "Invalid argument to negate: " <> tshow x

