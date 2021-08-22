{
{-# LANGUAGE ConstraintKinds, LambdaCase, OverloadedStrings, PolyKinds, RankNTypes #-}

module Parser (parseM, parse, parseFile, parseExpr) where

import Prelude hiding (readFile)

import AlexPosn
import Core
import Lexer
import ParserMonad

import Control.Applicative (liftA2)
import Control.Monad.Trans (lift)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Monad.Trans.State as State
import Data.Either
import Data.Functor.Identity
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.IO (readFile)
}

%name parseM Env
%name parseExprM Expr
%tokentype { Locatable Token }
%monad { ParserMonad }
%lexer { readToken >>= } { Locatable EOF _ }
%error { parseError }

%token
  ';'           { Locatable Semi _ }
  comment       { Locatable (CommentTok $$) _ }
  '{!-'         { Locatable BeginExecComment _ }
  '-!}'         { Locatable EndExecComment _ }
  '#'           { Locatable Hash _ }
  ':'           { Locatable Colon _ }
  '='           { Locatable Equals _ }
  '\\'          { Locatable LambdaTok _ }
  '->'          { Locatable RightArrow _ }
  '$'           { Locatable Dollar _ }
  '+'           { Locatable Plus _ }
  '-'           { Locatable Minus _ }
  '*'           { Locatable Times _ }
  '/'           { Locatable Slash _ }
  '&'           { Locatable And _ }
  '|'           { Locatable Or _ }
  '^'           { Locatable Xor _ }
  '++'          { Locatable Concat _ }
  '=='          { Locatable Equality _ }
  '!='          { Locatable Inequality _ }
  '<'           { Locatable OpenAngle _ }
  '>'           { Locatable CloseAngle _ }
  '{'           { Locatable OpenCurly _ }
  '}'           { Locatable CloseCurly _ }
  '('           { Locatable OpenParen _ }
  ')'           { Locatable CloseParen _ }
  '['           { Locatable OpenSquare _ }
  ']'           { Locatable CloseSquare _ }
  int           { Locatable (Int $$) _ }
  ident         { Locatable (IdentTok $$) _ }
  eof           { Locatable EOF _ }

%left ':'
%left '++'
%left '+' '-'
%left '*' '/'
%left '&' '|' '^'
%left '==' '!='
%%

Env               :: { Locatable [Impredicative EvalMonad DeclM] }
Env               : List(Decl)                                            { $1 }

Decl              :: { Locatable (Impredicative EvalMonad DeclM) }
Decl              : Comment ident List(ident) '=' Expr semi               {% fmap (\env -> pure decl'' <*> $2 <*> $3 <*> liftA2 CommentTag $1 $5 <*> pure env) (State.gets stateEnv) }

Expr              :: { Locatable Expr }
Expr              : Expr1                                                 { $1 }
                  | Expr1 '$' Expr                                        { liftA2 App $1 $3 }

Expr1             :: { Locatable Expr }
Expr1             : Expr2                                                 { $1 }
                  | '\\' ident Comment '->' Expr                          { liftA2 Lambda $2 $ liftA2 CommentTag $3 $5 }
                  | Expr1 Expr2                                           { liftA2 App $1 $2 }

Expr2             :: { Locatable Expr }
Expr2             : '(' Expr ')'                                          { $2 }
                  | int                                                   { fmap (Constant . SimpleIntV) $1 }
                  | ident                                                 { fmap Variable $1 }
                  | Comment                                               { $1 }

Comment           :: { Locatable Expr }
Comment           : comment                                               { fmap Constant $1 }
                  | '{!-' Expr '-!}'                                      { fmap LiftToComment $2 }
                  | Comment '#' Comment                                   { liftA2 ConcatComment $1 $3 }

List(p)           : {- empty -}                                           { pure [] }
                  | List(p) p                                             { liftA2 (:) $2 $1 }

Maybe(p)          : {- empty -}                                           { Nothing }
                  | p                                                     { Just $1 }

semi              :: { Locatable Token }
semi              : ';'                                                   { $1 }
                  | semi ';'                                              { $1 <* $2 }

{
decl' :: EvalMonad m => Ident -> [Ident] -> Expr -> Impredicative EvalMonad EnvM' -> DeclM m
decl' i args e (Impredicative (EnvM' env)) = decl i args e env

decl'' :: Ident -> [Ident] -> Expr -> Impredicative EvalMonad EnvM' -> Impredicative EvalMonad DeclM
decl'' i args e env = Impredicative (decl' i args e env)

parseError :: Locatable Token -> ParserMonad a
parseError = flip throwLocalError "Parse Error"

{-
-- | Parse the given string and return either an error together with a range or an `Env'
parse' :: EvalMonad m => Text -> Either (Text, Maybe (AlexPosn, AlexPosn)) (EnvM m)
parse' s = result
  where
    result :: EvalMonad m => Either (Text, Maybe (AlexPosn, AlexPosn)) (EnvM m)
    result = fmap (map runImpredicative . locatableValue) $ runParser' parseM s env

    env :: EvalMonad m => EnvM m
    env = builtins ++ fromRight [] result
-}

-- | Parse the given string and return either a nicely formatted error or an `Env'
parse :: Text -> Either Text (Impredicative EvalMonad EnvM')
parse s = result
  where
    result :: Either Text (Impredicative EvalMonad EnvM')
    result = fmap (sequenceImpredicative EnvM' . locatableValue) $ runParser parseM s env

    env :: Impredicative EvalMonad EnvM'
    env = mapImpredicative (mapEnvM' (builtins ++)) $ fromRight (Impredicative $ EnvM' []) result

-- | Parse the given file and return either a nicely formatted error or an `Env'
parseFile :: FilePath -> IO (Either Text (Impredicative EvalMonad EnvM'))
parseFile = fmap parse . readFile

-- | Parse the given string and return either a nicely formatted error or an `Expr'
parseExpr :: Text -> Impredicative EvalMonad EnvM' -> Either Text Expr
parseExpr s env = locatableValue <$> runParser parseExprM s env
}
