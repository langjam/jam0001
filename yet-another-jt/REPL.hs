{-# LANGUAGE LambdaCase, RankNTypes, OverloadedStrings #-}

module Main (main) where

import Core
import Parser
import ParserMonad

import Control.Monad.State
import qualified Data.Text.IO as Text
import System.Environment
import System.IO

evalRelease :: Impredicative EvalMonad EnvM' -> Expr -> IO ()
evalRelease (Impredicative (EnvM' env)) = print . runRelease . flip eval env

evalErrorTraceback :: Impredicative EvalMonad EnvM' -> Expr -> IO ()
evalErrorTraceback (Impredicative (EnvM' env)) = print . runErrorTraceback . flip eval env

evalTracing :: Impredicative EvalMonad EnvM' -> Expr -> IO ()
evalTracing (Impredicative (EnvM' env)) e = do
  let (x, (_, log)) = runState (runTracing $ eval e env) (0, [])
  _ <- traverse Text.putStrLn $ reverse log
  print x

repl :: (Impredicative EvalMonad EnvM' -> Expr -> IO ()) -> Impredicative EvalMonad EnvM' -> IO ()
repl eval' env = do
  putStr "> "
  hFlush stdout
  isEOF >>= \case
    True -> putStrLn "EOF"
    False -> Text.getLine >>= \case
      "\x04" -> putStrLn "EOT"
      s -> do
        either Text.putStrLn (eval' env) $ parseExpr s env
        repl eval' env

getMode :: String -> Impredicative EvalMonad EnvM' -> Expr -> IO ()
getMode "release" = evalRelease
getMode "errorTraceback" = evalErrorTraceback
getMode "tracing" = evalTracing

main :: IO ()
main = getArgs >>= (\(mode:filenames) -> ((fmap (sequenceImpredicative concatEnvM' . (Impredicative (EnvM' builtins) :)) . sequence) <$> traverse parseFile filenames) >>= either Text.putStrLn (repl $ getMode mode))

