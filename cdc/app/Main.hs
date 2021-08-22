module Main where

import System.Environment
import GHC.Float.RealFracMethods
import Text.Parsec.String
import Data.Maybe
import Text.Parsec
import Graphics.Gloss

import Core
import Lexer
import Parser

---- Eval ----
eval :: [Decl] -> Expr -> Expr
eval _ (Commt s) = Commt s
eval _ (Lit x) = Lit x
eval ds (Arith (Add a b)) = let Lit (I i1) = eval ds a in
                            let Lit (I i2) = eval ds b in
                            Lit (I (i1 + i2))
eval ds (Arith (Sub a b)) = let Lit (I i1) = eval ds a in
                            let Lit (I i2) = eval ds b in
                            Lit (I (i1 - i2))
eval ds (Arith (Mult a b)) = let Lit (I i1) = eval ds a in
                             let Lit (I i2) = eval ds b in
                             Lit (I (i1 * i2))


eval ds (Var x) = case lookup x $ map dToPair ds of
                    Just d -> eval ds d
                    Nothing -> Commt $ "Variable " ++ x ++ " does not exist."
eval _ (Lambda x e) = Lambda x e

eval ds (App (Var "comment") e2) = Commt $ show $ eval ds e2
    
-- case parse (contents pexp) "append" "with _x do when _" of
--     Left x -> error $ show x
--     Right e -> e

eval ds (App e1 e2) = do
    let Lambda n e1' = eval ds e1
    case eval ds e1 of
        Lambda n e1' ->
            let arg = eval ds e2 in
            eval ds $ substitute n arg e1'
        Commt s ->
            let Commt s' = eval ds e2 in
            Commt (s ++ "\n---\n" ++ s')

eval ds (List ls) = do
    List $ map (eval ds) ls
eval ds (CaseList e0 e1 b c e2) = do
    let List l = eval ds e0
    case l of
      [] -> eval ds e1
      x:xs -> eval (Decl b x:Decl c (List xs):ds) e2

eval ds (Prepend x y) = do
    let x' = eval ds x
    let List y' = eval ds y
    List (x':y')

eval _ x = error $ show x


substitute :: String -> Expr -> Expr -> Expr
substitute n arg (Var x) = if n == x then arg else Var x
substitute n arg (Lambda x e) = if x /= n then Lambda x $ substitute n arg e else Lambda x e
substitute n arg (App e1 e2) = App (substitute n arg e1) (substitute n arg e2)
substitute n arg (CaseList e0 e1 b c e2) = CaseList (substitute n arg e0) (substitute n arg e1) b c (if n /= b && n /= c then substitute n arg e2 else e2)
substitute n arg (Prepend a b) = Prepend (substitute n arg a) (substitute n arg b)
substitute n arg (Arith (Add a b)) = Arith $ Add (substitute n arg a) (substitute n arg b)
substitute n arg (Arith (Sub a b)) = Arith $ Sub (substitute n arg a) (substitute n arg b)
substitute n arg (Arith (Mult a b)) = Arith $ Mult (substitute n arg a) (substitute n arg b)
substitute n arg x = x
---- ---- ----


present :: [Decl] -> Expr -> IO ()
present ds (Presentation ss) = animate FullScreen white picture
    where
        picture ti =
            if floorFloatInt (ti / delay) < length ss
               then let Commt c = eval ds (ss !! floorFloatInt (ti / delay)) in
                  Translate (- int2Float (length c) * 15) 0 $ Scale 0.5 0.5 $ Text c
               else Translate (- int2Float (length "press escape to exit") * 15) 0 $ Scale 0.5 0.5 $ Text "press escape to exit"
        delay = 4


main = do
    putStrLn "Provide a filename:"
    filename <- getLine
    result <- parseFromFile (contents pmanydecl) filename
    case result of
      Left err    -> print err
      Right decls ->
        case lookup "main" $ map dToPair decls of
          Just e -> present decls e
          Nothing -> putStrLn "No main function defined"
