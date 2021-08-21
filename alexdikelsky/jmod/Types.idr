module Types

import NumberTheory
import Data.Nat


public export
data NonRec =
  Natural Nat
| Finite Nat Nat

public export
data Expr =
  Array0 NonRec
| Array1 (List NonRec)
| Array2 (List (List NonRec))
| ConsList (List Expr)
| Function (Expr -> Either Expr String)
| Symbol String
| Quoted Expr

public export
Context : Type
Context = List (String, Expr)

public export
Show NonRec where
  show (Natural n) = show n
  show (Finite n k) = (show n) ++ "_" ++ (show k)

public export
Show Expr where
  show (Array0 z) = show z
  show (Array1 k) = "[" ++ (foldr (\x => \y => x ++ " " ++ y) "" (map show k)) ++ "]"
  show (Array2 k) = "[" ++ (foldr (\x => \y => x ++ " " ++ y) "" (map show k)) ++ "]"
  show (ConsList k) = "(" ++ (foldr (\x => \y => x ++ " " ++ y) "" (map show k)) ++ ")"
  show (Function _) = "Function"
  show (Symbol k) = show k
  show (Quoted n) = "'" ++ show n


-- addNumbers : NonRec -> NonRec -> Either NonRec String
-- addNumbers (Natural n) (Natural k) = Left $ Natural $ n + k
-- addNumbers (Natural n) (Finite k m1) = Left $ Finite ((n + k) `mod` m1) m1
-- addNumbers (Finite n m1) (Natural k) = addNumbers (Finite n m1) (Natural k)
-- addNumbers (Finite n m1) (Finite k m2) =
--   if m2 == m1 then Left $ Finite ((n + k) `mod` m1) m1
--               else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)
-- 
-- multNumbers : NonRec -> NonRec -> Either NonRec String
-- multNumbers (Natural n) (Natural k) = Left $ Natural $ n * k
-- multNumbers (Natural n) (Finite k m1) = Left $ Finite ((n * k) `mod` m1) m1
-- multNumbers (Finite n m1) (Natural k) = multNumbers (Finite n m1) (Natural k)
-- multNumbers (Finite n m1) (Finite k m2) =
--   if m2 == m1 then Left $ Finite ((n * k) `mod` m1) m1
--               else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)
-- 
-- divNumbers : NonRec -> NonRec -> Either NonRec String
-- divNumbers (Natural n) (Natural k) = Left $ Natural $ n `div` k
-- divNumbers (Natural n) (Finite k m1) = 
--   case modinverse (cast k) m1 of
--        Left o => Left $ Finite ((o * n) `mod` m1) m1
--        Right s => Right s
-- 
-- divNumbers (Finite n m1) (Natural k) = 
--   case modinverse (cast k) m1 of
--        Left o => Left $ Finite ((o * n) `mod` m1) m1
--        Right s => Right s
-- 
-- divNumbers (Finite n m1) (Finite k m2) =
--   if m2 == m1 then 
--               case modinverse (cast k) m1 of
--                 Left o => Left $ Finite ((o * n) `mod` m1) m1
--                 Right s => Right s
--               else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)
-- 
-- modNumbers : NonRec -> NonRec -> Either NonRec String
-- modNumbers (Natural a) (Natural b) = Left $ Natural $ a `mod` b
-- modNumbers a b = Right $ "Can't mod " ++ (show a) ++ " by " ++ (show b)


-- Num (Either NonRec String) where
--   (Left a) + (Left b) = addNumbers a b
--   a + b = Right $ "Unable to add " ++ (show a) ++ " and " ++ (show b)
-- 
--   (Left a) * (Left b) = multNumbers a b
--   a * b = Right $ "Unable to add " ++ (show a) ++ " and " ++ (show b)
-- 
--   fromInteger = \n => Left $ Natural (cast n)
-- 
-- Integral (Either NonRec String) where
--   (Left a) `mod` (Left b) = modNumbers a b
--   (a `mod` b) = Right $ "Unable to mod " ++ (show a) ++ " by " ++ (show b)
--   (Left a) `div` (Left b) = divNumbers a b
--   (a `div` b) = Right $ "Unable to divide " ++ (show a) ++ " and " ++ (show b)
