module Functions

import Types
import Data.Nat

mapUnless : (a -> Either a String) -> List a -> Either (List a) String
mapUnless f [] = Left []
mapUnless f (x :: xs) =
  case (f x, mapUnless f xs) of
       (Left a, Left b) => Left $ a :: b
       (Right a, _) => Right $ show a
       (_, Right a) => Right $ show a

nonToExpr : Either NonRec String -> Either Expr String
nonToExpr (Left x) = Left $ Array0 x
nonToExpr (Right x) = Right x

addNumbers : NonRec -> NonRec -> Either NonRec String
addNumbers (Natural n) (Natural k) = Left $ Natural $ n + k
addNumbers (Natural n) (Finite k m1) = Left $ Finite ((n + k) `mod` m1) m1
addNumbers (Finite n m1) (Natural k) = addNumbers (Finite n m1) (Natural k)
addNumbers (Finite n m1) (Finite k m2)= 
  if m2 == m1 then Left $ Finite ((n + k) `mod` m1) m1
              else Right $ "Added numbers with modulus " ++ (show m1) ++ " and " ++ (show m2)


public export
add : Expr -> Either Expr String
add (Array0 n) = Left $ Array0 n
add (Array1 n) = Left $ Array1 n
add (Array2 n) = Left $ Array2 n

add (ConsList Nil) = Left $ Array0 $ Natural 0
add (ConsList (x :: Nil)) = Left x
add (ConsList (x :: xs)) = 
  case (x, add (ConsList xs)) of
     (Array0 n, Left (Array0 k)) => nonToExpr $ addNumbers n k
     (Array0 n, Left (Array1 k)) => 
         case mapUnless (addNumbers n) k of
              Left l => Left $ Array1 l
              Right r => Right r

     e => Right $ "Not done" ++ (show e)


add e = Right $ "Unable to add " ++ (show e)
