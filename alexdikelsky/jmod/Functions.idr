module Functions

import Types
import BinaryOperations

mapUnless : (a -> Either b String) -> List a -> Either (List b) String
mapUnless f [] = Left []
mapUnless f (x :: xs) =
  case (f x, mapUnless f xs) of
       (Left a, Left b) => Left $ a :: b
       (Right a, _) => Right $ show a
       (_, Right a) => Right $ show a

all2d : (a -> Either b String) -> List (List a) -> Either (List (List b)) String
all2d f l = mapUnless (\x => mapUnless f x) l


nonToExpr : Either NonRec String -> Either Expr String
nonToExpr (Left x) = Left $ Array0 x
nonToExpr (Right x) = Right x


applyVec : (NonRec -> NonRec -> Either NonRec String) -> List NonRec -> List NonRec -> Either (List NonRec) String
applyVec _ Nil Nil = Left Nil
applyVec f (x :: xs) (y :: ys) = 
  case (f x y, applyVec f xs ys) of
       (Left a, Left b) => Left $ a :: b
       _ => Right "Failed to add Vec"
applyVec _ _ _ = Right "Length Error"

applyMatToMat : (NonRec -> NonRec -> Either NonRec String) -> List (List NonRec) -> List (List NonRec) -> Either (List (List NonRec)) String
applyMatToMat _ Nil Nil = Left Nil
applyMatToMat f (x :: xs) (y :: ys) = 
  case (applyVec f x y, applyMatToMat f xs ys) of
       (Left a, Left b) => Left $ a :: b
       _ => Right "Yikes ...."
applyMatToMat _ _ _ = Right "oof"


applyVecToMat : (NonRec -> NonRec -> Either NonRec String) -> List NonRec -> List (List NonRec) -> Either (List (List NonRec)) String
applyVecToMat f vec mat = mapUnless (applyVec f vec) mat

applyF : (NonRec -> NonRec -> Either NonRec String) -> 
         (Either Expr String) 
         -> Expr 
         -> Either Expr String
applyF f i (Array0 n) = Left $ Array0 n
applyF f i (Array1 n) = Left $ Array1 n
applyF f i (Array2 n) = Left $ Array2 n
applyF f i (ConsList Nil) = i
applyF f i (ConsList (x :: Nil)) = Left x -- check
applyF f i (ConsList (x :: xs)) = 
  case (x, applyF f i (ConsList xs)) of
     (Array0 n, Left (Array0 k)) => nonToExpr $ f n k
     (Array0 n, Left (Array1 k)) => 
         case mapUnless (f n) k of
              Left l => Left $ Array1 l
              Right r => Right r
     (Array0 n, Left (Array2 twod)) =>
         case all2d (f n) twod of
              Left s =>  Left (Array2 s)
              Right k => Right $ "Not" ++ show k

     (Array2 n, Left (Array2 m)) =>
         case applyMatToMat f n m of
              Left s =>  Left (Array2 s)
              Right k => Right $ "Not" ++ show k

     (Array1 n, Left (Array1 k)) => 
            case applyVec f n k of
                 Left e => Left $ Array1 e
                 Right f => Right f
     (Array1 n, Left (Array2 k)) => 
            case applyVecToMat f n k of
                 Left e => Left $ Array2 e
                 Right f => Right f


     (Array1 k, Left (Array0 n)) => applyF f i (ConsList ((Array0 n) :: ((Array1 k) :: Nil)))
     (Array2 k, Left (Array0 n)) => applyF f i (ConsList ((Array0 n) :: ((Array2 k) :: Nil)))
     (Array2 k, Left (Array1 n)) => applyF f i (ConsList ((Array1 n) :: ((Array2 k) :: Nil)))
     (a, b) => Right $ "First " ++ (show a) ++ " second " ++ (show b)

applyF _ e _ = Right $ "Not implemented " ++ show e


public export
add : Expr -> Either Expr String
add = applyF addNumbers $ Left $ Array0 $ Natural 0

public export
mult : Expr -> Either Expr String
mult = applyF multNumbers $ Left $ Array0 $ Natural 1

public export
div : Expr -> Either Expr String
div = applyF divNumbers $ Left $ Array0 $ Natural 1

