module BinaryOperations
import Types
import Data.Nat
import NumberTheory

public export
addNumbers : NonRec -> NonRec -> Either NonRec String
addNumbers (Natural n) (Natural k) = Left $ Natural $ n + k
addNumbers (Natural n) (Finite k m1) = Left $ Finite ((n + k) `mod` m1) m1
addNumbers (Finite n m1) (Natural k) = addNumbers (Finite n m1) (Natural k)
addNumbers (Finite n m1) (Finite k m2) =
  if m2 == m1 then Left $ Finite ((n + k) `mod` m1) m1
              else Right $ "Added numbers with modulus " ++ (show m1) ++ " and " ++ (show m2)

public export
multNumbers : NonRec -> NonRec -> Either NonRec String
multNumbers (Natural n) (Natural k) = Left $ Natural $ n * k
multNumbers (Natural n) (Finite k m1) = Left $ Finite ((n * k) `mod` m1) m1
multNumbers (Finite n m1) (Natural k) = multNumbers (Finite n m1) (Natural k)
multNumbers (Finite n m1) (Finite k m2) =
  if m2 == m1 then Left $ Finite ((n * k) `mod` m1) m1
              else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)

public export
divNumbers : NonRec -> NonRec -> Either NonRec String
divNumbers (Natural n) (Natural k) = Left $ Natural $ n `div` k
divNumbers (Natural n) (Finite k m1) = 
  case modinverse (cast k) m1 of
       Left o => Left $ Finite ((o * n) `mod` m1) m1
       Right s => Right s

divNumbers (Finite n m1) (Natural k) = 
  case modinverse (cast k) m1 of
       Left o => Left $ Finite ((o * n) `mod` m1) m1
       Right s => Right s

divNumbers (Finite n m1) (Finite k m2) =
  if m2 == m1 then 
              case modinverse (cast k) m1 of
                Left o => Left $ Finite ((o * n) `mod` m1) m1
                Right s => Right s
              else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)

public export
modNumbers : NonRec -> NonRec -> Either NonRec String
modNumbers (Natural a) (Natural b) = Left $ Natural $ a `mod` b
modNumbers a b = Right $ "Can't mod " ++ (show a) ++ " by " ++ (show b)
