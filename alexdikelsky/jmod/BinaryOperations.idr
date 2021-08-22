module BinaryOperations
import Types
import Data.Nat
import NumberTheory

public export
addNumbers : NonRec -> NonRec -> Either NonRec String
addNumbers (Natural n) (Natural k) = Left $ Natural $ n + k
addNumbers (Natural n) (Finite k m1) = Left $ Finite ((n + k) `mod` m1) m1
addNumbers (Finite n m1) (Natural k) = addNumbers (Natural k) (Finite n m1)
addNumbers (Finite n m1) (Finite k m2) =
  if m2 == m1 then Left $ Finite ((n + k) `mod` m1) m1
              else Right $ "Added numbers with modulus " ++ (show m1) ++ " and " ++ (show m2)
addNumbers a b = Right $ "Failed to add " ++ (show a) ++ " and " ++ (show b)

public export
multNumbers : NonRec -> NonRec -> Either NonRec String
multNumbers (Natural n) (Natural k) = Left $ Natural $ n * k
multNumbers (Natural n) (Finite k m1) = Left $ Finite ((n * k) `mod` m1) m1
multNumbers (Finite n m1) (Natural k) = multNumbers (Natural k) (Finite n m1)
multNumbers (Finite n m1) (Finite k m2) =
  if m2 == m1 then Left $ Finite ((n * k) `mod` m1) m1
              else Right $ "Modulus mismatch " ++ (show m1) ++ " and " ++ (show m2)
multNumbers a b = Right $ "Failed to mult " ++ (show a) ++ " and " ++ (show b)

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
divNumbers a b = Right $ "Failed to divide " ++ (show a) ++ " and " ++ (show b)

public export
modNumbers : NonRec -> NonRec -> Either NonRec String
modNumbers (Natural a) (Natural b) = Left $ Natural $ a `mod` b
modNumbers a b = Right $ "Can't mod " ++ (show a) ++ " by " ++ (show b)

public export
eqNumbers : NonRec -> NonRec -> Either NonRec String
eqNumbers (Natural a) (Natural b) = Left $ if a == b then truth else falsehood 
eqNumbers (Finite a b) (Finite c d) = Left $ if (a == c) && (b == d) then truth else falsehood
eqNumbers _ _ = Left $ falsehood

public export
badNumbers : NonRec -> NonRec -> Either NonRec String
badNumbers (Natural a) (Natural b) = Left $ if a == b then truth else falsehood 
badNumbers (Finite a b) (Finite c d) = Left $ if (a == c) then truth else falsehood
badNumbers (Natural a) (Finite c d) = badNumbers (Natural a) (Natural c)
badNumbers (Finite a b) (Natural c) = badNumbers (Natural a) (Natural c)
badNumbers _ _ = Left $ falsehood

public export
gcdNumbers : NonRec -> NonRec -> Either NonRec String
gcdNumbers (Natural a) (Natural b) = 
  let gcd = fst (xgcd (cast a) (cast b)) in
      if gcd >= 0
         then Left (Natural (cast gcd))
         else Right $ "GCD failed, returning " ++ (show gcd) ++ 
                     " given " ++ (show a) ++ " and " ++ (show b)
gcdNumbers a b = Right $ (show a) ++ " and " ++ (show b) ++ " have no gcd"
