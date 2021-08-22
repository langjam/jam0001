module NumberTheory

public export
xgcd : Integer -> Integer -> (Integer, Integer, Integer)
xgcd 0 b = (b, 0, 1)
xgcd a b = let (g, s, t) = xgcd (b `mod` a) a in
               (g, t - (b `div` a) * s, s)

public export
modinverse : Integer -> Nat -> Either Nat String
modinverse n m = 
  case xgcd n (cast m) of
       (1, n, _) => 
          Left $ if n > 0 
             then cast n
             else cast ((cast m) + n)
       (k, a, b) => Right $ "Tried to find inverse of " ++ (show n) ++ "_" ++ (show m)
