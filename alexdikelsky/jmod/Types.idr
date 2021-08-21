module Types

public export
data Expr =
  Natural Nat
| Finite Nat Nat
| Array (List Expr)
| Function (Either Expr String -> Either Expr String)
| Symbol String
| Quoted Expr

public export
Context : Type
Context = List (String, Expr)


public export
Show Expr where
  show (Natural n) = show n
  show (Finite n k) = (show n) ++ "_" ++ (show k)
  show (Array k) = "[" ++ (foldr (\x => \y => x ++ " " ++ y) "" (map show k)) ++ "]"
  show (Function _) = "Function"
  show (Symbol k) = show k
  show (Quoted n) = "'" ++ show n
