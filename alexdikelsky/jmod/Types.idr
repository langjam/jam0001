module Types

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
