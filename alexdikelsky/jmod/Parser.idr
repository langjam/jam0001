module Parser
import Types

import Data.String.Parser
import Data.String.Parser.Expression
import Data.List1
import Data.Fin

whitespaceSep : Monad m => ParseT m a -> ParseT m (List1 a)
whitespaceSep p = p `sepBy1` spaces

naturalLit : Parser NonRec
naturalLit = do
  x <- some digit
  pure (Natural (getNatural x))
where 
  getNatural : List (Fin 10) -> Nat
  getNatural = foldl (\a => \b => 10 * a + cast b) 0

modLit : Parser NonRec
modLit = do
  x <- some digit
  token "m"
  y <- some digit
  pure (Finite (getNatural x) (getNatural y))
where 
  getNatural : List (Fin 10) -> Nat
  getNatural = foldl (\a => \b => 10 * a + cast b) 0

nonRecLit : Parser NonRec
nonRecLit = modLit <|> naturalLit

array0 : Parser Expr
array0 = do
  t <- nonRecLit
  pure (Array0 t)

array1 : Parser Expr
array1 = do
  token "["
  es <- whitespaceSep nonRecLit
  token "]"
  pure (Array1 (forget es))

array1nonrec : Parser (List NonRec)
array1nonrec = do
  token "["
  es <- whitespaceSep nonRecLit
  token "]"
  pure (forget es)

array2 : Parser Expr
array2 = do
  token "["
  es <- whitespaceSep array1nonrec
  token "]"
  pure (Array2 (forget es))

symbol : Parser Expr
symbol = do
  x <- some (letter <|> (char '-') 
        <|> (char '+') <|> (char '*') <|> (char '.') <|> (char '/')
        <|> (char '|') <|> (char ':') <|> (char '=') <|> (char 'λ')
        <|> (char '%') <|> (char '-')) 
  pure (Symbol (pack x))

emptyList : Parser Expr
emptyList = do
  token "("
  token ")"
  pure (ConsList [])


mutual 
  expr : Parser Expr
  expr = list <|> symbol <|> array0 <|> array1 <|> array2

  populatedList : Parser Expr
  populatedList = do
    token "("
    es <- whitespaceSep expr
    token ")"
    pure (ConsList (forget es))
  
  list : Parser Expr
  list = emptyList <|> populatedList

  parseToEnd : Parser Expr
  parseToEnd = do
    spaces
    e <- expr
    eos
    pure e

public export
parseExpr : String -> Either String (Expr, Int)
parseExpr s = parse parseToEnd s
