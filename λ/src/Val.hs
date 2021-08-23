module Val where

import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Data.IORef(IORef, readIORef, newIORef)
import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (intercalate)

import Syntax
import Unsafe.Coerce (unsafeCoerce)
import Data.Void (Void)
import Data.Typeable (Typeable)
import GHC.Base (reallyUnsafePtrEquality#)
import Parser hiding (parse)
import Text.Parsec (getInput, parse, choice, try, eof)
import Data.Foldable (toList)
import Data.Functor (($>))

data Val
  = Unit
  | Num Integer
  | Bool Bool
  | Str String
  | List (IORef [Val])
  | Rec (IORef (Map Ident Val))
  | Fn (Val -> IO (Either EvalError Val))
  deriving stock Typeable

type EvalError = String

type SeenVals = [IORef Void]

showArr :: SeenVals -> [Val] -> String
showArr seen a = "[" ++ intercalate ", " (showVal seen <$> a) ++ "]"

showField :: SeenVals -> Ident -> Val -> String
showField seen f v = f ++ " = " ++ showVal seen v

showRec :: SeenVals -> Map Ident Val -> String
showRec seen m
  | M.null m = "{ }"
  | otherwise = "{ " ++ intercalate ", " (uncurry (showField seen) <$> M.toList m) ++ " }"

guardCycle :: SeenVals -> IORef a -> IO String -> String
guardCycle seen r s =
  if unsafeCoerce r `elem` seen
  then "<∞>"
  else unsafePerformIO s

addSeen :: IORef a -> SeenVals -> SeenVals
addSeen = (:) . unsafeCoerce

escapeComms :: Char -> String
escapeComms c
  | c `elem` "{-}" = ['\\', c]
  | otherwise = [c]

showVal :: SeenVals -> Val -> String
showVal _ Unit = "()"
showVal _ (Num n) = show n
showVal _ (Bool b) = toLower <$> show b
showVal _ (Str s) = escapeComms =<< show s
showVal seen (List a) = guardCycle seen a $ showArr (addSeen a seen) <$> readIORef a
showVal seen (Rec r) = guardCycle seen r $ showRec (addSeen r seen) <$> readIORef r
showVal _ Fn{} = "<λ>"

instance Show Val where
  show = showVal []

compareVal :: Val -> Val -> Either EvalError Ordering
compareVal Unit Unit = pure EQ
compareVal (Num a) (Num b) = pure $ compare a b
compareVal (Bool a) (Bool b) = pure $ compare a b
compareVal (Str a) (Str b) = pure $ compare a b
compareVal _ _ = Left "Invalid values in comparison"

eqVal :: Val -> Val -> Bool
eqVal Unit Unit = True
eqVal (Num a) (Num b) = a == b
eqVal (Bool a) (Bool b) = a == b
eqVal (Str a) (Str b) = a == b

eqVal (List a') (List b') = a' == b' || unsafePerformIO do
  a <- readIORef a'
  b <- readIORef b'
  pure $ length a == length b && a == b

eqVal (Rec a') (Rec b') = a' == b' || unsafePerformIO do
  a <- M.toList <$> readIORef a'
  b <- M.toList <$>  readIORef b'

  pure $ a == b

eqVal _ _ = False

instance Eq Val where
  (==) = eqVal

physEqVal :: Val -> Val -> Bool
physEqVal (List a) (List b) = a == b
physEqVal (Rec a) (Rec b) = a == b

physEqVal (Fn a) (Fn b) =
  case reallyUnsafePtrEquality# a b of
    0# -> False
    _ -> True

physEqVal a b = eqVal a b

truthy :: Val -> Bool
truthy Unit = True
truthy (Num n) = n /= 0
truthy (Bool b) = b
truthy (Str s) = not $ null s
truthy (List a) = unsafePerformIO $ not . null <$> readIORef a
truthy (Rec r) = unsafePerformIO $ not . M.null <$> readIORef r
truthy Fn{} = True

valType :: Val -> String
valType Unit = "()"
valType Num{} = "Num"
valType Bool{} = "Bool"
valType Str{} = "Str"
valType List{} = "List"
valType Rec{} = "Rec"
valType Fn{} = "Fn"

unitVal :: Parser Val
unitVal = unit $> Unit

numVal :: Parser Val
numVal = Num <$> intRaw

boolVal :: Parser Val
boolVal = Bool <$> boolRaw

strVal :: Parser Val
strVal = Str <$> strRaw

listVal :: Parser Val
listVal = list (List . unsafePerformIO . newIORef) pVal

recVal :: Parser Val
recVal = rec' (Rec . unsafePerformIO . newIORef . M.fromList) pVal

pVal' :: Parser Val
pVal' = choice $ try <$> [recVal, listVal, strVal, boolVal, numVal, unitVal]

pVal :: Parser Val
pVal = ws *> pVal' <* ws <* eof

withRest :: Parser a -> Parser (a, String)
withRest p = (,) <$> p <*> getInput

instance Read Val where
  readsPrec _ = toList . parse (withRest pVal) ""
