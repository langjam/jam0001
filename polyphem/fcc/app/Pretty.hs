{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeSynonymInstances #-}
module Pretty where

import Data.Text
import Text.PrettyPrint
import qualified Data.Map as Map
import Prelude hiding ((<>))

import AST

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String



newtype TVar = TV Text
                 deriving (Eq, Show, Ord)

type Var = Identifier

data Type = TVar TVar
          | TCon Text
          | TArr Type Type
              deriving (Eq, Show, Ord)

typeInt, typeBool, typeChar, typeFloat :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"
typeChar = TCon "Char"
typeFloat = TCon "Float"

data Scheme = Forall [TVar] Type
              deriving (Show)

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)


parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id


class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Var where
    ppr _ (I x) = text $ unpack x

instance Pretty TVar where
    ppr _ (TV x) = text $ unpack x

instance Pretty Type where
  ppr p (TArr a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TArr{} = True
      isArrow _ = False
  ppr p (TVar a) = ppr p a
  ppr _ (TCon a) = text $ unpack a

instance Pretty Scheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) = text "forall" <+> hcat (punctuate space (fmap (ppr p) ts)) <> text "." <+> ppr p t

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b



instance Show TypeError where
  show (UnificationFail a b) = unpack $
    Data.Text.concat ["Cannot unify types: \n\t", pack $ pptype a, "\nwith \n\t", pack $ pptype b]
  show (InfiniteType (TV a) b) = unpack $
    Data.Text.concat ["Cannot construct the infinite type: ", a, " = ", pack $ pptype b]
  show (UnboundVariable a) = unpack $ Data.Text.concat ["Not in scope: ", pack a]

pptype :: Type -> String
pptype = render . ppr 0

