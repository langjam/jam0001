module Language.Stack exposing (Stack)

import Language.AST exposing (Atom)


type alias Stack =
    List Atom
