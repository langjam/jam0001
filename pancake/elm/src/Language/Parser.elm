module Language.Parser exposing (parse)

import Language.AST exposing (AST, Atom(..), Instruction, Universe(..))
import Maybe.Extra as MaybeX


parse : String -> Maybe AST
parse =
    String.lines >> MaybeX.traverse instruction


instruction : String -> Maybe Instruction
instruction _ =
    Just ( Alpha, Int 42 )
