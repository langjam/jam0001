module Language.Parser exposing (parse)

import Language.AST exposing (AST, Atom(..), Instruction, Universe(..))
import Language.Ops as Ops
import Maybe.Extra as MaybeX


parse : String -> Maybe AST
parse _ =
    -- String.lines >> MaybeX.traverse instruction
    Just
        [ ( Alpha, Int 1 )
        , ( Alpha, Int 41 )
        , ( Alpha, Actual Ops.add )
        ]


instruction : String -> Maybe Instruction
instruction _ =
    Just ( Alpha, Int 42 )
