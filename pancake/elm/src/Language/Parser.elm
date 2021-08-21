module Language.Parser exposing (parse)

import Language.AST exposing (AST, Instruction(..))


parse : String -> Maybe AST
parse _ =
    Just [ Int 1, Int 41, Add ]
