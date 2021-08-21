module Language.Parser exposing (..)

import Language.AST exposing (AST, Atom(..), Instruction, Universe(..))
import Language.Ops as Ops
import Maybe.Extra as MaybeX
import Parser exposing (..)
import Task exposing (sequence)
import Tuple exposing (..)


parse : String -> Maybe AST


parser _ =
    Just [ ( Omega, Int 3 ) ]



-- parse =
--     String.lines >> MaybeX.traverse instruction
-- instruction : String -> Maybe Instruction
-- instruction _ =
--     Just ( Alpha, Int 42 )


instruction : Parser Instruction
instruction =
    oneOf
        [ succeed (pair Alpha)
            |. spaces
            |= atom
        , succeed (pair Omega)
            |. spaces
            |. symbol "#"
            |. spaces
            |= atom
        ]


atom : Parser Atom
atom =
    oneOf
        [ succeed Int |= int
        , succeed Str
            |. symbol "\""
            |= getChompedString (chompUntil "\"")
            |. symbol "\""
        , succeed List
            |= Parser.sequence
                { start = "["
                , separator = ","
                , end = "]"
                , spaces = spaces
                , item = lazy (\_ -> atom)
                , trailing = Forbidden -- demand a trailing semi-colon
                }
        , succeed Quoted
            |. symbol "{"
            |= func
            |. symbol "}"
        , succeed Actual
            |= func
        ]


func : Parser String
func =
    getChompedString (chompIf (\c -> c == '+' || c == '-'))
