module Language.Parser exposing (..)

import Char exposing (isAlphaNum)
import Language.AST exposing (AST, Atom(..), Instruction, Universe(..))
import Parser exposing (..)
import Tuple exposing (pair)


parse : String -> List (Result (List DeadEnd) Instruction)
parse =
    preprocess >> List.map (run instruction)


preprocess : String -> List String
preprocess =
    String.lines
        >> List.map
            (\line ->
                if String.trim line |> String.isEmpty then
                    "pass"

                else
                    line
            )


parser : Parser AST
parser =
    Debug.todo "aleksimart"


instruction : Parser Instruction
instruction =
    oneOf
        [ succeed (pair Omega)
            |. symbol "#"
            |. spaces
            |= atom
            |. spaces
            |. end
        , succeed (pair Alpha)
            |. spaces
            |= atom
            |. spaces
            |. end
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
                , trailing = Forbidden
                }
        , succeed Quoted
            |. symbol "{"
            |= identifier
            |. symbol "}"
        , succeed Actual
            |= identifier
        ]


identifier : Parser String
identifier =
    getChompedString (chompWhile isIdentifier)


isIdentifier : Char -> Bool
isIdentifier c =
    isAlphaNum c || List.member c (String.toList "<=>!+-*/")
