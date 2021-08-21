module Language.Parser exposing (..)

import Char exposing (isAlphaNum)
import Language.AST exposing (AST, Atom(..), Instruction, Universe(..))
import Parser exposing (..)
import Tuple exposing (..)


parse =
    preprocess >> run parser


preprocess : String -> String
preprocess =
    String.lines
        >> List.map
            (\line ->
                if String.trim line |> String.isEmpty then
                    "pass"

                else
                    line
            )
        >> String.join "\n"


parser : Parser AST
parser =  




-- Parser.sequence
--     { start = ""
--     , separator = "\n"
--     , end = ""
--     , spaces = spaces
--     , item = lazy (\_ -> instruction)
--     , trailing = Optional
--     }


-- helper revInst =
--     oneOf
--         [ succeed (\inst -> Loop (inst :: revInst))
--             |. spaces
--             |= instruction
--             |. spaces
--             |. symbol "\n"
--         , succeed () |> map (\_ -> Done (List.reverse revInst))
--         ]


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
