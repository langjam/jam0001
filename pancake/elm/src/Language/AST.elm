module Language.AST exposing (..)

import Array exposing (Array)


type alias AST =
    List Instruction


toCode : AST -> Code
toCode =
    Array.fromList


type alias Code =
    Array Instruction


type Instruction
    = Int Int
    | Add
