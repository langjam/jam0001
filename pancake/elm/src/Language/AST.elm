module Language.AST exposing (..)

import Array exposing (Array)
import Maybe.Extra as MaybeX


type alias AST =
    List Instruction


type alias Code =
    Array Instruction


type alias Instruction =
    ( Universe, Atom )


type Universe
    = Alpha
    | Omega


type alias Func =
    { args : Array Atom
    , argi : Int
    , func : Array Atom -> Maybe Atom
    }


type Atom
    = Int Int
    | Add


toInt : Atom -> Maybe Int
toInt atom =
    case atom of
        Int int ->
            Just int

        _ ->
            Nothing


sum : List Atom -> Maybe Atom
sum =
    MaybeX.traverse toInt
        >> Maybe.map (List.sum >> Int)



-- AST UTILS


toCode : AST -> Code
toCode =
    Array.fromList


universeToString : Universe -> String
universeToString universe =
    case universe of
        Alpha ->
            "normal"

        Omega ->
            "comment"
