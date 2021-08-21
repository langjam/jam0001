module Language.AST exposing (..)

import Array exposing (Array)



-- AST


type alias AST =
    List Instruction



-- CODE


type alias Code =
    Array Instruction


toCode : AST -> Code
toCode =
    Array.fromList


type alias Instruction =
    ( Universe, Atom )



-- UNIVERSE


type Universe
    = Alpha
    | Omega


universeToString : Universe -> String
universeToString universe =
    case universe of
        Alpha ->
            "normal"

        Omega ->
            "comment"



-- ATOM


{-| Int, Str, and List are always Quoted.
Functions might not be quoted though.
-}
type Atom
    = Int Int
    | Str String
    | List (List Atom)
    | Quoted String
    | Actual String


toInt : Atom -> Maybe Int
toInt atom =
    case atom of
        Int int ->
            Just int

        _ ->
            Nothing


toStr : Atom -> Maybe String
toStr atom =
    case atom of
        Str str ->
            Just str

        _ ->
            Nothing


toList : Atom -> Maybe (List Atom)
toList atom =
    case atom of
        List list ->
            Just list

        _ ->
            Nothing
