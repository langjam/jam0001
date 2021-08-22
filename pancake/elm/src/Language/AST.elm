module Language.AST exposing (..)

-- AST


type alias AST =
    List Instruction


type alias Instruction =
    ( Universe, Atom )



-- UNIVERSE


type Universe
    = Alpha
    | Omega
    | Lambda


flipUniverse : Universe -> Universe
flipUniverse universe =
    case universe of
        Alpha ->
            Omega

        Omega ->
            Alpha

        Lambda ->
            Debug.todo "unreachable"


universeToString : Universe -> String
universeToString universe =
    case universe of
        Alpha ->
            "normal"

        Omega ->
            "comment"

        Lambda ->
            "label"



-- ATOM


type Atom
    = Int Int
    | Str String
    | List (List Atom)
    | Label String
    | Quoted String
    | Actual String


isLabel : Atom -> Bool
isLabel atom =
    case atom of
        Label _ ->
            True

        _ ->
            False
