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



-- FUNC


type alias Executioner =
    Array Atom -> Maybe Atom


type alias Func =
    { args : Array Atom
    , argi : Int
    , func : Executioner
    }


func : Int -> Executioner -> Func
func argc exec =
    let
        dummyAtom =
            Int 0
    in
    { args = Array.repeat argc dummyAtom
    , argi = 0
    , func = exec
    }


apply : Func -> Atom -> Func
apply f a =
    { args = Array.set f.argi a f.args
    , argi = f.argi + 1
    , func = f.func
    }


argsLeft : Func -> Int
argsLeft f =
    Array.length f.args - f.argi



-- ATOM


{-| Int, Str, and List are always Quoted.
Functions might not be quoted though.
-}
type Atom
    = Int Int
    | Str String
    | List (List Atom)
    | Quoted Func
    | Actual Func


toInt : Atom -> Maybe Int
toInt atom =
    case atom of
        Int int ->
            Just int

        _ ->
            Nothing
