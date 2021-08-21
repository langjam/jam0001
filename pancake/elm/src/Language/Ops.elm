module Language.Ops exposing (add, exec)

import Array
import Language.AST as AST exposing (Atom(..), Executioner, Func)
import Language.Stack exposing (Stack)
import Maybe.Extra as MaybeX


exec : Atom -> Stack -> Maybe Stack
exec atom stack =
    case atom of
        Actual func ->
            eval func stack

        _ ->
            atom :: stack |> Just


eval : Func -> Stack -> Maybe Stack
eval func stack =
    enoughArgs func stack


enoughArgs : Func -> Stack -> Maybe Stack
enoughArgs func stack =
    if List.length stack >= AST.argsLeft func then
        Just stack

    else
        Nothing


add : Func
add =
    let
        func : Executioner
        func =
            Array.toList >> sum
    in
    AST.func 2 func


sum : List Atom -> Maybe Atom
sum =
    MaybeX.traverse AST.toInt
        >> Maybe.map (List.sum >> Int)
