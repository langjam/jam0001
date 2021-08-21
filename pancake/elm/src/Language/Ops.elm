module Language.Ops exposing (exec)

import Flip exposing (flip)
import Language.AST as AST exposing (Atom(..))
import Language.Stack exposing (Stack)


exec : Atom -> Stack -> Maybe Stack
exec atom stack =
    case atom of
        Int _ ->
            atom :: stack |> Just

        Add ->
            add stack


add : Stack -> Maybe Stack
add stack =
    let
        argc =
            2

        top : Maybe Atom
        top =
            List.take argc stack |> AST.sum

        rest : Stack
        rest =
            List.drop argc stack

        result : Maybe Stack
        result =
            Maybe.andThen (flip (::) rest >> Just) top
    in
    enoughArgs argc stack result


enoughArgs : Int -> Stack -> Maybe Stack -> Maybe Stack
enoughArgs argc stack result =
    if List.length stack >= argc then
        result

    else
        Nothing
