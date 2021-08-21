module Language.Ops exposing (exec)

import Language.AST as AST exposing (Atom(..), Func)
import Language.Stack exposing (Stack)


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
