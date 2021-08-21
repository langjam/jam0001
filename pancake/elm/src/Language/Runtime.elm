module Language.Runtime exposing (..)

import Array
import Language.AST as AST exposing (AST, Code, Instruction, Universe(..))
import Language.Ops as Ops
import Language.Stack exposing (Stack)
import Maybe.Extra as MaybeX


type alias Runtime =
    { stack : Stack
    , ast : Code
    , ip : Int -- instruction pointer
    , ok : Bool
    , universe : Universe
    }


init : AST -> Runtime
init ast =
    { stack = []
    , ast = AST.toCode ast
    , ip = 0
    , ok = True
    , universe = Alpha
    }


step : Runtime -> Runtime
step runtime =
    MaybeX.unwrap (panic runtime) (exec runtime) <|
        Array.get runtime.ip runtime.ast


next : Runtime -> Runtime
next runtime =
    { runtime | ip = runtime.ip + 1 }


exec : Runtime -> Instruction -> Runtime
exec runtime instruction =
    case instruction of
        ( universe, atom ) ->
            if runtime.universe == universe then
                let
                    maybeStack =
                        Ops.exec atom runtime.stack
                in
                case maybeStack of
                    Just stack ->
                        next { runtime | stack = stack }

                    Nothing ->
                        panic runtime

            else
                runtime



-- UTIL


panic : Runtime -> Runtime
panic runtime =
    { runtime | ok = False }
