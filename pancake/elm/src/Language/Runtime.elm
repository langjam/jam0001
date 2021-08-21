module Language.Runtime exposing (..)

import Array
import Language.AST as AST exposing (AST, Code, Instruction(..))
import Language.Ops as Ops


type alias Runtime =
    { stack : List Int
    , ast : Code
    , ip : Int -- instruction pointer
    , ok : Bool
    }


init : AST -> Runtime
init ast =
    { stack = []
    , ast = AST.toCode ast
    , ip = 0
    , ok = True
    }


step : Runtime -> Runtime
step runtime =
    case Array.get runtime.ip runtime.ast of
        Nothing ->
            { runtime | ok = False }

        Just instruction ->
            exec runtime instruction


exec : Runtime -> Instruction -> Runtime
exec runtime instruction =
    case instruction of
        Int int ->
            { runtime | stack = int :: runtime.stack }

        Add ->
            case Ops.add runtime.stack of
                Nothing ->
                    { runtime | ok = False }

                Just stack ->
                    { runtime | stack = stack }
