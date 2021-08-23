module Language.Pancake exposing (..)

import Language.AST exposing (Instruction, Universe, universeToString)
import Language.Core as Core exposing (Runtime)
import Language.Parser as Parser
import Parser exposing (DeadEnd)
import Result.Extra as ResultX


compile : Parser.Output -> Result Parser.Output Runtime
compile parsed =
    case ResultX.combine parsed of
        Ok ast ->
            Ok <| Core.init ast

        Err _ ->
            Err parsed


errorLines : Parser.Output -> List Int
errorLines =
    let
        enumerate : Int -> Result (List DeadEnd) Instruction -> ( Int, Bool )
        enumerate idx result =
            case result of
                Ok _ ->
                    ( idx, True )

                Err _ ->
                    ( idx, False )

        justError : ( Int, Bool ) -> Maybe Int
        justError p =
            if Tuple.second p then
                Nothing

            else
                Just <| Tuple.first p
    in
    List.indexedMap enumerate >> List.filterMap justError


typedLines : Parser.Output -> List String
typedLines =
    let
        justUniverse : Result (List DeadEnd) Instruction -> Universe
        justUniverse result =
            case result of
                Ok ( universe, _ ) ->
                    universe

                Err _ ->
                    Debug.todo "should never happen"
    in
    List.map (justUniverse >> universeToString)
