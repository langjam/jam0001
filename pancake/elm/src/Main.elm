port module Main exposing (..)

import Exchange exposing (..)
import Language.AST as AST exposing (Universe(..))
import Language.Core exposing (Runtime)
import Language.Pancake as Pancake
import Platform exposing (Program, worker)



-- FLAGS


type alias Flags =
    ()



-- MODEL


type Model
    = Idle
    | Compiled Runtime



-- MSG


type Msg
    = GotSrc String
    | GotStep Bool



-- MAIN


main : Program Flags Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Idle, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSrc sourceCode ->
            case Pancake.compile sourceCode of
                Nothing ->
                    ( Idle, result compilationFail )

                Just runtime ->
                    ( Compiled runtime, result compilationOk )

        GotStep _ ->
            ( model, state <| StateInfo <| AST.universeToString Alpha )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ compile GotSrc
        , step GotStep
        ]



-- PORTS


port compile : (String -> msg) -> Sub msg


port result : CompilationResult -> Cmd msg


port step : (Bool -> msg) -> Sub msg


port state : StateInfo -> Cmd msg
