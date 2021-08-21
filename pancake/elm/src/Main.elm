port module Main exposing (..)

import Language.Pancake exposing (compile)
import Language.Runtime exposing (Runtime)
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
    | GotCommd String



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
    ( Idle, event "ready" )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSrc sourceCode ->
            case compile sourceCode of
                Nothing ->
                    ( Idle, event "compilation fail" )

                Just runtime ->
                    ( Compiled runtime, event "compilation ok" )

        GotCommd command ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    src GotSrc



-- PORTS


port src : (String -> msg) -> Sub msg


port event : String -> Cmd msg


port commd : (String -> msg) -> Sub msg
