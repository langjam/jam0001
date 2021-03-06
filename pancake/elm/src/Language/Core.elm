module Language.Core exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as JsonE
import Language.AST as AST
    exposing
        ( AST
        , Atom
        , Instruction
        , Universe(..)
        )
import Language.Parser exposing (identifier)
import Maybe.Extra as MaybeX


type alias Stack =
    List Value


encodeStack : Stack -> JsonE.Value
encodeStack =
    List.reverse >> JsonE.list encodeValue



-- CODE


type alias Code =
    Array Instruction


{-| `toCode` will replace all labels with `next` instructions.
-}
toCode : AST -> Code
toCode =
    let
        replaceLabel atom =
            if AST.isLabel atom then
                AST.Actual "pass"

            else
                atom
    in
    List.map (Tuple.mapSecond replaceLabel) >> Array.fromList


toLabels : AST -> Dict String Int
toLabels =
    let
        enumerate id ( _, atom ) =
            ( id, atom )

        label ( id, atom ) =
            case atom of
                AST.Label l ->
                    Just ( l, id )

                _ ->
                    Nothing
    in
    List.indexedMap enumerate >> List.filterMap label >> Dict.fromList



-- RUNTIME


type alias Runtime =
    { code : Code
    , ip : Int -- Instruction pointer.
    , universe : Universe

    -- Internals.
    , labels : Dict String Int
    , names : Dict String Value
    , stack : Stack
    , ok : Bool
    }


init : AST -> Runtime
init ast =
    { code = toCode ast
    , ip = 0
    , universe = Alpha
    , labels = toLabels ast
    , names =
        Dict.fromList
            [ ( "+", Fun <| func 2 sum )
            , ( "-", Fun <| func 2 sub )
            , ( "<", Fun <| func 2 lt )
            ]
    , stack = []
    , ok = True
    }


{-| `exec` deals with a series of `Atom`s. Here's how they look:

    type Atom
        = Int Int
        | Str String
        | List (List Atom)
        | Quoted String
        | Actual String
        | Label String -- always skipped

-}
exec : Runtime -> Instruction -> Runtime
exec runtime instruction =
    case instruction of
        ( universe, atom ) ->
            if runtime.universe == universe then
                dealWithAtom atom runtime

            else
                -- Skip this instruction as it's from a different universe.
                -- Labels are always skipped since runtime is never in Lambda!
                next runtime


{-| All atoms but the `Actual` are considered to be `Quoted` and are therefore,
pushed onto the `stack` immediately.

`Quoted` identifiers may belong to one of 3 groups:

1.  Command
2.  Label
3.  Name
4.  Unknown

-}
dealWithAtom : Atom -> Runtime -> Runtime
dealWithAtom atom runtime =
    case atom of
        AST.Actual identifier ->
            let
                command =
                    Dict.get identifier commands

                label =
                    Dict.get identifier runtime.labels

                name =
                    Dict.get identifier runtime.names
            in
            case command of
                Just alter ->
                    alter runtime |> next

                Nothing ->
                    case label of
                        Just to ->
                            jump to runtime

                        Nothing ->
                            case name of
                                Just v ->
                                    case v of
                                        Fun f ->
                                            eval f runtime |> next

                                        other ->
                                            push other runtime |> next

                                Nothing ->
                                    panic runtime

        _ ->
            push (toValue atom) runtime |> next


{-| `Command` has access to `Runtime` and can do whatever it pleases.
`Command`s should _not_ be quoted!
-}
type alias Command =
    Runtime -> Runtime


commands : Dict String Command
commands =
    Dict.fromList
        [ ( "pass", pass )
        , ( "flip", flip )
        , ( "flip_if", flip_if )
        , ( "halt", exit )
        ]


next : Command
next runtime =
    { runtime | ip = runtime.ip + 1 }


pass : Command
pass =
    identity


jump : Int -> Command
jump ip runtime =
    { runtime | ip = ip }


exit : Command
exit runtime =
    { runtime | ok = False }


flip : Command
flip runtime =
    { runtime | universe = AST.flipUniverse runtime.universe }


flip_if : Command
flip_if runtime =
    let
        ( runtime_, maybeValue ) =
            pop runtime
    in
    case maybeValue of
        Just (Int int) ->
            if int == 0 then
                runtime_

            else
                flip runtime_

        _ ->
            panic runtime


step : Command
step runtime =
    MaybeX.unwrap (panic runtime) (exec runtime) <|
        Array.get runtime.ip runtime.code


panic : Command
panic runtime =
    { runtime | ok = False }


push : Value -> Command
push atom runtime =
    { runtime | stack = atom :: runtime.stack }


pop : Runtime -> ( Runtime, Maybe Value )
pop runtime =
    if List.isEmpty runtime.stack then
        ( panic runtime, Nothing )

    else
        let
            x =
                List.head runtime.stack

            stack =
                Maybe.withDefault [] <| List.tail runtime.stack
        in
        ( { runtime | stack = stack }, x )


eval : Func -> Command
eval f runtime =
    if argsLeft f == 0 then
        case execute f of
            Just value ->
                push value runtime

            Nothing ->
                panic runtime

    else
        let
            ( runtime_, maybeValue ) =
                pop runtime
        in
        case maybeValue of
            Nothing ->
                runtime_

            Just top ->
                eval (app f top) runtime_


{-| `Func` is a restricted function that only has access to its arguments.
It can be partially applied since every `Func` is curried.
-}
type alias Func =
    { args : Array Value
    , argi : Int
    , func : Executioner
    }


type alias Executioner =
    Array Value -> Maybe Value


func : Int -> Executioner -> Func
func argc f =
    let
        dummyValue =
            Int 0
    in
    { args = Array.repeat argc dummyValue
    , argi = 0
    , func = f
    }


app : Func -> Value -> Func
app f v =
    { args = Array.set f.argi v f.args
    , argi = f.argi + 1
    , func = f.func
    }


argsLeft : Func -> Int
argsLeft f =
    Array.length f.args - f.argi


execute : Func -> Maybe Value
execute f =
    f.func f.args



-- LIBRARY


type Value
    = Int Int
    | Char Char
    | List (List Value)
    | Fun Func
    | Name String


encodeValue : Value -> JsonE.Value
encodeValue value =
    case value of
        Int int ->
            JsonE.int int

        Char char ->
            JsonE.string <| String.fromChar char

        List list ->
            JsonE.list encodeValue list

        Fun _ ->
            JsonE.object [ ( "type", JsonE.string "func" ) ]

        Name name ->
            JsonE.object
                [ ( "type", JsonE.string "name" )
                , ( "name", JsonE.string name )
                ]


toValue : Atom -> Value
toValue atom =
    case atom of
        AST.Int int ->
            Int int

        AST.Str str ->
            List <| List.map Char <| String.toList str

        AST.List atoms ->
            List <| List.map toValue atoms

        AST.Quoted id ->
            Name id

        _ ->
            Debug.todo "unreachable"


toInt : Value -> Int
toInt value =
    case value of
        Int int ->
            int

        _ ->
            Debug.todo "oops"


toChar : Value -> Maybe Char
toChar value =
    case value of
        Char char ->
            Just char

        _ ->
            Nothing


toList : Value -> Maybe (List Value)
toList value =
    case value of
        List list ->
            Just list

        _ ->
            Nothing


toFunc : Value -> Maybe Func
toFunc value =
    case value of
        Fun f ->
            Just f

        _ ->
            Nothing



-- HELPERS


sum : Executioner
sum =
    Array.map toInt >> Array.toList >> List.sum >> Int >> Just


sub : Executioner
sub args =
    let
        int id =
            toInt <| Maybe.withDefault (Int 0) <| Array.get id args
    in
    Just <| Int (int 1 - int 0)


lt : Executioner
lt args =
    let
        int id =
            toInt <| Maybe.withDefault (Int 0) <| Array.get id args

        result =
            int 1 < int 0
    in
    if result then
        Just (Int 1)

    else
        Just (Int 0)
