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
    JsonE.list encodeValue



-- CODE


type alias Code =
    Array Instruction


toCode : AST -> Code
toCode =
    List.filter (Tuple.second >> AST.isLabel >> not) >> Array.fromList


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
            [ ( "+", Fun add )
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
                    alter runtime

                Nothing ->
                    case label of
                        Just to ->
                            jump to runtime

                        Nothing ->
                            case name of
                                Just v ->
                                    case v of
                                        Fun f ->
                                            Debug.log "next runtime" <|
                                                next runtime

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
        [ ( "pass", next ) ]


next : Command
next runtime =
    { runtime | ip = runtime.ip + 1 }


jump : Int -> Command
jump ip runtime =
    { runtime | ip = ip }


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
            JsonE.object [ ( "func", JsonE.bool True ) ]

        Name name ->
            JsonE.object [ ( "name", JsonE.string name ) ]


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


toInt : Value -> Maybe Int
toInt value =
    case value of
        Int int ->
            Just int

        _ ->
            Nothing


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


add : Func
add =
    func 2 sum



-- HELPERS


sum : Executioner
sum =
    MaybeX.traverseArray toInt
        >> Maybe.map (Array.toList >> List.sum >> Int)
