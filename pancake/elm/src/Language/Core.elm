module Language.Core exposing (..)

import Array exposing (Array)
import Language.AST as AST
    exposing
        ( AST
        , Atom(..)
        , Code
        , Instruction
        , Universe(..)
        )
import Maybe.Extra as MaybeX


type alias Stack =
    List Atom


type alias Runtime =
    { code : Code

    -- Internals.
    , stack : Stack
    , ip : Int -- Instruction pointer.
    , ok : Bool
    , universe : Universe
    }


init : AST -> Runtime
init ast =
    { code = AST.toCode ast
    , stack = []
    , ip = 0
    , ok = True
    , universe = Alpha
    }


exec : Runtime -> Instruction -> Runtime
exec runtime instruction =
    case instruction of
        ( universe, atom ) ->
            if runtime.universe == universe then
                next runtime

            else
                runtime


{-| `Command` has access to `Runtime` and can do whatever it pleases.
`Command`s should _not_ be quoted!
-}
type alias Command =
    Runtime -> Runtime


next : Command
next runtime =
    { runtime | ip = runtime.ip + 1 }


step : Runtime -> Runtime
step runtime =
    MaybeX.unwrap (panic runtime) (exec runtime) <|
        Array.get runtime.ip runtime.code


panic : Runtime -> Runtime
panic runtime =
    { runtime | ok = False }


{-| `Func` is a restricted function that only has access to its arguments.
It can be partially applied since every `Func` is curried.
-}
type alias Func =
    { args : Array Atom
    , argi : Int
    , func : Executioner
    }


type alias Executioner =
    Array Atom -> Maybe Atom


func : Int -> Executioner -> Func
func argc f =
    let
        dummyAtom =
            Int 0
    in
    { args = Array.repeat argc dummyAtom
    , argi = 0
    , func = f
    }


app : Func -> Atom -> Func
app f a =
    { args = Array.set f.argi a f.args
    , argi = f.argi + 1
    , func = f.func
    }


argsLeft : Func -> Int
argsLeft f =
    Array.length f.args - f.argi



-- LIBRARY


add : Func
add =
    func 2 sum



-- HELPERS


sum : Array Atom -> Maybe Atom
sum =
    MaybeX.traverseArray AST.toInt
        >> Maybe.map (Array.toList >> List.sum >> Int)
