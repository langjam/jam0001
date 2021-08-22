module Exchange exposing (..)

import Json.Encode as JsonE
import Language.AST exposing (universeToString)
import Language.Core as Core exposing (Runtime)


type alias CompilationResult =
    { successful : Bool
    , errorLines : List Int
    , typedLines : List String -- "normal" / "comment" / "label"
    }


compilationOk : List String -> CompilationResult
compilationOk typedLines =
    { successful = True
    , errorLines = []
    , typedLines = typedLines
    }


compilationFail : List Int -> CompilationResult
compilationFail errorLines =
    { successful = False
    , errorLines = errorLines
    , typedLines = []
    }


type alias StateInfo =
    { universe : String
    , activeLine : Int
    , stack : JsonE.Value -- JSON
    , alive : Bool
    }


stateInfo : Runtime -> StateInfo
stateInfo runtime =
    { universe = universeToString runtime.universe
    , activeLine = runtime.ip
    , stack = Core.encodeStack runtime.stack
    , alive = runtime.ok
    }
