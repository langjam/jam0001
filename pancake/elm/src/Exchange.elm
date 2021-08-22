module Exchange exposing (..)


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
    { mode : String
    }
