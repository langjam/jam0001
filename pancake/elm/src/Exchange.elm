module Exchange exposing (..)


type alias CompilationResult =
    { successful : Bool
    }


compilationOk : CompilationResult
compilationOk =
    CompilationResult True


compilationFail : CompilationResult
compilationFail =
    CompilationResult False


type alias StateInfo =
    { mode : String
    }
