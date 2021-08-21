module Language.Pancake exposing (..)

import Language.Core as Core exposing (Runtime)
import Language.Parser exposing (parse)
import Parser exposing (DeadEnd)


compile : String -> Result (List DeadEnd) Runtime
compile =
    parse >> Result.map Core.init
