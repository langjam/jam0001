module Language.Pancake exposing (..)

import Language.Core as Core exposing (Runtime)
import Language.Parser exposing (parse)


compile : String -> Maybe Runtime
compile =
    parse >> Maybe.map Core.init
