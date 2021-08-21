module Language.Pancake exposing (..)

import Language.Parser exposing (parse)
import Language.Runtime as Runtime exposing (Runtime)


compile : String -> Maybe Runtime
compile src =
    Maybe.map Runtime.init (parse src)
