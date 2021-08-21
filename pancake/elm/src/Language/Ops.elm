module Language.Ops exposing (add)


add : List Int -> Maybe (List Int)
add stack =
    let
        argc =
            2

        top =
            List.take argc >> List.sum >> List.singleton

        rest =
            List.drop argc
    in
    if List.length stack >= argc then
        Just <| List.append (top stack) (rest stack)

    else
        Nothing
