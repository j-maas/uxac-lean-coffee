module Remote exposing (Remote(..), andMap, map, toMaybe)


type Remote a
    = Loading
    | Got a


map : (a -> b) -> Remote a -> Remote b
map f remote =
    case remote of
        Loading ->
            Loading

        Got a ->
            Got (f a)


toMaybe : Remote a -> Maybe a
toMaybe remote =
    case remote of
        Loading ->
            Nothing

        Got a ->
            Just a


andMap : Remote a -> Remote (a -> b) -> Remote b
andMap first second =
    case ( first, second ) of
        ( Got a, Got mapping ) ->
            Got (mapping a)

        _ ->
            Loading
