module Remote exposing (Remote(..), map, toMaybe)


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
