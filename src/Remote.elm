module Remote exposing (Remote(..), map)


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
