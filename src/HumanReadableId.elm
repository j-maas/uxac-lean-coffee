module HumanReadableId exposing (generate, humanize)

import Murmur3
import Random exposing (Generator, Seed)



-- TODO: Prevent collisions by adding a function that takes a List (Maybe String) and returns a List String that ensures collisions are deduplicated.


humanize : String -> String
humanize id =
    let
        seed =
            Murmur3.hashString 0 id
    in
    generate (Random.initialSeed seed)
        |> Tuple.first


generate : Seed -> ( String, Seed )
generate seed =
    Random.step userNameGenerator seed


userNameGenerator : Generator String
userNameGenerator =
    Random.map2
        (\adjective animal ->
            adjective ++ " " ++ animal
        )
        adjectiveGenerator
        animalGenerator


adjectiveGenerator : Generator String
adjectiveGenerator =
    Random.uniform
        "Adorable"
        [ "Amazing"
        , "Awesome"
        , "Brave"
        , "Charming"
        , "Creative"
        , "Colorful"
        , "Enthusiastic"
        , "Elegant"
        , "Generous"
        , "Fascinating"
        , "Fantastic"
        , "Friendly"
        , "Glorious"
        , "Incredible"
        , "Inventive"
        , "Kind"
        , "Outstanding"
        , "Spectacular"
        , "Sympathetic"
        , "Wonderful"
        , "Witty"
        ]


animalGenerator : Generator String
animalGenerator =
    Random.uniform
        "Butterfly"
        [ "Camel"
        , "Cat"
        , "Dolphin"
        , "Duck"
        , "Elephant"
        , "Gazelle"
        , "Giraffe"
        , "Guinea Pig"
        , "Horse"
        , "Kangaroo"
        , "Llama"
        , "Mouse"
        , "Ostrich"
        , "Penguin"
        , "Rabbit"
        , "Seahorse"
        , "Seal"
        , "Turtle"
        , "Whale"
        , "Zebra"
        ]
