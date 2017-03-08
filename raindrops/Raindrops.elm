module Raindrops exposing (..)


pling : Int -> String
pling number =
    case number of
        0 ->
            "Pling"

        _ ->
            ""


plang : Int -> String
plang number =
    case number of
        0 ->
            "Plang"

        _ ->
            ""


plong : Int -> String
plong number =
    case number of
        0 ->
            "Plong"

        _ ->
            ""


raindrops : Int -> String
raindrops number =
    let
        rain =
            (pling (number % 3) ++ plang (number % 5) ++ plong (number % 7))
    in
        case rain of
            "" ->
                toString (number)

            _ ->
                rain
