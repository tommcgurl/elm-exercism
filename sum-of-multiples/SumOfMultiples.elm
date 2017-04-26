module SumOfMultiples exposing (..)

import Maybe exposing (withDefault)


isMultiple : Int -> Int -> Bool
isMultiple numerator denominator =
    (numerator % denominator) == 0


hasMultiple : List Int -> Int -> Bool
hasMultiple numbers suspect =
    numbers
        |> List.any (isMultiple suspect)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples numbers n =
    let
        rangeStart =
            withDefault 1 (List.minimum numbers)

        upToN =
            List.range rangeStart (n - 1)
    in
        upToN
            |> List.filter (hasMultiple numbers)
            |> List.foldl (+) 0
