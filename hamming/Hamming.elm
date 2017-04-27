module Hamming exposing (..)

import Array exposing (..)


getDistance : Array Char -> Array Char -> Int
getDistance dnaOne dnaTwo =
    let
        iterator =
            List.range 0 ((Array.length dnaOne) - 1)
    in
        iterator
            |> List.filter (\index -> (Array.get index dnaOne) /= (Array.get index dnaTwo))
            |> List.length


distance : String -> String -> Maybe Int
distance dnaOne dnaTwo =
    let
        lenDiff =
            (String.length dnaOne) - (String.length dnaTwo)

        dnaOneArray =
            dnaOne |> String.toList |> Array.fromList

        dnaTwoArray =
            dnaTwo |> String.toList |> Array.fromList
    in
        case lenDiff of
            0 ->
                if dnaOne == dnaTwo then
                    Just 0
                else
                    Just (getDistance dnaOneArray dnaTwoArray)

            _ ->
                Nothing
