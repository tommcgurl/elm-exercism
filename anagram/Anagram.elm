module Anagram exposing (..)


isAnagram : String -> String -> Bool
isAnagram word candidate =
    let
        lowerWord =
            word
                |> String.toLower

        lowerCandidate =
            candidate
                |> String.toLower

        sortedWord =
            lowerWord
                |> String.split ""
                |> List.sort

        sortedCandidate =
            lowerCandidate
                |> String.split ""
                |> List.sort
    in
        if (lowerWord == lowerCandidate) then
            False
        else
            sortedWord == sortedCandidate


detect : String -> List String -> List String
detect word candidates =
    candidates
        |> List.filter (isAnagram word)
