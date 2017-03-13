module Pangram exposing (..)

import Regex exposing (..)


dedupeList : List String -> String
dedupeList strings =
    strings
        |> List.foldl
            (\next acc ->
                if next == (String.right 1 acc) then
                    acc
                else
                    acc ++ next
            )
            ""


isPangram : String -> Bool
isPangram string =
    string
        |> replace All (regex "[^a-zA-Z]") (\_ -> "")
        |> String.toLower
        |> String.split ""
        |> List.sort
        |> dedupeList
        |> (==) "abcdefghijklmnopqrstuvwxyz"
