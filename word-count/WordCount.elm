module WordCount exposing (..)

import Dict exposing (..)
import Maybe exposing (withDefault)
import Regex exposing (..)
import Debug exposing (log)


updateMap : String -> Dict String Int -> Dict String Int
updateMap string map =
    case string of
        "" ->
            map

        word ->
            let
                value =
                    withDefault 0 (Dict.get (String.toLower word) map)
            in
                Dict.insert (String.toLower word) (value + 1) map


countWords : List String -> Dict String Int
countWords strings =
    strings
        |> List.foldl updateMap Dict.empty


wordCount : String -> Dict String Int
wordCount string =
    string
        |> Regex.split All (regex "[\\W]")
        |> countWords
