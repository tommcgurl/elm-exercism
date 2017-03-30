module ScrabbleScore exposing (..)

import String exposing (toLower, split)
import Dict exposing(..)

letterValues : Dict String number
letterValues =
    Dict.fromList
        [ ( "a", 1 )
        , ( "e", 1 )
        , ( "i", 1 )
        , ( "o", 1 )
        , ( "u", 1 )
        , ( "l", 1 )
        , ( "n", 1 )
        , ( "r", 1 )
        , ( "s", 1 )
        , ( "t", 1 )
        , ( "d", 1 )
        , ( "g", 2 )
        , ( "b", 3 )
        , ( "c", 3 )
        , ( "m", 3 )
        , ( "p", 3 )
        , ( "f", 4 )
        , ( "h", 4 )
        , ( "v", 4 )
        , ( "w", 4 )
        , ( "y", 4 )
        , ( "k", 5 )
        , ( "j", 8 )
        , ( "x", 8 )
        , ( "q", 10 )
        , ( "z", 10 )
        ]


getLetterScore : String -> Int
getLetterScore letter =
    -- Always return 0 if the letter is not found.
    Maybe.withDefault 0 (Dict.get letter letterValues)


scoreWord : String -> Int
scoreWord word =
    word
        |> toLower
        |> split ""
        |> List.foldl (\next acc -> acc + (getLetterScore next)) 0
