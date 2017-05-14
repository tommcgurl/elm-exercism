module RunLengthEncoding exposing (..)

import Dict exposing (..)
import Array exposing (..)
import Regex exposing (..)
import Debug exposing (log)


type alias Counter =
    { currentLetter : String
    , count : Int
    , acc : String
    }


version : Int
version =
    2


getCounts : String -> Counter -> Counter
getCounts nextLetter counter =
    let
        currentLetter =
            counter.currentLetter

        currentCount =
            counter.count

        sameLetter =
            nextLetter == currentLetter
    in
        case sameLetter of
            True ->
                { counter | count = (currentCount + 1) }

            False ->
                let
                    acc =
                        if currentCount == 1 then
                            counter.acc ++ currentLetter
                        else
                            counter.acc ++ (toString currentCount) ++ currentLetter
                in
                    { counter
                        | count = 1
                        , currentLetter = nextLetter
                        , acc = acc
                    }


encode : String -> String
encode string =
    let
        counter =
            string
                |> String.split ""
                |> List.foldl getCounts { currentLetter = "", count = 1, acc = "" }
    in
        if counter.count == 1 then
            counter.acc ++ counter.currentLetter
        else
            counter.acc ++ (toString counter.count) ++ counter.currentLetter



-- |> String.split ""


constructString : Array String -> String
constructString array =
    case (Array.isEmpty array) of
        True ->
            ""

        False ->
            let
                digit =
                    Maybe.withDefault "" (Array.get 0 array)

                letter =
                    Maybe.withDefault "" (Array.get 1 array)

                isInt =
                    String.toInt digit

                arrayEnd =
                    (Array.length array)
            in
                case isInt of
                    Err _ ->
                        -- Continue looking at the tail
                        digit ++ constructString (Array.slice 1 arrayEnd array)

                    Ok val ->
                        -- construct string
                        (String.repeat val letter) ++ (constructString (Array.slice 2 arrayEnd array))


decode : String -> String
decode encodedString =
    encodedString
        |> Regex.split Regex.All (Regex.regex "([0-9]*)([\\w\\s]?)")
        |> List.filter (\val -> val /= "")
        |> Array.fromList
        |> constructString
