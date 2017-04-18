module Strain exposing (..)


keep : (a -> Bool) -> List a -> List a
keep condition list =
    case list of
        [] ->
            []

        x :: tail ->
            if (condition x) then
                x :: (keep condition tail)
            else
                keep condition tail


discard : (a -> Bool) -> List a -> List a
discard condition list =
    case list of
        [] ->
            []

        x :: tail ->
            if (condition x) then
                discard condition tail
            else
                x :: (discard condition tail)
