module Accumulate exposing (..)


accumulate : (a -> a) -> List a -> List a
accumulate func list =
    case list of
        [] ->
            []

        x :: tail ->
            (func x) :: (accumulate func tail)
