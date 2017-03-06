module Leap exposing (..)


isLeapYear : Int -> Bool
isLeapYear year =
    if year % 4 == 0 then
        (year % 100 /= 0) || (year % 400 == 0) && True
    else
        False
