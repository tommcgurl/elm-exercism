module SpaceAge exposing (..)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


getPlanetTimeRelativeToEarth : Planet -> Float
getPlanetTimeRelativeToEarth planet =
    case planet of
        Mercury ->
            0.2408467 * 365.25

        Venus ->
            0.61519726 * 365.25

        Earth ->
            1.0 * 365.25

        Mars ->
            1.8808158 * 365.25

        Jupiter ->
            11.862615 * 365.25

        Saturn ->
            29.447498 * 365.25

        Uranus ->
            84.016846 * 365.25

        Neptune ->
            164.79132 * 365.25


ageOn : Planet -> Int -> Float
ageOn planet seconds =
    seconds
        |> toFloat
        -- convert seconds to minutes
        |>
            flip (/) 60
        -- convert minutes to hours
        |>
            flip (/) 60
        -- convert hours to days
        |>
            flip (/) 24
        -- convert earth days to planets days
        |>
            flip (/) (getPlanetTimeRelativeToEarth planet)
