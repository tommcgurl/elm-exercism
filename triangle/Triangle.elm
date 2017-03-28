module Triangle exposing (..)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene
    | Degenerate


flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


getTriangleType : Float -> Float -> Float -> Triangle
getTriangleType x y z =
    if (x == y) && (x == z) then
        Equilateral
    else if (x == y) || (x == z) || (y == z) then
        Isosceles
    else
        Scalene


triangleKind : Float -> Float -> Float -> Result String Triangle
triangleKind x y z =
    let
        sides =
            List.sortWith flippedComparison [ x, y, z ]

        maxSide =
            Maybe.withDefault 0 (List.head sides)

        tailSum =
            List.sum (Maybe.withDefault [ 0 ] (List.tail sides))
    in
        if (x <= 0 || y <= 0 || z <= 0) then
            Err "Invalid lengths"
        else if (tailSum < maxSide) then
            Err "Violates inequality"
        else if (tailSum == maxSide) then
            Ok Degenerate
        else
            Ok (getTriangleType x y z)
