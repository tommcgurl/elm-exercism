module DifferenceOfSquares exposing (squareOfSum, sumOfSquares, difference)


sum : List Int -> Int
sum list =
    list
        |> List.foldl (+) 0


sumAndSquare : List Int -> Int
sumAndSquare list =
    list
        |> List.foldl (\next acc -> acc + (square next)) 0


square : Int -> Int
square int =
    int ^ 2


squareOfSum : Int -> Int
squareOfSum n =
    List.range 1 n
        |> sum
        |> square


sumOfSquares : Int -> Int
sumOfSquares n =
    List.range 1 n
        |> sumAndSquare


difference : Int -> Int
difference n =
    (squareOfSum n) - (sumOfSquares n)
