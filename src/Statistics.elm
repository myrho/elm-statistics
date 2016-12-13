module Statistics exposing (divWithDefault, (/.), median, quantileR7)

{-|
A growing collection of utility functions for statistics.

# Quantile
@docs median, quantileR7

# Division
@docs divWithDefault, (/.)
-}

import Array


{-|
Division of numbers by 0 results in a given default.

    divWithDefault 1 5 0 = 1
    divWithDefault 1 5 2 = 2.5
-}
divWithDefault : Float -> Float -> Float -> Float
divWithDefault default dividend divisor =
    if divisor == 0 then
        default
    else
        dividend / divisor


{-|
Division of numbers by 0 results in 0.

    5 /. 0 = 0
    5 /. 2 = 2.5
-}
(/.) : Float -> Float -> Float
(/.) =
    divWithDefault 0


{-|
Calculate the median of a list of values

    median [1,1,1,3,4,7,9,11,13,13] = 5.5
-}
median : List Float -> Float
median =
    quantileR7 0.5


{-|
Calculate the quantile of a list of values using formula R-7
(https://en.wikipedia.org/wiki/Quantile).

    quantile 0.3 [1,1,1,3,4,7,9,11,13,13] = 2.4
    quantile 0.5 [1,1,1,3,4,7,9,11,13,13] = 5.5 -- the median
-}
quantileR7 : Float -> List Float -> Float
quantileR7 p values =
    let
        p_clamped =
            clamp 0 1 p

        v =
            Array.fromList <|
                List.sort values

        n =
            List.length values

        h =
            ((toFloat n) - 1) * p

        h_floor =
            floor h

        x_h_floor =
            Array.get h_floor v
                |> Maybe.withDefault 0

        x_h_plus1 =
            Array.get (h_floor + 1) v
                |> Maybe.withDefault 0
    in
        x_h_floor
            + (h - (toFloat h_floor))
            * (x_h_plus1 - x_h_floor)
