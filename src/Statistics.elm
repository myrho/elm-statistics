module Statistics (divWithDefault, (/.), median) where

divWithDefault : Float -> Float -> Float -> Float
divWithDefault default dividend divisor =
  if divisor == 0
    then 
      default
    else
      dividend / divisor

(/.) = 
  divWithDefault 0

median : List Float -> Float
median values =
  let
    len =
      List.length values
  in
    List.sort values
      |> List.drop ((toFloat len ) / 2 |> ceiling |> (\c -> c - 1))
      |> List.take (2 - (len % 2))
      |> List.sum
      |> (\s -> s / (toFloat <| 2 - len % 2))
