module Statistics (divWithDefault, (/.)) where

divWithDefault : Float -> Float -> Float -> Float
divWithDefault default dividend divisor =
  if divisor == 0
    then 
      default
    else
      dividend / divisor

(/.) = 
  divWithDefault 0
