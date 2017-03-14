module Util exposing (cropTo)


cropTo : Int -> Int -> Int -> Int
cropTo min max value =
    if (max <= min) then
        min
    else if (value < min) then
        min
    else if (value >= max) then
        max - 1
    else
        value
