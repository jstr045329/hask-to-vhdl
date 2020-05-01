module Rendering.IntToBin where


intToBin :: Int -> String
intToBin 0 = "0"
intToBin 1 = "1"
intToBin n
    | mod n 2 == 1      = intToBin (div n 2) ++ "1"
    | otherwise         = intToBin (div n 2) ++ "0"

