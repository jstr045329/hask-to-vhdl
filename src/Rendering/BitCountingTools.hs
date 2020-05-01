module Rendering.BitCountingTools where


count1s :: String -> Int
count1s ""                  = 0 
count1s s
    | head s == '1'         = 1 + (count1s $ tail s)
    | otherwise             = count1s $ tail s


count0s :: String -> Int
count0s ""                  = 0
count0s s
    | head s == '0'         = 1 + (count0s $ tail s)
    | otherwise             = count0s $ tail s

