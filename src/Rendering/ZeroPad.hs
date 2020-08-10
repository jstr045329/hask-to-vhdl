module Rendering.ZeroPad where


-- This function merely prepends a string with 0's:
zeroPad :: Int -> String -> String
zeroPad 0 s = s
zeroPad n s
    | n <= (length s)       = s
    | otherwise             = "0" ++ (zeroPad (n - 1) s)


-- This function is like zeroPad, but puts the 0's on the right.
padLSBs :: Int -> String -> String
padLSBs 0 s = s
padLSBs n s
    | n <= (length s)       = s
    | otherwise             = (padLSBs (n - 1) s) ++ "0"


-- This function gives you a field of fixed width n, 
-- unless s is longer than n in which case it returns s:
rightJustifyBitString :: String -> Int -> String
rightJustifyBitString s n
    | (n <= (length s)) = s
    | otherwise = zeroPad n s  


-- This function gives you a field of fixed width n, 
-- unless s is longer than n in which case it returns s:
leftJustifyBitString :: String -> Int -> String
leftJustifyBitString s n 
    | (n < (length s)) = s
    | otherwise = padLSBs n s 
