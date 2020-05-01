module Rendering.ZeroPad where


zeroPad :: Int -> String -> String
zeroPad 0 s = s
zeroPad n s
    | n <= (length s)       = s
    | otherwise             = "0" ++ (zeroPad (n - 1) s)


