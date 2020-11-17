module Rendering.CommentTools where
import Tools.WhiteSpaceTools


commentBlockWidth :: Int
commentBlockWidth = 120


-- Make a line of dashes:
genDashLine :: Int -> String
genDashLine n
    | n <= 0 = ""
    | otherwise = "-" ++ (genDashLine (n-1))


dashLine :: String
dashLine = genDashLine commentBlockWidth


centerStr :: Int -> String -> String
centerStr n s = "--" ++ (spacePad (n2-2) s) where
    n2 = quot (commentBlockWidth - length(s)) 2


prependDashes :: String -> String
prependDashes s = "--    " ++ s


-- Creates a comment block in which the first string is centered,
-- and any strings thereafter are left justified. 
commentBlock :: [String] -> [String]
commentBlock los
    | los == [] = []
    | otherwise = 
        [dashLine] ++ 
        [centerStr commentBlockWidth (head los)] ++ 
        (if ((length los) > 1)
            then (map prependDashes (tail los))
            else []) ++ 
        [dashLine]


