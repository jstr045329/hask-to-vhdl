module Parsing.TokenMatchingTools where

skipNTokens :: [String] -> Int -> [String]
skipNTokens los 0 = los
skipNTokens [] _ = []
skipNTokens los n = skipNTokens (tail los) (n - 1)


keywordMatch :: [String] -> [String] -> Bool
keywordMatch los1 los2 = los1 == los2


afterKeyword :: [String] -> [String] -> [String]
afterKeyword los keywordList
    | los == [] = []
    | take (length keywordList) los == keywordList = skipNTokens los (length keywordList)
    | otherwise = afterKeyword (tail los) keywordList


untilKeyword :: [String] -> [String] -> [String] -> [String]
untilKeyword los_in keywordList los_out
    | los_in == [] = []
    | take (length keywordList) los_in == keywordList = los_out
    | otherwise = untilKeyword (tail los_in) keywordList (los_out ++ (take 1 los_in))




