module Parsing.TokenMatchingTools where


skipNTokens :: [String] -> Int -> [String]
skipNTokens los 0 = los
skipNTokens [] _ = []
skipNTokens los n = skipNTokens (tail los) (n - 1)


keywordMatch :: [String] -> [String] -> Bool
keywordMatch los1 los2 = los1 == los2


-- Excludes the tokens that match at the beginning
afterKeyword :: [String] -> [String] -> [String]
afterKeyword los keywordList
    | los == [] = []
    | take (length keywordList) los == keywordList = skipNTokens los (length keywordList)
    | otherwise = afterKeyword (tail los) keywordList


-- Excludes the tokens that match at the end
untilKeyword :: [String] -> [String] -> [String] -> [String]
untilKeyword los_in keywordList los_out
    | los_in == [] = []
    | take (length keywordList) los_in == keywordList = los_out
    | otherwise = untilKeyword (tail los_in) keywordList (los_out ++ (take 1 los_in))


-- Includes the tokens that match at the beginning:
afterKeywordIncFirst :: [String] -> [String] -> [String]
afterKeywordIncFirst los keywordList
    | los == [] = []
    | take (length keywordList) los == keywordList = los
    | otherwise = afterKeywordIncFirst (tail los) keywordList


-- Includes tokens that match at the end:
untilKeywordIncEnd :: [String] -> [String] -> [String] -> [String]
untilKeywordIncEnd los_in keywordList los_out
    | los_in == [] = []
    | (take (length keywordList) los_in == keywordList) = (los_out ++ keywordList)
    | otherwise = untilKeywordIncEnd (tail los_in) keywordList (los_out ++ (take 1 los_in))


-- A version of afterKeyword that includes an early termination argument. 
-- Allows you to avoid searching all the way to the end of a file just to 
-- determine that "downto" isn't used, and other wasteful situations like that.
-- Excludes the tokens that match at the beginning
afterKeywordEarlyTerm :: [String] -> [String] -> [String] -> [String]
afterKeywordEarlyTerm los keywordList termList
    | los == [] = []
    | take (length termList) los == termList = []
    | take (length keywordList) los == keywordList = skipNTokens los (length keywordList)
    | otherwise = afterKeyword (tail los) keywordList


-- EXCLUDES matching tokens at the beginning
-- This function searches depth first, but terminates search when it encounters a semicolon
-- so inefficiency is greatly reduced.
afterAny :: [String] -> [[String]] -> [String]
afterAny los keywordListList
    | los == [] = []
    | keywordListList == [] = []
    | (length (afterKeywordEarlyTerm los (head keywordListList) [";"]) > 0) = 
        (afterKeywordEarlyTerm los (head keywordListList) [";"])
    | otherwise = afterAny los (tail keywordListList)


-- INCLUDES matching tokens at the beginning
-- This function searches depth first, but terminates search when it encounters a semicolon
-- so inefficiency is greatly reduced.
afterAnyIncFirst :: [String] -> [[String]] -> [String]
afterAnyIncFirst los keywordListList
    | los == [] = []
    | keywordListList == [] = []
    | (length (afterKeywordEarlyTerm los (head keywordListList) [";"]) > 0) = 
        (afterKeywordEarlyTerm los (head keywordListList) [";"]) ++ (head keywordListList)
    | otherwise = afterAnyIncFirst los (tail keywordListList)


