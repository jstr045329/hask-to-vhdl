module Parsing.SensitivityScanner where
import Data.HashSet
import Parsing.GuaranteeWhitespace
import Parsing.TokenMatchingTools


isolateProcess :: [String] -> [String]
isolateProcess los = 
    untilKeyword
        (afterKeyword los ["process"])
        ["end", "process"]
        []


isolateSensitivity :: [String] -> [String]
isolateSensitivity los =
    untilKeyword
        (afterKeyword los ["("])
        [")"]
        []


removeOneProcess :: [String] -> [String]
removeOneProcess los = skipNTokens los (length (untilKeyword los ["end", "process", ";"] []) + 3)


isolateAllProcesses :: [String] -> [[String]]
isolateAllProcesses los
    | los == [] = [[]]
    | length (isolateProcess los) == 0 = [[]]
    | otherwise = [isolateProcess los] ++ isolateAllProcesses (removeOneProcess los)

   
--sensitivitySets :: [String] -> [fromList [String]]
 
