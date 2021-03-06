module Parsing.ExtractEntity where
import Parsing.TokenMatchingTools


getEntityName :: [String] -> String
getEntityName [] = []
getEntityName los 
    | (length (afterKeyword los ["entity"]) > 0) = head (afterKeyword los ["entity"])
    | otherwise = []


isolateEntityDec :: [String] -> [String]
isolateEntityDec los
    | length (los) < 5 = []
    | los !! 0 /= "entity" = isolateEntityDec (tail los)

    | length (untilKeyword los ["end", "entity"] []) > 0 =
        (untilKeyword los ["end", "entity"] [])

    | length (untilKeyword los ["end", (los !! 1)] []) > 0 =
        (untilKeyword los ["end", (los !! 1)] [])

    | length (untilKeyword los ["end"] []) > 0 =
        (untilKeyword los ["end"] [])

    | otherwise = []


