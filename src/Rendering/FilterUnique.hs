module Rendering.FilterUnique (filterUnique) where
import Data.HashSet
import Rendering.InfoTypes


filter' :: [Information] -> HashSet Information
filter' x = fromList x


filterUnique :: [Information] -> [Information]
filterUnique someList 
    | length someList == 0 = []
    | length someList == 1 = someList
    | otherwise = toList (filter' someList)


intersection' :: [Information] -> [Information] -> [Information]
intersection' a b = [x | x <- (filterUnique (a ++ b)), elem x (fromList a), elem x (fromList b)]


