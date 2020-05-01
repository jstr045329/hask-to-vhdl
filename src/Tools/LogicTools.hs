module Tools.LogicTools where


logicalOr :: [Bool] -> Bool
logicalOr someList
    | someList == [] = False
    | head someList == True = True
    | otherwise = logicalOr (tail someList)


logicalAnd :: [Bool] -> Bool
logicalAnd someList
    | someList == [] = True
    | head someList == False = False
    | otherwise = logicalAnd (tail someList)


-- The motivation for this function is to make something pickier i
-- than a true OR gate. Technically you can make p anything in [0, 1].
-- This function acts more like an OR gate when p < 0.5, and more like an AND gate 
-- when p > 0.5. This function returns True when more than p * num_inputs are true.
pickyOr :: (Fractional a, Ord a) => [Bool] -> a -> Bool
pickyOr someList p = ((sum (map (\x -> if x then 1 else 0) someList)) / (fromIntegral (length someList))) >= p


-- This function gives a more descriptive name to pickyOr when you intend to use
-- it more like an AND gate. Both functions are equivalent. Use p < 0.5 when you 
-- want something like an OR gate; and use p > 0.5 when you want something like
-- an AND gate. When p is close to 1, the gate returns True when the inputs are
-- in a near consensus; otherwise, returns False.
shloppyAnd :: (Fractional a, Ord a) => [Bool] -> a -> Bool
shloppyAnd someList p = pickyOr someList p


-- The following 9 functions simply give descriptive names to some common uses of pickyOr:
minority10 :: [Bool] -> Bool
minority10 someList = pickyOr someList 0.1


minority20 :: [Bool] -> Bool
minority20 someList = pickyOr someList 0.2


minority30 :: [Bool] -> Bool
minority30 someList = pickyOr someList 0.3


minority40 :: [Bool] -> Bool
minority40 someList = pickyOr someList 0.4


majority50 :: [Bool] -> Bool
majority50 someList = pickyOr someList 0.5


majority60 :: [Bool] -> Bool
majority60 someList = pickyOr someList 0.6


majority70 :: [Bool] -> Bool
majority70 someList = pickyOr someList 0.7


majority80 :: [Bool] -> Bool
majority80 someList = pickyOr someList 0.8


majority90 :: [Bool] -> Bool
majority90 someList = pickyOr someList 0.9


