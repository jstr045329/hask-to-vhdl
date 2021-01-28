module Tools.ListTools where


-- Define a version of tail that doesn't die just because you give it an empty list:
tail' :: [a] -> [a]
tail' [] = []
tail' x = tail x


skipN :: [a] -> Int -> [a]
skipN [] _ = []
skipN someList 0 = someList
skipN someList n = skipN (tail someList) (n-1)


dropLast :: [a] -> [a]
dropLast someList
    | length someList == 0 = []
    | length someList == 1 = []
    | otherwise = [head someList] ++ (dropLast (tail someList))


------------------------------------------------------------------------------------------------------------------------
--                                            TODO: Delete This Function
--
-- Replace calls to this with calls to StringTools.joinStringsWithCommas
------------------------------------------------------------------------------------------------------------------------
joinWithCommas :: [String] -> String
joinWithCommas [] = ""
joinWithCommas tokList
    | length tokList == 1 = head tokList
    | otherwise = (head tokList) ++ ", " ++ (joinWithCommas (tail tokList))


------------------------------------------------------------------------------------------------------------------------
--                                          Take The Last N Items In A List
------------------------------------------------------------------------------------------------------------------------
lastN :: [a] -> Int -> [a]
lastN [] _ = []
lastN _ 0 = []
lastN someList n
    | length someList <= n = someList
    | otherwise = lastN (tail someList) n

------------------------------------------------------------------------------------------------------------------------
--                                            TODO: Delete This Function
-- For new functions, use skipN (above).
------------------------------------------------------------------------------------------------------------------------
repTail :: Int -> [String] -> [String]
repTail 0 xs = xs
repTail n xs = tail (repTail (n-1) xs)


------------------------------------------------------------------------------------------------------------------------
--                                      Flatten a List of Lists to Just A List
------------------------------------------------------------------------------------------------------------------------
flattenShallow :: [[a]] -> [a]
flattenShallow [[]] = []
flattenShallow [] = []
flattenShallow listOfLists = (head listOfLists) ++ (flattenShallow (tail listOfLists))


-- NOTE: This function uses a C-style -1 to signal that 
indexOf :: (Eq a) => a -> [a] -> Int -> Int
indexOf x [] _ = -1
indexOf x someList n
    | x == (head someList) = n
    | otherwise = indexOf x (tail someList) (n+1)


