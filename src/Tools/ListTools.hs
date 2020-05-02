module Tools.ListTools where


dropLast :: [a] -> [a]
dropLast someList
    | length someList == 0 = []
    | length someList == 1 = []
    | otherwise = [head someList] ++ (dropLast (tail someList))


joinWithCommas :: [String] -> String
joinWithCommas [] = ""
joinWithCommas tokList
    | length tokList == 1 = head tokList
    | otherwise = (head tokList) ++ ", " ++ (joinWithCommas (tail tokList))


lastN :: [a] -> Int -> [a]
lastN _ 0 = []
lastN someList n
    | length someList <= n = someList
    | otherwise = lastN (tail someList) n

    

