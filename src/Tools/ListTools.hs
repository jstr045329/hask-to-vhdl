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


