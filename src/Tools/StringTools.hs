module Tools.StringTools (
      findSubStrIdx
    , containsSubStr
    , toLower
    , endsInNumbers
    , findNumberIdx
    , skipFirstChars
    , replaceSubStr
    , doubleBackslashes
    ) where
import qualified Data.Char as DC
import Tools.LogicTools


findSubStrIdx :: String -> String -> Integer -> Maybe Integer
findSubStrIdx "" _ _ = Nothing
findSubStrIdx s target n
    | take (length target) s == target      = Just n
    | otherwise                             = findSubStrIdx (tail s) target (n + 1)


containsSubStr :: String -> String -> Bool
containsSubStr s t
    | (findSubStrIdx s t 0) == Nothing      = False
    | otherwise                             = True


toLower :: String -> String
toLower s = map DC.toLower s


takeLast :: Int -> String -> String
takeLast n s = (reverse (take n (reverse s)))


endsInNumbers :: Int -> String -> Bool
endsInNumbers n s = logicalAnd boolList where
    boolList = map (\x -> elem x ['0'..'9']) (takeLast n s)


findNumberGuts :: String -> Int -> Int
findNumberGuts s n 
    | (n == length s) = 0
    | (endsInNumbers n s) = findNumberGuts s (n + 1)
    | otherwise = n

-- Finds the index of first number, but only when a string ends in numbers.
-- If string does not end in numbers, returns length of string
findNumberIdx :: String -> Int
findNumberIdx s = (length s) - (findNumberGuts s 1) + 1


skipFirstChars :: String -> Int -> String 
skipFirstChars "" _ = ""
skipFirstChars s 0 = s
skipFirstChars s n = skipFirstChars (tail s) (n-1)


-- Replaces first occurrence only:
replaceSubStr :: String -> String -> String -> String
replaceSubStr "" _ _ = ""
replaceSubStr _ "" _ = ""
replaceSubStr thing2Change thing2Delete thing2Insert
    | thing2Change == "" = ""
    | thing2Delete == "" = thing2Change
    | (take (length thing2Delete) thing2Change == thing2Delete) =
        thing2Insert ++ (skipFirstChars thing2Change (length thing2Delete))
    | otherwise = [head thing2Change] ++ (replaceSubStr (tail thing2Change) thing2Delete thing2Insert)


doubleBackslashes :: String -> String
doubleBackslashes "" = ""
doubleBackslashes s
    | (head s == '\\') = ['\\', '\\'] ++ (doubleBackslashes (tail s))
    | otherwise = [head s] ++ (doubleBackslashes (tail s))










