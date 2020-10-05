module Tools.StringTools (
      findSubStrIdx
    , containsSubStr
    , toLower
    , endsInNumbers
    , findNumberIdx
    , skipFirstChars
    , replaceSubStr
    , doubleBackslashes
    , allLowercase
    , allUppercase
    , joinStringsWithCommas
    , joinStringsWithUnderscores
    ) where
import qualified Data.Char as DC
import Tools.LogicTools
-- import Tools.ListTools


------------------------------------------------------------------------------------------------------------------------
--                                        Check If String Contains Substring
------------------------------------------------------------------------------------------------------------------------
findSubStrIdx :: String -> String -> Integer -> Maybe Integer
findSubStrIdx "" _ _ = Nothing
findSubStrIdx s target n
    | take (length target) s == target      = Just n
    | otherwise                             = findSubStrIdx (tail s) target (n + 1)


containsSubStr :: String -> String -> Bool
containsSubStr s t
    | (findSubStrIdx s t 0) == Nothing      = False
    | otherwise                             = True


------------------------------------------------------------------------------------------------------------------------
--                                Check If a String Is All Uppercase or All Lowercase
--
-- To say that a string is all lowercase, really what we mean is a lack of uppercase. The reason is that numbers
-- and special symbols should not make the whole string return false. Therefore in these functions, we invert the
-- set for which we test membership (check if any characters belong to the oppose set), invert individual results,
-- and then AND those results together.
--
------------------------------------------------------------------------------------------------------------------------
lowercaseChars :: String 
lowercaseChars = ['a'..'z']


uppercaseChars :: String 
uppercaseChars = ['A'..'Z']


allLowercase :: String -> Bool 
allLowercase s = logicalAnd [not p | p <- [elem c uppercaseChars | c <- s]]


allUppercase :: String -> Bool 
allUppercase s = logicalAnd [not p | p <- [elem c lowercaseChars | c <- s]]


------------------------------------------------------------------------------------------------------------------------
--                                    Convert a String to Uppercase or Lowercase
------------------------------------------------------------------------------------------------------------------------
toLower :: String -> String
toLower s = map DC.toLower s


toUpper :: String -> String 
toUpper s = map DC.toUpper s


------------------------------------------------------------------------------------------------------------------------
--                                     Take The Last N Characters from A String
------------------------------------------------------------------------------------------------------------------------
takeLast :: Int -> String -> String
takeLast n s = (reverse (take n (reverse s)))


------------------------------------------------------------------------------------------------------------------------
--                             Check That The Last N Characters Of A String Are Numbers
------------------------------------------------------------------------------------------------------------------------
endsInNumbers :: Int -> String -> Bool
endsInNumbers n s = logicalAnd boolList where
    boolList = map (\x -> elem x ['0'..'9']) (takeLast n s)


------------------------------------------------------------------------------------------------------------------------
--                                            Find Index of First Number
--
-- But only when a string ends in numbers.
------------------------------------------------------------------------------------------------------------------------
findNumberGuts :: String -> Int -> Int
findNumberGuts s n 
    | (n == length s) = 0
    | (endsInNumbers n s) = findNumberGuts s (n + 1)
    | otherwise = n

findNumberIdx :: String -> Int
findNumberIdx s = (length s) - (findNumberGuts s 1) + 1


------------------------------------------------------------------------------------------------------------------------
--                                      Skip The First N Characters of A String
------------------------------------------------------------------------------------------------------------------------
skipFirstChars :: String -> Int -> String 
skipFirstChars "" _ = ""
skipFirstChars s 0 = s
skipFirstChars s n = skipFirstChars (tail s) (n-1)

------------------------------------------------------------------------------------------------------------------------
--                                                Replace A Substring
--
-- Replaces first occurence only.
------------------------------------------------------------------------------------------------------------------------
replaceSubStr :: String -> String -> String -> String
replaceSubStr "" _ _ = ""
replaceSubStr _ "" _ = ""
replaceSubStr thing2Change thing2Delete thing2Insert
    | thing2Change == "" = ""
    | thing2Delete == "" = thing2Change
    | (take (length thing2Delete) thing2Change == thing2Delete) =
        thing2Insert ++ (skipFirstChars thing2Change (length thing2Delete))
    | otherwise = [head thing2Change] ++ (replaceSubStr (tail thing2Change) thing2Delete thing2Insert)


------------------------------------------------------------------------------------------------------------------------
--                                  Replace Single Backslash In String With Double
--
-- Useful when you need to feed backslashes to the OS, and you don't want Mr. OS interpreting them as special characters.
------------------------------------------------------------------------------------------------------------------------
doubleBackslashes :: String -> String
doubleBackslashes "" = ""
doubleBackslashes s
    | (head s == '\\') = ['\\', '\\'] ++ (doubleBackslashes (tail s))
    | otherwise = [head s] ++ (doubleBackslashes (tail s))


------------------------------------------------------------------------------------------------------------------------
--                                             Join Strings With Commas
------------------------------------------------------------------------------------------------------------------------
joinStringsWithCommas :: [String] -> String
joinStringsWithCommas [] = ""
joinStringsWithCommas los
    | ((length los) == 1) = head los
    | otherwise = (head los) ++ ", " ++ (joinStringsWithCommas (tail los))


------------------------------------------------------------------------------------------------------------------------
--                                           Join Strings With Underscores
------------------------------------------------------------------------------------------------------------------------
joinStringsWithUnderscores :: [String] -> String 
joinStringsWithUnderscores [] = ""
joinStringsWithUnderscores los
    | ((length los) == 1) = head los
    | otherwise = (head los) ++ "_" ++ (joinStringsWithUnderscores (tail los))

