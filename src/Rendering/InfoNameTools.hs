module Rendering.InfoNameTools where 
import Data.List.Split
import Tools.LogicTools
import Text.Printf
import Tools.StringTools
import Tools.ListTools


------------------------------------------------------------------------------------------------------------------------
--                                Split an Information Name Into Its Component Parts
------------------------------------------------------------------------------------------------------------------------
splitName :: String -> [String]
splitName s = splitOn "_" s 


------------------------------------------------------------------------------------------------------------------------
--                                   Determine What An Information Is By Its Name
--
-- This function checks if the first token in a signal name (when split on underscores) matches target.
-- See InfoTypes.hs for tools that check what Information is by its type constructor.
------------------------------------------------------------------------------------------------------------------------
matchInfoPrefix :: String -> String -> Bool 
matchInfoPrefix someString target = ((head (splitName someString)) == target)


nameIsInput :: String -> Bool
nameIsInput s = matchInfoPrefix s "i"
    

nameIsOutput :: String -> Bool
nameIsOutput s = matchInfoPrefix s "o"


nameIsSignal :: String -> Bool
nameIsSignal s = matchInfoPrefix s "s"


nameIsVariable :: String -> Bool
nameIsVariable s = matchInfoPrefix s "v"


nameIsConstant :: String -> Bool 
nameIsConstant s = allUppercase s


------------------------------------------------------------------------------------------------------------------------
--                                     All Possible Prefix Letters In One String
--
-- If you add anything to this list, be sure to give it a descriptive name in the Determine What An Information Is...
-- section above.
--
------------------------------------------------------------------------------------------------------------------------
prefixLetters :: String 
prefixLetters = "iosv"


------------------------------------------------------------------------------------------------------------------------
--                                        Check If Name Has Any Valid Prefix
------------------------------------------------------------------------------------------------------------------------
nameHasPrefix :: String -> Bool
nameHasPrefix someName
    | ((length (splitName someName)) < 2) = False
    | otherwise = logicalOr boolList where
    boolList = map (\oneTarget -> matchInfoPrefix someName [oneTarget]) prefixLetters


------------------------------------------------------------------------------------------------------------------------
--                                         Define A Suffix For Signal Names
--
-- Suffix specifies delay relative to the input that drives it.
------------------------------------------------------------------------------------------------------------------------
signalSuffixFormatStr :: String 
signalSuffixFormatStr = "_%06d"


signalSuffix :: Integer -> String 
signalSuffix n = printf signalSuffixFormatStr n 


------------------------------------------------------------------------------------------------------------------------
--                                         Convert Input Name to Signal Name
--
-- If a port name starts with the prefix i_ or o_, this function replaces the prefix with the signal prefix s_. If
-- the name has no such prefix, this function simply prepends it with s_. In all cases, the signal suffix is appended
-- at the end.
------------------------------------------------------------------------------------------------------------------------
portName2SigName :: String -> Integer -> String 
portName2SigName s n
    | ((take 2 s) == "i_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | ((take 2 s) == "o_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | otherwise = "s_" ++ s ++ (signalSuffix n)


------------------------------------------------------------------------------------------------------------------------
--                                                 Parse An Integer
------------------------------------------------------------------------------------------------------------------------
readInt :: String -> Integer 
readInt s = read s 


------------------------------------------------------------------------------------------------------------------------
--                                          Test If Characters Are Numbers
------------------------------------------------------------------------------------------------------------------------
numberChars :: String 
numberChars = ['0'..'9']


charIsNum :: Char -> Bool
charIsNum c = elem c numberChars


allCharsAreNumbers :: String -> Bool 
allCharsAreNumbers s = logicalAnd ([charIsNum c | c <- s]) 


------------------------------------------------------------------------------------------------------------------------
--                                            Extract Number from String
------------------------------------------------------------------------------------------------------------------------
getNumberFromString :: String -> Maybe Integer
getNumberFromString s
    | (allCharsAreNumbers (last (splitName s))) = Just (readInt (last (splitName s)))
    | otherwise = Nothing


------------------------------------------------------------------------------------------------------------------------
--                                          Check If Name Ends In A Number
------------------------------------------------------------------------------------------------------------------------
nameEndsWithNumber :: String -> Bool
nameEndsWithNumber s
    | ((getNumberFromString s) == Nothing) = False
    | otherwise = True


------------------------------------------------------------------------------------------------------------------------
--                                 Extract Name Stub from Port, Signal, or Variable
------------------------------------------------------------------------------------------------------------------------
extractMiddle :: String -> String 
extractMiddle s
    | ((nameHasPrefix s) && (nameEndsWithNumber s)) = joinStringsWithUnderscores (tail (dropLast (splitName s)))
    | (nameHasPrefix s) = joinStringsWithUnderscores (tail (splitName s))
    | (nameEndsWithNumber s) = joinStringsWithUnderscores (dropLast (splitName s))
    | otherwise = s


extractNameStub :: String -> String
extractNameStub "" = ""
extractNameStub someName
    | ((length (splitName someName)) == 0) = ""
    | ((length (splitName someName)) == 1) = someName
    | otherwise = extractMiddle someName

