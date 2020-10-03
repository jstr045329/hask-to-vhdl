module Rendering.InfoNameTools where 
import Data.List.Split
import Tools.LogicTools
import Text.Printf


------------------------------------------------------------------------------------------------------------------------
--                                Split an Information Name Into Its Component Parts
------------------------------------------------------------------------------------------------------------------------
splitName :: String -> [String]
splitName s = splitOn "_" s 


------------------------------------------------------------------------------------------------------------------------
--                                   Determine What An Information Is By Its Name
--
-- This function checks if the first token in a signal name (when split on underscores) matches target.
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

