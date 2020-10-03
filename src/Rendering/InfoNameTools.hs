module Rendering.InfoNameTools where 
import Data.List.Split
import Tools.LogicTools
import Text.Printf


splitName :: String -> [String]
splitName s = splitOn "_" s 


nameIsInput :: String -> Bool
nameIsInput s
    | ((head (splitName s)) == "i") = True 
    | otherwise = False 
    

nameIsOutput :: String -> Bool
nameIsOutput s
    | ((head (splitName s)) == "o") = True 
    | otherwise = False 


nameIsSignal :: String -> Bool
nameIsSignal s
    | ((head (splitName s)) == "s") = True 
    | otherwise = False 


signalSuffixFormatStr :: String 
signalSuffixFormatStr = "_%06d"


signalSuffix :: Integer -> String 
signalSuffix n = printf signalSuffixFormatStr n 


-- If a port name starts with the prefix i_ or o_, this function replaces
-- the prefix with the signal prefix s_. If the name has no such prefix, 
-- this function simply prepends it with s_. In all cases, the signal suffix
-- is appended at the end. 
portName2SigName :: String -> Integer -> String 
portName2SigName s n
    | ((take 2 s) == "i_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | ((take 2 s) == "o_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | otherwise = "s_" ++ s ++ (signalSuffix n)


readInt :: String -> Integer 
readInt s = read s 


numberChars :: String 
numberChars = ['0'..'9']


charIsNum :: Char -> Bool
charIsNum c = elem c numberChars


allCharsAreNumbers :: String -> Bool 
allCharsAreNumbers s = logicalAnd ([charIsNum c | c <- s]) 


getNumberFromString :: String -> Maybe Integer
getNumberFromString s
    | (allCharsAreNumbers (last (splitName s))) = Just (readInt (last (splitName s)))
    | otherwise = Nothing




-- incrementSuffix :: String -> String 
 -- incrementSuffix s = 
    -- TODO: Flesh this out









