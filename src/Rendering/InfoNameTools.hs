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
--                                                 Parse An Integer
------------------------------------------------------------------------------------------------------------------------
readInt :: String -> Integer 
readInt s = read s 


readInt' :: String -> Int
readInt' s = (read s) :: Int

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
--
-- extractMiddle is the brains, but you're probably better off calling extractNameStub.
--
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


------------------------------------------------------------------------------------------------------------------------
--                                        Check If Name Ends With Two Numbers
--
-- This function returns True if a signal or port is in this format: s_someNameStub_0000_0000. False otherwise. The
-- two numbders at the end do not need to be 4 digits, but they do need to be free of letters and special characters
-- that aren't underscores. While you can technically sprinkle underscores freely into the number regions, doing
-- so will make those separate numbers as far as this function is concerned. Technically, this function returns True
-- if a name ends with 2 or more numbers, so additional numbers will not throw it off. For example, s_someNameStub_1_2_3
-- will return True. s_someNameStub_46 will return False, as will s_hillary.
--
------------------------------------------------------------------------------------------------------------------------
nameEndsWithTwoNumbers :: String -> Bool
nameEndsWithTwoNumbers "" = False
nameEndsWithTwoNumbers s
    | ((length tokList) < 3) = False
    | otherwise = ((allCharsAreNumbers secondToLast) && (allCharsAreNumbers lastLast)) where 
    tokList = splitName s
    listLen = length tokList
    secondToLast = tokList !! (listLen-2)
    lastLast = tokList !! (listLen-1)


------------------------------------------------------------------------------------------------------------------------
--                                       Extract Last Two Numbers From A Name
--
-- This convention is used to indicate (signal_number, layer_number) in trees of tuples, which in turn are useful
-- for pipelining functions.
--
------------------------------------------------------------------------------------------------------------------------
extractLastTwoNumbers :: String -> Maybe (Int, Int)
extractLastTwoNumbers s
    | (not (nameEndsWithTwoNumbers s)) = Nothing
    | otherwise = Just (x0, x1) where
        tokList = splitName s
        listLen = length tokList
        x0 = readInt' (tokList !! (listLen-2))
        x1 = readInt' (tokList !! (listLen-1))


------------------------------------------------------------------------------------------------------------------------
--                                         Convert Input Name to Signal Name
--
-- If a port name starts with the prefix i_ or o_, this function replaces the prefix with the signal prefix s_. If
-- the name has no such prefix, this function simply prepends it with s_. In all cases, the signal suffix is appended
-- at the end. Note this is a lossy convertion so if you want to convert a signal to a port, you need to know Input or 
-- Output. 
------------------------------------------------------------------------------------------------------------------------
namesToLeaveAlone :: [String]
namesToLeaveAlone = ["clk", "clk_p", "clk_n", "rst", "rst_p", "rst_n"]


portName2SigName :: String -> Integer -> String 
portName2SigName s n
    | ((take 2 s) == "i_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | ((take 2 s) == "o_") = "s_" ++ (tail (tail s)) ++ (signalSuffix n)
    | (elem s namesToLeaveAlone) = s -- TODO: Try moving this branch to top precedence and see if that breaks anything.
    | otherwise = "s_" ++ s ++ (signalSuffix n)


------------------------------------------------------------------------------------------------------------------------
--                                         Convert Signal Name To Input Name
------------------------------------------------------------------------------------------------------------------------
sigName2InputName :: String -> String 
sigName2InputName s = "i_" ++ (extractNameStub s)


------------------------------------------------------------------------------------------------------------------------
--                                        Convert Signal Name to Output Name
------------------------------------------------------------------------------------------------------------------------
sigName2OutputName :: String -> String 
sigName2OutputName s = "o_" ++ (extractNameStub s)

