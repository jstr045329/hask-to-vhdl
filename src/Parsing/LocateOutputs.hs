------------------------------------------------------------------------------------------------------------------------
--                                                  Locate Outputs 
--
-- This module contains tools for locating assignment targets in tokenized VHDL. 
--
-- NOTE: It is critical that whatever you feed into isAssignmentTarget is first cleansed by removeIfStatements.
--
------------------------------------------------------------------------------------------------------------------------
module Parsing.LocateOutputs (
        removeIfStatements
    ,   isAssignmentTarget
    ) where
import Data.HashSet
import Parsing.NumberRecognition
import Parsing.VhdlTokens
import Parsing.NumericStdFunctions
import Parsing.TokenMatchingTools


------------------------------------------------------------------------------------------------------------------------
--                                               Remove If Statements 
--
-- This function removes if, then, and everything in between. It's necessary to prevent the less than or equal to 
-- operator from being confused as an assignment. 
--
------------------------------------------------------------------------------------------------------------------------
removeIfStatements :: Bool -> [String] -> [String]
removeIfStatements _ [] = []
removeIfStatements withinIfStatement los
    | ((withinIfStatement == True) && ((head los) == "then")) = removeIfStatements False (tail los)
    | (withinIfStatement == True) = removeIfStatements True (tail los)
    | ((head los) == "if") = removeIfStatements True (tail los)
    | ((head los) == "elsif") = removeIfStatements True (tail los)
    | (((length los) > 2) && ((take 2 los) == ["end", "if", ";"])) = removeIfStatements False (tail (tail (tail los)))
    | (((length los) > 1) && ((take 2 los) == ["end", "if"])) = removeIfStatements False (tail (tail los))
    | otherwise = [head los] ++ (removeIfStatements False (tail los))


------------------------------------------------------------------------------------------------------------------------
--                                            Locate Closing Parenthesis 
--
-- This is a modified version of the function in PortExtractor.hs. Do not replace one with the other without testing 
-- it first. PortExtractor.hs throws an error if n < 0. 
--
------------------------------------------------------------------------------------------------------------------------
untilClosingParen :: [String] -> Int -> [String]
untilClosingParen [] _ = []
untilClosingParen los n
    | (n < 0) = []
    | ((head los) == ";") = []
    | ((head los) == "(") = [head los] ++ (untilClosingParen (tail los) (n + 1)) 
    | (((head los) == ")") && (n == 1)) = [head los]
    | ((head los) == ")") = [head los] ++ (untilClosingParen (tail los) (n - 1)) 
    | otherwise = [head los] ++ (untilClosingParen (tail los) n)


------------------------------------------------------------------------------------------------------------------------
--                                                Remove Parentheses 
--
-- It is possible for a less than or equal to operator to be embedded in a function call. Therefore, remove function 
-- arguments and their parentheses. 
--
------------------------------------------------------------------------------------------------------------------------
scrubParentheses :: [String] -> [String]
scrubParentheses [] = []
scrubParentheses los 
    | ((head los) == "(") = scrubParentheses (skipNTokens los numToksToSkip)
    | otherwise = [head los] ++ (scrubParentheses (tail los)) where
        numToksToSkip = length (untilClosingParen los 0)


scrubAllParentheses :: [String] -> [String]
scrubAllParentheses los
    | (elem "(" los) = scrubAllParentheses (scrubParentheses los)
    | otherwise = los


------------------------------------------------------------------------------------------------------------------------
--                                     Check If Head Of LOS Is Assignment Target 
--                                                          
--
-- This function returns True if los python3 py/prettyComments.py Check If Head Of LOS Is Assignment Target This 
-- function returns True if the first element in los is the target of an assignment. Returns False otherwise. 
--
------------------------------------------------------------------------------------------------------------------------
isAssignmentTarget :: [String] -> Bool
isAssignmentTarget los
    | ((length los) < 2) = False
    | (isVhdlToken (head los)) = False
    | (isVhdlNumber (head los)) = False
    | (isNumericStd (head los)) = False
    | ((take 3 (tail los)) == ["end", "if", ";"]) = False
    | ((take 2 (tail los)) == ["end", "if"]) = False
    | ((take 3 los) == ["end", "if", ";"]) = False
    | ((take 2 los) == ["end", "if"]) = False
    | ((los !! 1) == "(") = isAssignmentTarget ([head los] ++ (scrubAllParentheses (tail los)))
    | ((los !! 1) == "if") = isAssignmentTarget ([head los] ++ (removeIfStatements False (tail los)))
    | ((los !! 1) == "elsif") = isAssignmentTarget ([head los] ++ (removeIfStatements False (tail los)))
    | ((los !! 1) == "<=") = True
    | ((los !! 1) == ":=") = True
    | otherwise = False 


