------------------------------------------------------------------------------------------------------------------------
--                                                 Recognize Numbers 
--
-- This module contains tools for identifying number literals in VHDL. 
-- At present, this module does not support octal numbers.
------------------------------------------------------------------------------------------------------------------------
module Parsing.NumberRecognition where


isBinLiteral :: String -> Bool
isBinLiteral s
    | ((head s) /= '"') = False
    | ((last s) /= '"') = False
    | otherwise = True


isHexLiteral :: String -> Bool
isHexLiteral s
    | ((length s) < 3) = False
    | ((head s) /= 'x') = False
    | ((s !! 1) /= '"') = False
    | ((last s) /= '"') = False
    | otherwise = True


isIntLiteral :: String -> Bool
isIntLiteral s
    | (elem (head s) "0123456789") = True
    | (((length s) > 1) && ((head s) == '-') && (elem (s !! 1) "0123456789")) = True
    | otherwise = False


isVhdlNumber :: String -> Bool
isVhdlNumber s = (isBinLiteral s) || (isHexLiteral s) || (isIntLiteral s)


