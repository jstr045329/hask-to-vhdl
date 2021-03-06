------------------------------------------------------------------------------------------------------------------------
--                                               Identify VHDL Tokens 
--
-- Return true if a string is a VHDL operator. Return False otherwise. 
--
------------------------------------------------------------------------------------------------------------------------
module Parsing.VhdlTokens where
import qualified Data.HashSet as HashSet
import Parsing.GuaranteeWhitespace


isVhdlToken :: String -> Bool
isVhdlToken s = elem s (HashSet.fromList (twoCharTokens ++ oneCharTokens))


