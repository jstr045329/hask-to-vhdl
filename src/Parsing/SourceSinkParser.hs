------------------------------------------------------------------------------------------------------------------------
--                                                Source Sink Parser 
--
-- This module parses VHDL literal code (raw VHDL typed into the TUI) and figures out which names are outputs, which 
-- are inputs, and which are signals. 
--
-- For this module, synthesizable code is always the first priority. 
--
------------------------------------------------------------------------------------------------------------------------
module Parsing.SourceSinkParser (
        InfoPack
    ,   blankInfoPack
    ,   sigNames
    ,   inputNames
    ,   outputNames
    ,   internalState
    ,   constantNames
    ,   varNames
    ,   uniteInfoPacks
    ,   parseVhd
    ,   finalizePorts
    ) where
import Rendering.InfoTypes
import Parsing.TokenMatchingTools
import qualified Data.HashSet as HashSet
import Parsing.GuaranteeWhitespace
import Parsing.NumberRecognition
import Parsing.VhdlKeywords
import Parsing.VhdlTokens
import Parsing.PortExtractor
import Parsing.ScrapeInputs
import Parsing.InputParsingKeywords
import Parsing.ScrapeOutputs


data InfoPack = InfoPack {
        sigNames :: HashSet.HashSet String
    ,   inputNames :: HashSet.HashSet String
    ,   outputNames :: HashSet.HashSet String
    ,   internalState :: HashSet.HashSet String
    ,   constantNames :: HashSet.HashSet String
    ,   varNames :: HashSet.HashSet String
    } deriving (Eq, Show)


uniteInfoPacks :: InfoPack -> InfoPack -> InfoPack
uniteInfoPacks a b = InfoPack {
        sigNames = HashSet.union (sigNames a) (sigNames b)
    ,   inputNames = HashSet.union (inputNames a) (inputNames b)
    ,   outputNames = HashSet.union (outputNames a) (outputNames b)
    ,   internalState = HashSet.union (internalState a) (internalState b)
    ,   constantNames = HashSet.union (constantNames a) (constantNames b)
    ,   varNames = HashSet.union (varNames a) (varNames b)
    }


blankInfoPack :: InfoPack
blankInfoPack = InfoPack {
        sigNames = HashSet.fromList []
    ,   inputNames = HashSet.fromList []
    ,   outputNames = HashSet.fromList []
    ,   internalState = HashSet.fromList []
    ,   constantNames = HashSet.fromList []
    ,   varNames = HashSet.fromList []
    }


------------------------------------------------------------------------------------------------------------------------
--                                        Find First Non-Keyword, Non-Number 
--
-- Since we're scanning for inputs in this module, this function also filters out operators.
--
------------------------------------------------------------------------------------------------------------------------
takeFirstNonKeywordNonNumber :: [String] -> [String]
takeFirstNonKeywordNonNumber [] = []
takeFirstNonKeywordNonNumber los
    | (isBinLiteral (head los)) = takeFirstNonKeywordNonNumber (tail los)
    | (isHexLiteral (head los)) = takeFirstNonKeywordNonNumber (tail los)
    | (isIntLiteral (head los)) = takeFirstNonKeywordNonNumber (tail los)
    | (isVhdlKeyword (head los)) = takeFirstNonKeywordNonNumber (tail los)
    | (isVhdlToken (head los)) = takeFirstNonKeywordNonNumber (tail los)
    | otherwise = [head los]


------------------------------------------------------------------------------------------------------------------------
--                                          Handle Contents of Parentheses 
--
-- Focus of this function is correctly handling the inside of a parenthesis, when called from extract SIGNALS from 
-- string.
--
-- At this time, this function does not worry about situations like this:
--
--          ram_output <= my_ram(to_integer(addr(37 downto 16)));
--
-- While the language does allow that, such a use case is highly unlikely in practice.
--
-- This function does, however, support these:
--
--          ram_output <= my_ram(addr);
--
--          ram_output <= my_ram(to_integer(addr));
--
------------------------------------------------------------------------------------------------------------------------
takeFirstNonNumberStopAtParen :: [String] -> [String]
takeFirstNonNumberStopAtParen [] = []
takeFirstNonNumberStopAtParen los
    | ((head los) == ")") = []
    | (((length los) > 1) && ((los !! 1) == "to")) = []
    | (((length los) > 1) && ((los !! 1) == "downto")) = []
    | (isVhdlKeyword (head los)) = takeFirstNonNumberStopAtParen (tail los)
    | (isVhdlToken (head los)) = takeFirstNonNumberStopAtParen (tail los)
    | otherwise = [head los]


------------------------------------------------------------------------------------------------------------------------
--                                       Extract Signal Names From One String 
--
-- This function takes a list of strings, along with the keywords that came before, and extracts the signal names. 
--
------------------------------------------------------------------------------------------------------------------------
extractSignalsFromString :: [String] -> InputParsingKeywords -> [String]
extractSignalsFromString los (IP_If _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los (IP_Elsif _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los (IP_Case _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los (IP_OpenParen _) = [] 
extractSignalsFromString los (IP_CloseParen _) = []
extractSignalsFromString los (IP_Abs _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los (IP_Downto _) = [] -- NOTE: Since focus is on synthesizable VHDL, we do not recognize signals as part of a downto statement. 
extractSignalsFromString los (IP_To _) = [] -- NOTE: Since focus is on synthesizable VHDL, we do not recognize signals as part of a to statement. 
extractSignalsFromString los (IP_Integer _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los (IP_ToInteger _) = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los IP_NoKeyword = takeFirstNonKeywordNonNumber (stopAtClosingParen los 0 0)
extractSignalsFromString los _ = []


------------------------------------------------------------------------------------------------------------------------
--                                                  Extract Outputs 
--
-- This function isolates Information's that are on the left of the assignment operator. 
--
------------------------------------------------------------------------------------------------------------------------
--noAssignmentOperator :: [String] -> Bool
--noAssignmentOperator [] = True
--noAssignmentOperator los = not ((elem ":=" los) || (elem "<=" los))


--isolateOutputs :: [String] -> [String]
--isolateOutputs los
--    | ((length los) < 2) = []
--    | (noAssignmentOperator los) = []
--    | otherwise = untilVhdlKeyword (untilVhdlOperator los)


------------------------------------------------------------------------------------------------------------------------
--                                                  Parse One Line 
--
-- NOTE: Do NOT use this function on subprograms! unless you're past all function f(x : std_logic...)...begin... 
-- stuff, and before the end function stuff. 
--
------------------------------------------------------------------------------------------------------------------------
parseVhd :: [String] -> InputParsingKeywords -> InfoPack
parseVhd [] _ = blankInfoPack
parseVhd los pastKeywords
    | (isBinLiteral (head los)) = parseVhd (tail los) pastKeywords
    | (isHexLiteral (head los)) = parseVhd (tail los) pastKeywords
    | (isIntLiteral (head los)) = parseVhd (tail los) pastKeywords
    | ((head los) == "(") = parseVhd (tail los) (IP_OpenParen pastKeywords)
    | ((head los) == ")") = parseVhd (tail los) (IP_CloseParen pastKeywords)
    | ((head los) == "if") = parseVhd (tail los) (IP_If pastKeywords)
    | ((head los) == "elsif") = parseVhd (tail los) (IP_Elsif pastKeywords)
    | ((head los) == "abs") = parseVhd (tail los) (IP_Abs pastKeywords)
    | ((head los) == "to") = parseVhd (tail los) (IP_To pastKeywords)
    | ((head los) == "downto") = parseVhd (tail los) (IP_Downto pastKeywords)
    | ((head los) == "for") = parseVhd (tail los) (IP_For pastKeywords)
    | ((head los) == "generate") = parseVhd (tail los) (IP_Generate pastKeywords)
    | ((head los) == "loop") = parseVhd (tail los) (IP_Loop pastKeywords)
    | ((head los) == "case") = parseVhd (tail los) (IP_Case pastKeywords)
    | ((head los) == "abs") = parseVhd (tail los) (IP_Abs pastKeywords)
    | ((head los) == "integer") = parseVhd (tail los) (IP_Integer pastKeywords)
    | ((head los) == "to_integer") = parseVhd (tail los) (IP_ToInteger pastKeywords)
    | (isVhdlKeyword (head los)) = parseVhd (tail los) pastKeywords
    | (elem (head los) (HashSet.fromList (twoCharTokens ++ oneCharTokens))) = parseVhd (tail los) pastKeywords
    | otherwise = 
        uniteInfoPacks
            (parseVhd (tail los) pastKeywords)
            InfoPack {
                -- NOTE: Everything in sigNames should have a name similar to a signal name, but sigNames does not
                -- by itself tell you what all your signal names should be. Additional algos are required to decide
                -- which names in sigNames should be declared directly as a signal, if any; which names need both 
                -- a signal variant and an output variant; and whether sigNames should be /= internalState.
                sigNames = HashSet.intersection myOutputs myInputs 
            ,   inputNames = myInputs
            ,   outputNames = myOutputs
            ,   internalState = HashSet.intersection myOutputs myInputs 
            ,   constantNames = HashSet.fromList []
            ,   varNames = HashSet.fromList []
            } where
                myInputs = HashSet.fromList (scrapeFormulaInputs los)
                myOutputs = HashSet.fromList (scrapeFormulaOutputs los)


------------------------------------------------------------------------------------------------------------------------
--                                                  Finalize Ports 
--
-- Perform set operations to ensure that input names do not also appear in output & vice versa.
-- Internal State should be the intersection of those two sets.
-- Right now signalNames = internalState, but that could change in the future.
--
------------------------------------------------------------------------------------------------------------------------
finalizePorts :: InfoPack -> InfoPack
finalizePorts iPack = iPack { 
        inputNames = HashSet.difference (inputNames iPack) (outputNames iPack) 
    ,   outputNames = HashSet.difference (outputNames iPack) (inputNames iPack)
    ,   sigNames = HashSet.intersection (inputNames iPack) (outputNames iPack)
    ,   Parsing.SourceSinkParser.internalState = HashSet.intersection (inputNames iPack) (outputNames iPack)
    } 


