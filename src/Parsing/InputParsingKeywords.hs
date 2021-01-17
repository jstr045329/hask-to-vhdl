module Parsing.InputParsingKeywords where 

------------------------------------------------------------------------------------------------------------------------
--                                   Represent Keywords Relevant To Input Parsing 
--
-- This type represents keywords that must be understood in order to scan a list of strings for inputs. 
-- This type intentionally does not represent keywords such as "begin", "end function", or other keywords
-- that are irrelevant to locating inputs.
--
------------------------------------------------------------------------------------------------------------------------

-- NOTE: This datatype can probably be eliminated from SourceSinkParser.
-- I DO kinda like the idea of this datatype though. 
-- It might be more useful if you put [String] on the end of all of the constructors except IP_NoKeyword. 

-- TODO: See if the use of this datatype can be eliminated from this file. It's not providing any information at present.
data InputParsingKeywords = 
        IP_If InputParsingKeywords
    |   IP_Case InputParsingKeywords
    |   IP_Elsif InputParsingKeywords
    |   IP_OpenParen InputParsingKeywords
    |   IP_CloseParen InputParsingKeywords
    |   IP_Abs InputParsingKeywords
    |   IP_Downto InputParsingKeywords
    |   IP_To InputParsingKeywords
    |   IP_For InputParsingKeywords
    |   IP_Loop InputParsingKeywords
    |   IP_Generate InputParsingKeywords
    |   IP_Integer InputParsingKeywords
    |   IP_ToInteger InputParsingKeywords
    |   IP_NoKeyword -- Need something to show bottom of stack. 
    deriving (Eq, Show)

