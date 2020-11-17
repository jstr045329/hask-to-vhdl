module Rendering.Condition where
import Rendering.Percentage
import Rendering.InfoTypes
import Rendering.Assignment


------------------------------------------------------------------------------------------------------------------------
--                                               Represent Conditions 
------------------------------------------------------------------------------------------------------------------------
data Condition =    
                        -- Comparisons of Magnitude:
                        GreaterT Information Information
                    |   LesserT Information Information
                    |   EqualTo Information Information
                    |   GreaterTEq Information Information
                    |   LessTEq Information Information

                        -- Logical Operations:
                    |   StdLogicHigh Information
                    |   StdLogicLow Information
                    |   Not Condition
                    |   JustInfo Information -- Like a buffer. Allows Information to be wrapped in the Condition type.
                    |   And [Condition]
                    |   Or [Condition]
                    |   Xor [Condition]
                    |   Nand [Condition]
                    |   Nor [Condition]
                    |   Xnor [Condition]
                        deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                                 Wrap An Operator 
--
-- Inserts an infix operator between 2 signal names. 
--
------------------------------------------------------------------------------------------------------------------------
wrapOperator :: Information -> Information -> String -> String
wrapOperator i1 i2 op = (nomen i1) ++ " " ++ op ++ " " ++ (nomen i2)


------------------------------------------------------------------------------------------------------------------------
--                                                  Combine Helper 
-- Handles the recursive part of combining conditions. Also handles the bool2SL calls. 
------------------------------------------------------------------------------------------------------------------------
combineHelper :: [Condition] -> String -> String
combineHelper [] _ = ""
combineHelper cList gate
    | length cList == 1     = "bool2SL(" ++ (cond2Str (head cList)) ++ ")"
    | otherwise             = "(bool2SL(" ++ (cond2Str (head cList)) ++ ") " ++ gate ++ " " ++ (combineHelper (tail cList) gate) ++ ")"


------------------------------------------------------------------------------------------------------------------------
--                                                Combine Conditions 
--
-- Provides a wrapper with easier to remember name for the above logic. Also handles sl2Bool calls. Last argument 
-- specifies whether to invert the output. 
--
------------------------------------------------------------------------------------------------------------------------
combineConditions :: [Condition] -> String -> Bool -> String
combineConditions [] _ _ = ""
combineConditions cList gate False = "sl2Bool" ++ (combineHelper cList gate)
combineConditions cList gate True = "sl2Bool(not " ++ (combineHelper cList gate) ++ ")"


------------------------------------------------------------------------------------------------------------------------
--                                      Convert List of Conditions to a String 
--
-- The following functions are intended for combining a small enough number of conditions that the resulting expression 
-- can realistically happen within 1 clock cycle. If you need to combine a greater number of conditions, and therefore 
-- pipelining, use RegisteredGates.hs. 
--
------------------------------------------------------------------------------------------------------------------------
andConditions :: [Condition] -> String
andConditions cList = combineConditions cList "and" False


orConditions :: [Condition] -> String
orConditions cList = combineConditions cList "or" False 


nandConditions :: [Condition] -> String
nandConditions cList = combineConditions cList "and" True


norConditions :: [Condition] -> String
norConditions cList = combineConditions cList "or" True 


-- Parentheses are inserted into VHDL to affect right associativity
xorConditions :: [Condition] -> String
xorConditions cList = combineConditions cList "xor" False


-- Parentheses are inserted into VHDL to affect right associativity
xnorConditions :: [Condition] -> String
xnorConditions cList = combineConditions cList "xor" True


notCondition :: Condition -> String
notCondition c = "not " ++ cond2Str c


----------------------------------------------------------------------------------------------------
--                                      Render Conditions
--
-- Typically, user should want to call these functions. (As opposed to all the other **** above.)
-- Technically, there are several entry points you could use in the file that would work equally 
-- well. But if you make other Haskell files call cond2Str, it fits well with the type system. 
----------------------------------------------------------------------------------------------------
cond2Str :: Condition -> String

-- Comparisons of Magnitude:
cond2Str (GreaterT i1 i2) = wrapOperator i1 i2 ">"
cond2Str (LesserT i1 i2) = wrapOperator i1 i2 "<" 
cond2Str (EqualTo i1 i2) = wrapOperator i1 i2 "=" 
cond2Str (GreaterTEq i1 i2) = wrapOperator i1 i2 ">=" 
cond2Str (LessTEq i1 i2) = wrapOperator i1 i2 "<=" 

-- Logical Operations:
cond2Str (Not c) = "sl2Bool(not(bool2SL(" ++ (cond2Str c) ++ ")))"
cond2Str (JustInfo i) = nomen i
cond2Str (And cList) = andConditions cList
cond2Str (Or cList) = orConditions cList
cond2Str (Xor cList) = xorConditions cList
cond2Str (Nand cList) = nandConditions cList
cond2Str (Nor cList) = norConditions cList
cond2Str (Xnor cList) = xnorConditions cList
cond2Str (StdLogicHigh someSig) = (nomen someSig) ++ " = '1'"
cond2Str (StdLogicLow someSig) = (nomen someSig) ++ " = '0'"

