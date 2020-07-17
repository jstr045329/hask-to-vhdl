module Rendering.VhdMath where
import Rendering.InfoTypes
import Rendering.Percentage
import Rendering.Condition
import Rendering.Assignment

-- TODO: Add functions that automatically convert std_logic_vector 
-- to signed/unsigned for arithmetic, then convert the result back again.
-- They should make sure the appropriate libraries are included.

-- TODO: Add functions that automatically convert std_logic_vector
-- to signed/unsigned for comparisons, then convert the result to std_logic.


liftInfo :: Condition -> Information
liftInfo (JustInfo i) = i
liftInfo _ = error "Only pass wrapped Information into this function"


wrapOperator :: Information -> Information -> String -> String
wrapOperator i1 i2 op = "(" ++ (nomen i1) ++ " " ++ op ++ " " ++ (nomen i2) ++ ")"


combineConditions :: [Condition] -> String -> String
combineConditions [] _ = ""
combineConditions cList gate
    | length cList == 1     = cond2Str (head cList)
    | otherwise             = cond2Str (head cList) ++ " " ++ gate ++ " " ++ combineConditions (tail cList) gate

-- The following functions are intended for combining a small enough number of conditions
-- that the resulting expression can realistically happen within 1 clock cycle. 
-- If you need to combine a greater number of conditions, and therefore pipelining, 
-- use RegisteredGates.hs.
andConditions :: [Condition] -> String
andConditions cList = combineConditions cList "and"


orConditions :: [Condition] -> String
orConditions cList = combineConditions cList "or"


nandConditions :: [Condition] -> String
nandConditions cList = combineConditions cList "nand"


norConditions :: [Condition] -> String
norConditions cList = combineConditions cList "nor"


xorConditions :: [Condition] -> String
xorConditions cList = combineConditions cList "xor"


xnorConditions :: [Condition] -> String
xnorConditions cList = combineConditions cList "xnor"


notCondition :: Condition -> String
notCondition c = "not " ++ cond2Str c


----------------------------------------------------------------------------------------------------
--                                      Render Conditions
----------------------------------------------------------------------------------------------------
cond2Str :: Condition -> String

-- Comparisons of Magnitude:
cond2Str (GreaterT i1 i2) = wrapOperator i1 i2 ">"
cond2Str (LesserT i1 i2) = wrapOperator i1 i2 "<" 
cond2Str (EqualTo i1 i2) = wrapOperator i1 i2 "=" 
cond2Str (GreaterTEq i1 i2) = wrapOperator i1 i2 ">=" 
cond2Str (LessTEq i1 i2) = wrapOperator i1 i2 "<=" 


-- Logical Operations:
cond2Str (Not c) = "not " ++ (cond2Str c)
cond2Str (JustInfo i) = nomen i
cond2Str (And cList) = andConditions cList
cond2Str (Or cList) = orConditions cList
cond2Str (Xor cList) = xorConditions cList
cond2Str (Nand cList) = nandConditions cList
cond2Str (Nor cList) = norConditions cList
cond2Str (Xnor cList) = xnorConditions cList
cond2Str JustTrue = error "This should never happen"

-- Recursive Conditions:
cond2Str (Princess cList) -- The Princess has a list of demands, which she ANDs together
    | cList == [] = ""
    | length cList == 1 = cond2Str (head cList)
    | otherwise = (cond2Str (head cList)) ++ " and " ++ cond2Str (Princess (tail cList))

cond2Str (Pauper cList) -- Beggars can't be choosers. This unfortunate soul ORs conditions together. 
    | cList == [] = ""
    | length cList == 1 = cond2Str (head cList)
    | otherwise = (cond2Str (head cList)) ++ " or " ++ cond2Str (Pauper (tail cList))


-- TODO: It would be really handy to enable something like for loops that distribute computation over time.
-- But they are rendered as an FSM. That way, synthesis would be reliable. 

-- TODO: Write a library that mimics for loops, with one type representing the loop distributed over time,
-- and another type representing loop distributed across space. The former renders to an FSM; the latter
-- to a generate statement, function, or set of pipelined functions. 


