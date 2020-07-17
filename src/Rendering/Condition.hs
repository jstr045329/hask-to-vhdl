module Rendering.Condition where
import Rendering.Percentage
import Rendering.InfoTypes


----------------------------------------------------------------------------------------------------
--                       Provide a Type That Creates & Combines Conditions
----------------------------------------------------------------------------------------------------
data Condition = 
                    -- Comparisons of Magnitude:
                        JustTrue -- Always true. Useful for unconditional assignments
                    |   GreaterT Information Information 
                    |   LesserT Information Information
                    |   EqualTo Information Information
                    |   GreaterTEq Information Information
                    |   LessTEq Information Information
                    |   Princess [Condition] -- List of demands
                    |   Pauper [Condition] -- Takes whatever it can get

                    -- Logical Operations:
                    |   Not Condition
                    |   JustInfo Information -- Like a buffer. Allows Information to be wrapped in the Condition type.
                    |   And [Condition]
                    |   Or [Condition]
                    |   Xor [Condition]
                    |   Nand [Condition]
                    |   Nor [Condition]
                    |   Xnor [Condition]

                    -- Fuzzy Logic & Soft Operations
                    |   DemandAnyX [Condition]
                    |   DemandAtLeastX [Condition] Percentage
                    |   DemandAtMostX [Condition] Percentage
                    |   DemandBetweenXAndY [Condition] Percentage Percentage
                    |   DemandMinMXPlusB [Condition] Percentage Int
                    |   DemandMaxMXPlusB [Condition] Percentage Int
                        deriving (Eq, Show)


-- TODO: Write rendering functions for Fuzzy Logic & Soft Operation Constructors, above. 
-- TODO: Go through all functions that accept the Condition type, and handle Fuzzy Logic.


-- The rendering of all this can all build on the Count 1's module. Therefore, think about 
-- whether to split off the Condition datatype into its own module to avoid mutual imports.


