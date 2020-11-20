module Rendering.Assignment where
import Rendering.InfoTypes
import Rendering.InfoNameTools


------------------------------------------------------------------------------------------------------------------------
--                                          The Fundamental Assignment Type
------------------------------------------------------------------------------------------------------------------------
data Assignment =   Assignment Information Information 
                    deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                    Choose the Appropriate Assignment Operator
------------------------------------------------------------------------------------------------------------------------
assignmentOperator :: Information -> String
assignmentOperator (Port _ _ _ _ _ _ _ _ _) = " <= "
assignmentOperator (VhdSig _ _ _ _ _ _ _ _) = " <= "
assignmentOperator _ = " := "


------------------------------------------------------------------------------------------------------------------------
--                                   Extract One of the Signals in The Assignment
------------------------------------------------------------------------------------------------------------------------
getLeftSide :: Assignment -> Information
getLeftSide (Assignment x _) = x


getRightSide :: Assignment -> Information
getRightSide (Assignment _ x) = x


getAssignmentTarget :: Assignment -> Information
getAssignmentTarget a = getLeftSide a


getAssignmentSource :: Assignment -> Information
getAssignmentSource a = getRightSide a


------------------------------------------------------------------------------------------------------------------------
--                                               Render One Assignment
------------------------------------------------------------------------------------------------------------------------
assignment2Str :: Assignment -> String
assignment2Str (Assignment i1 (Literal _ _ (Specified s) _)) = (nomen i1) ++ (assignmentOperator i1) ++ s ++ ";"
assignment2Str (Assignment i1 i2) = (nomen i1) ++ (assignmentOperator i1) ++ (nomen i2) ++ ";"


------------------------------------------------------------------------------------------------------------------------
--                                              Render Many Assignments
------------------------------------------------------------------------------------------------------------------------
assignBatch :: [Assignment] -> [String]
assignBatch aList = map assignment2Str aList


------------------------------------------------------------------------------------------------------------------------
--                                     Generate Signals For Infinite Delay Chain 
--
-- This function is useful for declaring the signals assigned by assignSignalChainWithInput (below); or any other 
-- time you might need the signal maps themselves. 
--
------------------------------------------------------------------------------------------------------------------------
generateSignalChainWithInput :: Information -> [Information]
generateSignalChainWithInput someInput = [port2SigN someInput n | n <- [0..]]


------------------------------------------------------------------------------------------------------------------------
--                                 Take Some Information and Drive A Signal With It
--
-- Takes some input and generates this pattern:
--      s_myInput_0000 <= i_myInput;
--      s_myInput_0001 <= s_myInput_0000;
--      s_myInput_0002 <= s_myInput_0001;
--      ...
-- but only generates an individual assignment (as opposed to a list).
------------------------------------------------------------------------------------------------------------------------
assignOneSignalInChain :: Information -> Integer -> Assignment
assignOneSignalInChain someInput 0 = Assignment (port2Sig someInput) someInput
assignOneSignalInChain someInput n = Assignment (port2SigN someInput n) (port2SigN someInput (n-1))


------------------------------------------------------------------------------------------------------------------------
--                              Generate an Infinite List of Signals Driven by An Input
--
-- You could also think of this as an infinite delay chain. 
-- When rendering a process, do something like this:
--      take whateverNumber (assignSignalChainWithInput someInput)
-- and assignSignalChainWithInput will give you as many delays as you need. 
--
-- This function takes some input port and creates an infinite list of assignments
-- having the same name stub. For example:
--      s_myInput_0000 <= i_myInput;
--      s_myInput_0001 <= s_myInput_0000;
--      s_myInput_0002 <= s_myInput_0001;
--      ...
-- See endlessSignals in InfiniteSigList.hs to generate the list of signals. This list is more useful 
-- for assigning the signals. 
--
-- To declare the signals that are used in this function, call generateSignalChainWithInput (above).
------------------------------------------------------------------------------------------------------------------------
assignSignalChainWithInput :: Information -> [Assignment]
assignSignalChainWithInput someInput = [assignOneSignalInChain someInput i | i <- [0..]]

