module Rendering.Assignment where
import Rendering.InfoTypes


data Assignment =   Assignment Information Information 
                    deriving (Eq, Show)


assignmentOperator :: Information -> String
assignmentOperator (Port _ _ _ _ _ _ _ _ _) = " <= "
assignmentOperator (VhdSig _ _ _ _ _ _ _ _) = " <= "
assignmentOperator _ = " := "


getLeftSide :: Assignment -> Information
getLeftSide (Assignment x _) = x


getRightSide :: Assignment -> Information
getRightSide (Assignment _ x) = x


getAssignmentTarget :: Assignment -> Information
getAssignmentTarget a = getLeftSide a


getAssignmentSource :: Assignment -> Information
getAssignmentSource a = getRightSide a


assignment2Str :: Assignment -> String
assignment2Str (Assignment i1 i2) = (nomen i1) ++ (assignmentOperator i1) ++ (nomen i2) ++ ";"


assignBatch :: [Assignment] -> [String]
assignBatch aList = map assignment2Str aList

