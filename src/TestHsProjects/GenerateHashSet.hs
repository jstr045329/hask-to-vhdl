module TestHsProjects.GenerateHashSet where
import Rendering.InfoTypes
import Text.Printf


hashSetFormatStr :: String
hashSetFormatStr = "%06d"


makeRegisters :: Int -> [Information]
makeRegisters n 
    | (n < 0) = []
    | otherwise =
        [VhdSig {
                nomen = thisSignalName
            ,   dataType = UnconstrainedInt
            ,   width = Hard 32
            ,   sDefault = Specified "0"
            ,   sReset = "0"
            ,   clocked = Just True
            ,   comments = []
            ,   assertionLevel = Nothing
            }] ++ (makeRegisters (n-1)) where 
        nameStr = "s_num_" ++ hashSetFormatStr
        thisSignalName = printf nameStr (n-1)


makeOccupiedRegisters :: Int -> [Information]
makeOccupiedRegisters n
    | (n < 0) = []
    | otherwise = 
        [VhdSig {
                nomen = thisSignalName
            ,   dataType = StdLogic
            ,   width = Hard 1
            ,   sDefault = Specified "'0'"
            ,   sReset = "'0'"
            ,   clocked = Just True
            ,   comments = []
            ,   assertionLevel = Just "'1'"
            }] ++ (makeOccupiedRegisters (n-1)) where 
        nameStr = "s_occupied_" ++ hashSetFormatStr
        thisSignalName = printf nameStr (n-1)

-- Draw picture of circuit

-- Drive registers with demux

-- 
