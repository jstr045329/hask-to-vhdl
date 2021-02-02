------------------------------------------------------------------------------------------------------------------------
--                                       Provide A GeneratorState for Testing 
--
-- This module provides a GeneratorState you can use for tests. It does NOT, by itself, test GeneratorState.
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.TestGeneratorState where 
import Rendering.RapidProjectGen.GeneratorState
import Rendering.Entity
import Rendering.EntityTree
import Rendering.Process
import Rendering.InfoTypes
import Data.HashSet


testGeneratorState001 = defaultGeneratorState {
    processUnderConstruction = [
        defaultProcess {
            procPlainLines = ["bob <= frederick nand roseanne;"]
        ,   procOutputSignals = fromList [
                    easySig "frederick" StdLogic (Hard 1) []
                ]
            }
        ]
    }

-- procPlainLines
-- easySig :: String -> DataType -> Width -> [String] -> Information

