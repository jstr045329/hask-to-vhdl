------------------------------------------------------------------------------------------------------------------------
--                          This Module Represents The State Of The Rapid Project Generator 
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.GeneratorState where 
import Rendering.Entity
import Rendering.InfoTypes
import Rendering.ProjectParameters
import Rendering.EntityTree


data GeneratorState = GeneratorState {
        showVhd :: Bool 
    ,   showHs :: Bool 
    ,   formingEntity :: Bool 
    ,   useConcurrent :: Bool
    ,   useSequential :: Bool
    ,   drinkingVhd :: Bool 
    ,   drinkingHs :: Bool
    ,   enableAutoPortMap :: Bool
    ,   projectParameters :: ProjectParameters
    ,   pathToPresent :: [String]
    ,   entTree :: EntityTree
    ,   defaultClk :: Information
    ,   defaultRst :: Information
    ,   defaultDataType :: DataType
    ,   defaultWidth :: Width
    } deriving (Eq, Show)


defaultGeneratorState :: GeneratorState 
defaultGeneratorState = GeneratorState {
        showVhd = True
    ,   showHs = False
    ,   formingEntity = False
    ,   useConcurrent = True
    ,   useSequential = False 
    ,   drinkingVhd = False 
    ,   drinkingHs = False
    ,   enableAutoPortMap = False
    ,   projectParameters = easyProjParams
    ,   pathToPresent = ["Unnamed"]
    ,   entTree = EntityTree defaultEntity []
    ,   defaultClk = easyClk
    ,   defaultRst = easyRst
    ,   defaultDataType = StdLogic
    ,   defaultWidth = Hard 1
    }

-- TODO: Delete all unused fields in GeneratorState



