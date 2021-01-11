------------------------------------------------------------------------------------------------------------------------
--                          This Module Represents The State Of The Rapid Project Generator 
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.GeneratorState where 
import Rendering.Entity
import Rendering.InfoTypes
import Rendering.ProjectParameters
import Rendering.EntityTree
import Rendering.InterspersedCode
import Rendering.Process


data GeneratorState = GeneratorState {
        showVhd :: Bool 
    ,   showHs :: Bool 
    ,   formingEntity :: Bool 
    ,   useConcurrent :: Bool
    ,   useSequential :: Bool
    ,   codeLines :: [InterspersedCode]
    ,   enableAutoPortMap :: Bool
    ,   projectParameters :: ProjectParameters
    ,   pathToPresent :: [String]
    ,   entTree :: EntityTree
    ,   defaultClk :: Information
    ,   defaultRst :: Information
    ,   defaultDataType :: DataType
    ,   defaultWidth :: Width
    ,   viewVhd :: Bool
    ,   viewHs :: Bool
    ,   drinkProcess :: Bool
    ,   processUnderConstruction :: [Process]
    } deriving (Eq, Show)


defaultGeneratorState :: GeneratorState 
defaultGeneratorState = GeneratorState {
        showVhd = True
    ,   showHs = False
    ,   formingEntity = False
    ,   useConcurrent = True
    ,   useSequential = False 
    ,   enableAutoPortMap = False
    ,   projectParameters = easyProjParams
    ,   pathToPresent = ["Top"] -- ["Unnamed"]
    ,   entTree = EntityTree (defaultEntity { entNomen = "Top" }) []
    ,   defaultClk = easyClk
    ,   defaultRst = easyRst
    ,   defaultDataType = StdLogic
    ,   defaultWidth = Hard 1
    ,   viewVhd = True
    ,   viewHs = False
    ,   drinkProcess = False
    ,   processUnderConstruction = []
    }


