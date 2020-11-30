module Rendering.RapidProjectGen.GeneratorState where 
import Rendering.Entity
import Rendering.InfoTypes
import Rendering.ProjectParameters


data GeneratorState = GeneratorState {
        keepGoing :: Bool
    ,   showVhd :: Bool 
    ,   showHs :: Bool 
    ,   formingEntity :: Bool 
    ,   useConcurrent :: Bool
    ,   useSequential :: Bool
    ,   drinkingVhd :: Bool 
    ,   drinkingHs :: Bool
    ,   enableAutoPortMap :: Bool
    ,   uniqueNameInt :: Integer
    ,   projectParameters :: ProjectParameters
    ,   childEntities :: [Entity]
    ,   presentEntity :: Entity
    ,   parentEntity :: [Entity]
    ,   parentState :: [GeneratorState]
    ,   defaultClk :: Information
    ,   defaultRst :: Information
    ,   defaultDataType :: DataType
    ,   defaultWidth :: Width
    } deriving (Eq, Show)


defaultGeneratorState :: GeneratorState 
defaultGeneratorState = GeneratorState {
        keepGoing = True
    ,   showVhd = True
    ,   showHs = False
    ,   formingEntity = False
    ,   useConcurrent = True
    ,   useSequential = False 
    ,   drinkingVhd = False 
    ,   drinkingHs = False
    ,   enableAutoPortMap = False
    ,   uniqueNameInt = 495137688
    ,   projectParameters = easyProjParams
    ,   childEntities = []
    ,   presentEntity = defaultEntity
    ,   parentEntity = [TopLevelEntity]
    ,   parentState = []
    ,   defaultClk = easyClk
    ,   defaultRst = easyRst
    ,   defaultDataType = StdLogic
    ,   defaultWidth = Hard 1
    }





