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


------------------------------------------------------------------------------------------------------------------------
--                                        Struct To Represent Generator State 
------------------------------------------------------------------------------------------------------------------------
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
    ,   renderedCodeStartLoc :: Int
    ,   userMessages :: [(String, Int)]
    } deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                         Define A Default Generator State 
------------------------------------------------------------------------------------------------------------------------
defaultGeneratorState :: GeneratorState 
defaultGeneratorState = GeneratorState {
        showVhd = True
    ,   showHs = False
    ,   formingEntity = False
    ,   useConcurrent = True
    ,   useSequential = False 
    ,   codeLines = []
    ,   enableAutoPortMap = False
    ,   projectParameters = easyProjParams
    ,   pathToPresent = ["Top"]
    ,   entTree = EntityTree (defaultEntity { entNomen = "Top" }) []
    ,   defaultClk = easyClk
    ,   defaultRst = easyRst
    ,   defaultDataType = StdLogic
    ,   defaultWidth = Hard 1
    ,   viewVhd = True
    ,   viewHs = False
    ,   drinkProcess = False
    ,   processUnderConstruction = [defaultProcess]
    ,   renderedCodeStartLoc = 0
    ,   userMessages = []
    }


------------------------------------------------------------------------------------------------------------------------
--                                      Get Present Entity From Generator State 
--
-- This function returns the name of the entity that the user is presently working on. 
--
------------------------------------------------------------------------------------------------------------------------
gPEnt :: GeneratorState -> String
gPEnt gS = last (pathToPresent gS)


------------------------------------------------------------------------------------------------------------------------
--                                                Purge User Messages 
--
-- This function deletes user messages after they age out. 
--
------------------------------------------------------------------------------------------------------------------------
usrMsgAgeLimit :: Int
usrMsgAgeLimit = 1


-- This function increments age of any 
iterateOverUsrMessages :: [(String, Int)] -> [(String, Int)]
iterateOverUsrMessages [] = []
iterateOverUsrMessages someList
    | (a >= usrMsgAgeLimit) = iterateOverUsrMessages (tail someList) 
    | otherwise = [(s, a+1)] ++ (iterateOverUsrMessages (tail someList)) where
        (s, a) = head someList


purgeUserMessages :: GeneratorState -> GeneratorState
purgeUserMessages gS = gS {userMessages = iterateOverUsrMessages (userMessages gS)}




