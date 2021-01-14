module Rendering.RapidProjectGen.DrillDown where 
import Rendering.RapidProjectGen.GeneratorState
import Rendering.Entity
import Rendering.EntityTree


drillDownOneLayer :: String -> GeneratorState -> GeneratorState
drillDownOneLayer someName gS
    | ((length matchingChild) > 0) = gS {pathToPresent = (pathToPresent gS) ++ [someName]}
    | otherwise = gS {userMessages = (userMessages gS) ++ [("Child Entity Not Found", 0)]} where
         startingPlace = fetchOneEntityTree (gPEnt gS) (entTree gS)
         matchingChild = [getPresentEntityName (head x) | x <- [childList | (EntityTree _ childList) <- startingPlace]]


--  getNodesWithName :: String -> EntityTree -> [Entity]






-- 16 data GeneratorState = GeneratorState {
-- 17         showVhd :: Bool
-- 18     ,   showHs :: Bool
-- 19     ,   formingEntity :: Bool
-- 20     ,   useConcurrent :: Bool
-- 21     ,   useSequential :: Bool
-- 22     ,   codeLines :: [InterspersedCode]
-- 23     ,   enableAutoPortMap :: Bool
-- 24     ,   projectParameters :: ProjectParameters
-- 25     ,   pathToPresent :: [String]
-- 26     ,   entTree :: EntityTree
-- 27     ,   defaultClk :: Information
-- 28     ,   defaultRst :: Information
-- 29     ,   defaultDataType :: DataType
-- 30     ,   defaultWidth :: Width
-- 31     ,   viewVhd :: Bool
-- 32     ,   viewHs :: Bool
-- 33     ,   drinkProcess :: Bool
-- 34     ,   processUnderConstruction :: [Process]
-- 35     ,   renderedCodeStartLoc :: Int
-- 36     ,   childEntityNotFound :: Bool
-- 37     } deriving (Eq, Show)





