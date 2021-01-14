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


