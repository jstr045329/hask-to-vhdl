module Rendering.RapidProjectGen.ExtractParsedNames where 
import Rendering.RapidProjectGen.RapidTuiState
import Rendering.RapidProjectGen.GeneratorState
import Rendering.RapidProjectGen.TuiParameters
import Rendering.Entity
import Rendering.EntityTree
import Rendering.InfoTypes
import qualified Data.HashSet as HashSet
import Tools.ListTools
import Tools.StringTools
import Tools.WhiteSpaceTools
import Rendering.RapidProjectGen.PresentEntity
import Parsing.SourceSinkParser
import Rendering.RapidProjectGen.ScreenParameters
import Rendering.RapidProjectGen.SideColDashes
import Rendering.RapidProjectGen.DisplayInformation
import Rendering.Process
import Rendering.ProjectParameters


------------------------------------------------------------------------------------------------------------------------
--                                            Apply Information Defaults 
--
-- Retrieve default Info settings from GeneratorState, and apply those to a new Information. 
--
------------------------------------------------------------------------------------------------------------------------
applyInformationDefaults :: GeneratorState -> Information -> Information
applyInformationDefaults gS myInfo = myInfo {
        dataType = (defaultDataType gS) 
    ,   width = (defaultWidth gS) 
    }   


------------------------------------------------------------------------------------------------------------------------
--                                               Extract Parsed Names 
------------------------------------------------------------------------------------------------------------------------
extractParsedInputs :: TuiState -> [Information]
extractParsedInputs ts = map (\oneInfoName -> applyInformationDefaults gS (easyInSl oneInfoName [])) (HashSet.toList (inputNames (finalizePorts (parsedNames (head myEntList))))) where
    gS = generatorState ts
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedOutputs :: TuiState -> [Information]
extractParsedOutputs ts = map (\oneInfoName -> applyInformationDefaults gS (easyOutSl oneInfoName [])) (HashSet.toList (outputNames (finalizePorts (parsedNames (head myEntList))))) where
    gS = generatorState ts
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedSignals :: TuiState -> [Information]
extractParsedSignals ts = map (\oneInfoName -> applyInformationDefaults gS (easySig oneInfoName StdLogicVector (Hard 32) [])) (HashSet.toList (sigNames (finalizePorts (parsedNames (head myEntList))))) where
    gS = generatorState ts
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


------------------------------------------------------------------------------------------------------------------------
--                                           Glean Generics from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanGenerics :: TuiState -> [String]
gleanGenerics ts = 
    take numGenericsToDisplay
        ([ctrString "Generics" sideColumn, sideColDashes] ++  
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo (generics (head (getNodesWithName (pEnt ts) (entTree (generatorState ts)))))
            else []) ++
        blankLines)


------------------------------------------------------------------------------------------------------------------------
--                                             Glean Ports from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanPorts :: TuiState -> [String]
gleanPorts ts =
    take numPortsToDisplay
        ([ctrString "Ports" sideColumn, sideColDashes] ++
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo ((ports (head (getNodesWithName (pEnt ts) (entTree (generatorState ts))))) ++ myInformations)
            else []) ++
        blankLines) where
            myInformations = (extractParsedInputs ts) ++ (extractParsedOutputs ts)


------------------------------------------------------------------------------------------------------------------------
--                                            Glean Signals from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanSignals :: TuiState -> [String]
gleanSignals ts =
    take numSignalsToDisplay
        ([ctrString "Signals" sideColumn, sideColDashes] ++
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo ((signals (head (getNodesWithName (pEnt ts) (entTree (generatorState ts))))) ++ myInformations)
            else []) ++
        blankLines) where
            myInformations = extractParsedSignals ts


------------------------------------------------------------------------------------------------------------------------
--                                         Glean Rendered Code from TuiState 
------------------------------------------------------------------------------------------------------------------------
bedOfProcrustes :: String -> String
bedOfProcrustes s = take renderedCodeWidth (s ++ (repeat ' '))


allProcessLines :: Entity -> [String]
allProcessLines someEnt = flattenShallow (map (\x -> renderProcess x easyProjParams) (processes someEnt))


gleanRenderedCode :: TuiState -> [String]
gleanRenderedCode ts = [titleLine] ++ take renderedLinesToShow (skipN perfectLines startLoc) where
    oneEntTree = entTree (generatorState ts)
    oneEnt = head (fetchOneEntity (pEnt ts) oneEntTree)
    gS = generatorState ts
    myNewProcessLines = if ((head (processUnderConstruction gS)) == defaultProcess)
                            then []
                            else renderProcess (head (processUnderConstruction gS)) (projectParameters gS)
    rawLines = (addToVhdBody oneEnt) ++ (allProcessLines oneEnt) ++ myNewProcessLines ++ blankLines
    perfectLines = map bedOfProcrustes rawLines
    titleLine = ctrString "Rendered Code" renderedCodeWidth
    startLoc = renderedCodeStartLoc (generatorState ts)


