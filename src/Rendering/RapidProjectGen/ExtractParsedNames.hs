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
import Data.Sort
import Data.List
import Parsing.ConstantRecognition
import Parsing.GuaranteeWhitespace
import Rendering.FilterUnique
import qualified Rendering.PopulateTemplates as PopTemp


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
-- 
-- From GeneratorState
--
------------------------------------------------------------------------------------------------------------------------
extractParsedInputs' :: GeneratorState -> [Information]
extractParsedInputs' gS = map (\oneInfoName -> applyInformationDefaults gS (easyInSl oneInfoName [])) (sort (HashSet.toList (inputNames (finalizePorts (parsedNames (head myEntList)))))) where
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedOutputs' :: GeneratorState -> [Information]
extractParsedOutputs' gS = map (\oneInfoName -> applyInformationDefaults gS (easyOutSl oneInfoName [])) (sort (HashSet.toList (outputNames (finalizePorts (parsedNames (head myEntList)))))) where
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedSignals' :: GeneratorState -> [Information]
extractParsedSignals' gS = map (\oneInfoName -> applyInformationDefaults gS (easySig oneInfoName StdLogicVector (Hard 32) [])) (sort (HashSet.toList (sigNames (finalizePorts (parsedNames (head myEntList)))))) where
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


makeOneGeneric :: String -> Information
makeOneGeneric s = Generic {
        nomen = s
    ,   dataType = UnconstrainedInt
    ,   width = Hard 32
    ,   sDefault = Specified "<Assign_Me>"
    ,   comments = []
    }


extractGenericNames :: [String] -> [String]
extractGenericNames los = [x | x <- los, Parsing.ConstantRecognition.isGeneric x]


extractParsedGenerics' :: GeneratorState -> [Information]
extractParsedGenerics' gS = map makeOneGeneric (extractGenericNames (tokenize [intercalate " " ((addToVhdBody myEnt) ++ (procPlainLines (head (processUnderConstruction gS))))])) where
    myEnt = head (fetchOneEntity (gPEnt gS) (entTree gS))


------------------------------------------------------------------------------------------------------------------------
--                                               Extract Parsed Names 
-- 
-- From TuiState
--
------------------------------------------------------------------------------------------------------------------------
extractParsedInputs :: TuiState -> [Information]
extractParsedInputs ts = extractParsedInputs' (generatorState ts)


extractParsedOutputs :: TuiState -> [Information]
extractParsedOutputs ts = extractParsedOutputs' (generatorState ts)


extractParsedSignals :: TuiState -> [Information]
extractParsedSignals ts = extractParsedSignals' (generatorState ts)


extractParsedGenerics :: TuiState -> [Information]
extractParsedGenerics ts = extractParsedGenerics' (generatorState ts)


------------------------------------------------------------------------------------------------------------------------
--                                           Glean Generics from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanGenerics :: TuiState -> [String]
gleanGenerics ts = 
    take numGenericsToDisplay
        ([ctrString "Generics" sideColumn, sideColDashes] ++  
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo (aggGenerics (head (getNodesWithName (pEnt ts) (entTree (generatorState ts)))))
            else []) ++
        blankLines)


------------------------------------------------------------------------------------------------------------------------
--                                             Glean Ports from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanPorts :: TuiState -> [String]
gleanPorts ts =
    take numPortsToDisplay
        ([ctrString "Ports" sideColumn, sideColDashes] ++
        (sort (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then sort (map showOneInfo myInformations) 
            else [])) ++
        blankLines) where
            myEnt = getPresentEntity (generatorState ts)
            myInformations = [x | x <- filterUnique ((aggInputs myEnt) ++ (aggOutputs myEnt)), not (elem (nomen x) (map nomen (signals myEnt)))]


------------------------------------------------------------------------------------------------------------------------
--                                            Glean Signals from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanSignals :: TuiState -> [String]
gleanSignals ts =
    take numSignalsToDisplay
        ([ctrString "Signals" sideColumn, sideColDashes] ++
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            -- then sort (map showOneInfo ((signals (head (getNodesWithName (pEnt ts) (entTree (generatorState ts))))) ++ myInformations))
            then sort (map showOneInfo myInformations)
            else []) ++
        blankLines) where
            myEnt = getPresentEntity (generatorState ts)
            myInformations = [x | x <- filterUnique (aggSignals myEnt), not (elem (nomen x) (map nomen (ports myEnt)))]


------------------------------------------------------------------------------------------------------------------------
--                                         Glean Rendered Code from TuiState 
------------------------------------------------------------------------------------------------------------------------
bedOfProcrustes :: String -> String
bedOfProcrustes s = take renderedCodeWidth (s ++ (repeat ' '))


allProcessLines :: Entity -> [String]
allProcessLines someEnt = flattenShallow (map (\x -> renderProcess x easyProjParams) (processes someEnt))


gleanRenderedCode :: GeneratorState -> [String]
gleanRenderedCode gS = [titleLine] ++ take renderedLinesToShow (skipN perfectLines startLoc) where
    oneEnt = getPresentEntity gS
    myNewProcessLines = if ((head (processUnderConstruction gS)) == defaultProcess)
                            then []
                            else renderProcess (head (processUnderConstruction gS)) (projectParameters gS)
    rawLines = (PopTemp.populateEntityTemplate
                (declareBatch (aggGenerics oneEnt))
                (declareBatch ((aggInputs oneEnt) ++ (aggOutputs oneEnt)))
                (declareBatch (aggSignals oneEnt))
                ((addToVhdBody oneEnt) ++ (allProcessLines oneEnt) ++ myNewProcessLines)
                (PopTemp.vanillaSettings (entNomen oneEnt))) ++
                blankLines
    perfectLines = map bedOfProcrustes rawLines
    titleLine = ctrString "Rendered Code" renderedCodeWidth
    startLoc = renderedCodeStartLoc gS


------------------------------------------------------------------------------------------------------------------------
--                       Place Default Signals In Entity, Giving Precedence To Declared Names 
--
-- This function declares default ports, default signals, etc., but only deposits them in the aggregate list of Information's 
-- if the same name has not been declared. In other words, if the user declares an input named Jose, yet the parser 
-- thinks that Jose should be an output, the final entity will contain input Jose. 
--
------------------------------------------------------------------------------------------------------------------------
populateEntityDefaults :: GeneratorState -> Entity -> Entity
populateEntityDefaults gS myEnt = myEnt {
        parsedInputs = extractParsedInputs' gS
    ,   parsedOutputs = extractParsedOutputs' gS
    ,   parsedSignals = extractParsedSignals' gS
    ,   parsedGenerics = extractParsedGenerics' gS
    } 


addTheMoreImportantThing :: [Information] -> [Information] -> [Information] -> [Information]
addTheMoreImportantThing superior inferior bigList = superior ++ [x | x <- inferior, not (elem (nomen x) [nomen y | y <- superior])]


putInfoInEntity :: GeneratorState -> Entity -> Entity
putInfoInEntity gS myEnt = (populateEntityDefaults gS myEnt) {
        aggInputs = addTheMoreImportantThing (getInputPorts (ports myEnt)) (parsedInputs myEnt) bigList
    ,   aggOutputs = addTheMoreImportantThing (getOutputPorts (ports myEnt)) (parsedOutputs myEnt) bigList
    ,   aggSignals = addTheMoreImportantThing (signals myEnt) (parsedSignals myEnt) bigList
    ,   aggGenerics = addTheMoreImportantThing (generics myEnt) (parsedGenerics myEnt) bigList
    } where
        newEnt = (populateEntityDefaults gS myEnt)
        bigList = (ports myEnt) ++ (signals myEnt) ++ (generics myEnt)


putInfoInPresentEntity :: GeneratorState -> GeneratorState
putInfoInPresentEntity gS = changePresentEntity (\x -> putInfoInEntity gS x) gS
        

