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
extractParsedInputs' :: GeneratorState -> HashSet.HashSet Information
extractParsedInputs' gS = HashSet.map (\oneInfoName -> applyInformationDefaults gS (easyInSl oneInfoName [])) (inputNames (finalizePorts (parsedNames (head myEntList)))) where
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedOutputs' :: GeneratorState -> HashSet.HashSet Information
extractParsedOutputs' gS = HashSet.map (\oneInfoName -> applyInformationDefaults gS (easyOutSl oneInfoName [])) (outputNames (finalizePorts (parsedNames (head myEntList)))) where
    myEntList = fetchOneEntity (gPEnt gS) (entTree gS) 


extractParsedSignals' :: GeneratorState -> HashSet.HashSet Information
extractParsedSignals' gS = HashSet.map (\oneInfoName -> applyInformationDefaults gS (easySig oneInfoName StdLogicVector (Hard 32) [])) (sigNames (finalizePorts (parsedNames (head myEntList)))) where
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


extractParsedGenerics' :: GeneratorState -> HashSet.HashSet Information
extractParsedGenerics' gS = HashSet.fromList (map makeOneGeneric (extractGenericNames (tokenize [intercalate " " ((addToVhdBody myEnt) ++ (procPlainLines (head (processUnderConstruction gS))))]))) where
    myEnt = head (fetchOneEntity (gPEnt gS) (entTree gS))


------------------------------------------------------------------------------------------------------------------------
--                                           Glean Generics from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanGenerics :: TuiState -> [String]
gleanGenerics ts = 
    take numGenericsToDisplay
        ([ctrString "Generics" sideColumn, sideColDashes] ++  
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then HashSet.toList (HashSet.map showOneInfo (aggGenerics (head (getNodesWithName (pEnt ts) (entTree (generatorState ts))))))
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
            then HashSet.toList (HashSet.map showOneInfo myInformations) 
            else []) ++
        blankLines) where
            myEnt = getPresentEntity (generatorState ts)
            myInformations = HashSet.difference (HashSet.union (aggInputs myEnt) (aggOutputs myEnt)) (HashSet.fromList (signals myEnt))
            

------------------------------------------------------------------------------------------------------------------------
--                                            Glean Signals from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanSignals :: TuiState -> [String]
gleanSignals ts =
    take numSignalsToDisplay
        ([ctrString "Signals" sideColumn, sideColDashes] ++
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo myInformations
            else []) ++
        blankLines) where
            myEnt = getPresentEntity (generatorState ts)
            myInformations = HashSet.toList (HashSet.difference (aggSignals myEnt) (HashSet.fromList (ports myEnt)))


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

    -- NOTE: If TUI is still slow, check if gleanRenderedCode (and other gleaning/rendering functions) get fed back into parseVhd.
    rawLines = (PopTemp.populateEntityTemplate
                (declareBatch (HashSet.toList (aggGenerics oneEnt)))
                (declareBatch (HashSet.toList (HashSet.union (aggInputs oneEnt) (aggOutputs oneEnt))))
                (declareBatch (HashSet.toList (aggSignals oneEnt)))
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


addTheMoreImportantThing :: [Information] -> [Information] -> [Information]
addTheMoreImportantThing superior inferior = superior ++ [x | x <- inferior, not (elem (nomen x) [nomen y | y <- superior])]


-- NOTE: This function is probably inefficient. Revisit if updating is still slow. 
putInfoInEntity :: GeneratorState -> Entity -> Entity
putInfoInEntity gS myEnt = (populateEntityDefaults gS myEnt) {
        aggInputs = HashSet.fromList (addTheMoreImportantThing (getInputPorts (ports myEnt)) (HashSet.toList (parsedInputs myEnt)))
    ,   aggOutputs = HashSet.fromList (addTheMoreImportantThing (getOutputPorts (ports myEnt)) (HashSet.toList (parsedOutputs myEnt)))
    ,   aggSignals = HashSet.fromList (addTheMoreImportantThing (signals myEnt) (HashSet.toList (parsedSignals myEnt)))
    ,   aggGenerics = HashSet.fromList (addTheMoreImportantThing (generics myEnt) (HashSet.toList (parsedGenerics myEnt)))
    } where
        newEnt = (populateEntityDefaults gS myEnt)
        bigList = (ports myEnt) ++ (signals myEnt) ++ (generics myEnt)


putInfoInPresentEntity :: GeneratorState -> GeneratorState
putInfoInPresentEntity gS = changePresentEntity (\x -> putInfoInEntity gS x) gS
        

