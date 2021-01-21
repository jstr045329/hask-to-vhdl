module Rendering.RapidProjectGen.UpdateVhdRendering (
        updateVhdRendering
    ,   updateProcessOutputs
    ) where
import Rendering.Entity
import Rendering.Process
import Rendering.RapidProjectGen.GeneratorState
import Parsing.SourceSinkParser
import Data.List
import Parsing.InputParsingKeywords
import Tools.ListTools
import Rendering.InfoTypes
import Rendering.EntityTree
import Parsing.GuaranteeWhitespace
import qualified Data.HashSet as HashSet
import Rendering.RapidProjectGen.PresentEntity


------------------------------------------------------------------------------------------------------------------------
--                               Call parseVhd Afresh on All Relevant Lists of Strings 
--
-- This function concatenates all relevant lists, calls parseVhd on the whole, without changing the input lists. 
-- Its purpose is to update the parsedNames field in the present entity. The motivation for doing so is so that the 
-- present Entity contains all parsed Information names at all times. 
--
-- NOTE: At present, updateVhdRendering is very parseVhd centric, but it does not have to be. It is reasonable to call
-- from this function any other functions necessary to support good rendering.
--
------------------------------------------------------------------------------------------------------------------------
updateVhdRendering :: GeneratorState -> GeneratorState
updateVhdRendering gS = changePresentEntity (\x -> x {parsedNames = parseVhd (tokenize [intercalate " " myVhdLines]) IP_NoKeyword}) gS where
                            myEntity = head (fetchOneEntity (gPEnt gS) (entTree gS))
                            myVhdLines = (addToVhdBody myEntity) ++ (flattenShallow (map procPlainLines (processes myEntity))) ++ (procPlainLines (head (processUnderConstruction gS)))


------------------------------------------------------------------------------------------------------------------------
--                              Use GeneratorState Assumptions to Make Default Signals 
--
-- Take the defaults specified in GeneratorState and make one or more signals. 
--
------------------------------------------------------------------------------------------------------------------------
makeDefaultSig :: String -> GeneratorState -> Information
makeDefaultSig oneSigName gS = VhdSig {
        nomen = oneSigName
    ,   dataType = defaultDataType gS
    ,   width = defaultWidth gS
    ,   sDefault = Unspecified
    ,   sReset = makeResetVal (defaultDataType gS)
    ,   clocked = Nothing
    ,   comments = []
    ,   assertionLevel = Nothing
    }


makeDefaultSigBatch :: GeneratorState -> [Information]
makeDefaultSigBatch gS = map (\x -> makeDefaultSig x gS) parsedOutputNames where
    myEntity = getPresentEntity gS
    myProcess = head (processUnderConstruction gS)
    parsedOutputNames = HashSet.toList (outputNames (parseVhd (tokenize [intercalate " " (procPlainLines (head (processUnderConstruction gS)))]) IP_NoKeyword))


------------------------------------------------------------------------------------------------------------------------
--                                     Make The Best Available Information List 
--
-- This function provides the latest available signal list. 
--
-- This function:
--      1) Gleans all the names 
--      2) Starts a list with all the declared signals
--      3) Appends all the gleaned signals that are not already in the list.
--
------------------------------------------------------------------------------------------------------------------------
fetchProcessOutputs :: GeneratorState -> [Information]
fetchProcessOutputs gS = bestAvailableSigList where
    myEntity = getPresentEntity gS
    declaredSignals = (signals myEntity)
    parsedSignals = [x | x <- (makeDefaultSigBatch gS), not (elem (nomen x) (map nomen declaredSignals))]
    bestAvailableSigList = declaredSignals ++ parsedSignals


------------------------------------------------------------------------------------------------------------------------
--                            Embed Latest Information List in Process Under Construction 
--
-- This function takes the best available Information list and embeds it in the processUnderConstruction in 
-- GeneratorState. 
--
------------------------------------------------------------------------------------------------------------------------
updateProcessOutputs :: GeneratorState -> GeneratorState
updateProcessOutputs gS = gS {
    processUnderConstruction = [(head (processUnderConstruction gS)) {
            procOutputSignals = fetchProcessOutputs gS
        }]
    }


