module Rendering.RapidProjectGen.UpdateVhdRendering (
        updateVhdRendering
    ) where
import Rendering.Entity
import Rendering.Process
import Rendering.RapidProjectGen.GeneratorState
import Parsing.SourceSinkParser
import Data.List
import Parsing.InputParsingKeywords
import Tools.ListTools
import Rendering.EntityTree
import Parsing.GuaranteeWhitespace


updateVhdRendering :: GeneratorState -> GeneratorState
updateVhdRendering gS = changePresentEntity (\x -> x {parsedNames = parseVhd (tokenize [intercalate " " myVhdLines]) IP_NoKeyword}) gS where
                            myEntity = head (fetchOneEntity (gPEnt gS) (entTree gS))
                            myVhdLines = (addToVhdBody myEntity) ++ (flattenShallow (map procPlainLines (processes myEntity))) ++ (procPlainLines (head (processUnderConstruction gS)))


