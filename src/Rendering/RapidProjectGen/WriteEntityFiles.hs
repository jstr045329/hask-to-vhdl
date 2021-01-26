module Rendering.RapidProjectGen.WriteEntityFiles (
    dumpGeneratorStateToFile
    ) where
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
import Rendering.ProcessFile
import Rendering.RapidProjectGen.ExtractParsedNames
import Rendering.GenerateTestbench


------------------------------------------------------------------------------------------------------------------------
--                                      Convert One Entity to a List of Strings 
--
-- Only use this after Entity is done. The reason is that this function has no access to processUnderConstruction. 
--
------------------------------------------------------------------------------------------------------------------------
gleanOneEntity :: Entity -> [String]
gleanOneEntity oneEnt = (PopTemp.populateEntityTemplate
                            (declareBatch (aggGenerics oneEnt))
                            (declareBatch ((aggInputs oneEnt) ++ (aggOutputs oneEnt)))
                            (declareBatch (aggSignals oneEnt))
                            ((addToVhdBody oneEnt) ++ (allProcessLines oneEnt))
                            (PopTemp.vanillaSettings (entNomen oneEnt))) 




------------------------------------------------------------------------------------------------------------------------
--                                         Recursively Glean an Entity Tree 
------------------------------------------------------------------------------------------------------------------------
gleanWholeTree :: EntityTree -> [String]
gleanWholeTree (EntityTree oneEnt []) = gleanOneEntity oneEnt
gleanWholeTree (EntityTree oneEnt moreTrees) = 
    (gleanOneEntity oneEnt) ++ (flattenShallow (map gleanWholeTree moreTrees))
    -- TODO: PICK UP HERE: Call generateTestbench on each entity (after testbenches are de-grossed.)


------------------------------------------------------------------------------------------------------------------------
--                                         Generate Testbench for One Entity 
------------------------------------------------------------------------------------------------------------------------
makeOneTestbench :: Entity -> [String]
makeOneTestbench oneEnt 
    | ((length (gleanOneEntity oneEnt)) < 16) = []
    | otherwise = generateTestbench (gleanOneEntity oneEnt)


------------------------------------------------------------------------------------------------------------------------
--                                        Generate Testbenches for Hierarchy 
------------------------------------------------------------------------------------------------------------------------
makeTestbenches :: EntityTree -> [String]
makeTestbenches (EntityTree oneEnt []) = makeOneTestbench oneEnt
makeTestbenches (EntityTree oneEnt moreTrees) = 
    (makeOneTestbench oneEnt) ++ (flattenShallow (map makeTestbenches moreTrees))


------------------------------------------------------------------------------------------------------------------------
--                                                     Call This 
------------------------------------------------------------------------------------------------------------------------
dumpGeneratorStateToFile :: TuiState -> IO TuiState
dumpGeneratorStateToFile tS =
    do
        -- dump2File "./myVhdOutput.vhd" ((gleanWholeTree (entTree (generatorState tS))) ++ (makeTestbenches (entTree (generatorState tS))))
        dump2File "./myVhdOutput.vhd" (gleanWholeTree (entTree (generatorState tS)))
        return tS


