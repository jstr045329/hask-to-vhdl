module Rendering.RapidProjectGen.WriteEntityFiles (
        dumpGeneratorStateToFile
    ,   dumpGeneratorStateToFile'
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
                            (declareBatch (HashSet.toList (aggGenerics oneEnt)))
                            (declareBatch (HashSet.toList (HashSet.union (aggOutputs oneEnt) (aggInputs oneEnt))))
                            (declareBatch (HashSet.toList (aggSignals oneEnt)))
                            ((addToVhdBody oneEnt) ++ (allProcessLines oneEnt))
                            (PopTemp.vanillaSettings (entNomen oneEnt))) 


------------------------------------------------------------------------------------------------------------------------
--                                         Recursively Glean an Entity Tree 
------------------------------------------------------------------------------------------------------------------------
gleanWholeTree :: EntityTree -> [[String]]
gleanWholeTree (EntityTree oneEnt []) = [gleanOneEntity oneEnt]
gleanWholeTree (EntityTree oneEnt moreTrees) = 
    [(gleanOneEntity oneEnt)] ++ (flattenShallow (map gleanWholeTree moreTrees))


dumpAllEntities :: EntityTree -> IO [()]
dumpAllEntities (EntityTree oneEnt []) = 
    do
        dump2File ("/home/jim/hask-to-vhdl-results/" ++ (entNomen oneEnt) ++ ".vhd") (gleanOneEntity oneEnt)
        return []
dumpAllEntities (EntityTree oneEnt moreTrees) =
    do
        dump2File ("/home/jim/hask-to-vhdl-results/" ++ (entNomen oneEnt) ++ ".vhd") (gleanOneEntity oneEnt)
        (mapM dumpAllEntities moreTrees)
        return []
    

------------------------------------------------------------------------------------------------------------------------
--                                         Generate Testbench for One Entity 
------------------------------------------------------------------------------------------------------------------------
makeOneTestbench :: Entity -> [String]
makeOneTestbench oneEnt 
    | ((length (gleanOneEntity oneEnt)) < 16) = [] -- TODO: Delete this branch and see if that breaks anything
    | otherwise = generateTestbench (tokenize [intercalate " " (gleanOneEntity oneEnt)])


------------------------------------------------------------------------------------------------------------------------
--                                        Generate Testbenches for Hierarchy 
------------------------------------------------------------------------------------------------------------------------
makeTestbenches :: EntityTree -> [String]
makeTestbenches (EntityTree oneEnt []) = makeOneTestbench oneEnt
makeTestbenches (EntityTree oneEnt moreTrees) = 
    (makeOneTestbench oneEnt) ++ (flattenShallow (map makeTestbenches moreTrees))


makeOneTcl :: Entity -> [String]
makeOneTcl oneEnt = PopTemp.populateTclTemplate "/home/jim/hask-to-vhdl/" "<PATH_TO_THIS_PROJECT>" ["<LOCAL_DEPS>"] (entNomen oneEnt)

dumpOneTcl oneEnt = 
    dump2File 
        ("/home/jim/hask-to-vhdl-results/" ++ (entNomen oneEnt) ++ "_tb.tcl")
        (makeOneTcl oneEnt)


dumpAllTcls :: EntityTree -> IO [()]
dumpAllTcls (EntityTree oneEnt []) =
    do  
        dumpOneTcl oneEnt
        return []
dumpAllTcls (EntityTree oneEnt moreTrees) =
    do
        (dumpOneTcl oneEnt)
        (mapM dumpAllTcls moreTrees)
        return []


------------------------------------------------------------------------------------------------------------------------
--                                                     Call This 
------------------------------------------------------------------------------------------------------------------------

dumpOneTestbench oneEnt =
    dump2File 
        ("/home/jim/hask-to-vhdl-results/" ++ (entNomen oneEnt) ++ "_tb.vhd")
        (makeOneTestbench oneEnt)

dumpAllTestbenches :: EntityTree -> IO [()]
dumpAllTestbenches (EntityTree oneEnt []) = 
    do
        dumpOneTestbench oneEnt
        return []
dumpAllTestbenches (EntityTree oneEnt moreTrees) =
    do
        (dumpOneTestbench oneEnt)
        (mapM dumpAllTestbenches moreTrees)
        return []
    

------------------------------------------------------------------------------------------------------------------------
--                                          Take TuiState and Dump To File 
------------------------------------------------------------------------------------------------------------------------
dumpGeneratorStateToFile tS =
    do
        dumpAllEntities (entTree (generatorState tS))
        dumpAllTestbenches (entTree (generatorState tS))
        dumpAllTcls (entTree (generatorState tS))
        return tS


------------------------------------------------------------------------------------------------------------------------
--                                       Take GeneratorState and Dump To File 
------------------------------------------------------------------------------------------------------------------------
dumpGeneratorStateToFile' gS =
    do
        dumpAllEntities (entTree gS)
        dumpAllTestbenches (entTree gS)
        dumpAllTcls (entTree gS)
        return gS


