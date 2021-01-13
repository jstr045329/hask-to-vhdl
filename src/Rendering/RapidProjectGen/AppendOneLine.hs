------------------------------------------------------------------------------------------------------------------------
--                                        Append One Line To Generator State 
--
-- This module detects whether the generator state is in a slurp VHDL or slurp H2V Command Mode, and if so, appends 
-- a string to the appropriate list. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.AppendOneLine where 
import Rendering.RapidProjectGen.GeneratorState
import Rendering.InterspersedCode
import Tools.ListTools
import Rendering.Process
import Rendering.Entity
import Rendering.EntityTree


appendH2VC :: String -> GeneratorState -> GeneratorState
appendH2VC s gS = gS { codeLines = (codeLines gS) ++ [InterspersedCode NoVhd NoHs (H2VLiteral [s])]}


modifyLastProcess :: (Process -> Process) -> GeneratorState -> GeneratorState
modifyLastProcess someFunc gS = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { processes = (dropLast (processes x)) ++ [someFunc (last (processes x))]})}


appendVhd :: String -> GeneratorState -> GeneratorState
appendVhd s gS 
--    | (drinkProcess gS) = modifyLastProcess (\x -> x { procPlainLines = (procPlainLines x) ++ [s]}) gS
    | otherwise = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { addToVhdBody = (addToVhdBody x) ++ [s]})}


-- 54 changeOneEntity :: String -> EntityTree -> (Entity -> Entity) -> EntityTree
-- 60 changeOneEntity oneEntNomen (EntityTree oneEntity children) someFunc =
-- 61     EntityTree
-- 62         (modEntMatchName oneEntNomen oneEntity someFunc)
-- 63         (map (\x -> changeOneEntity oneEntNomen x someFunc) children) where




-- 13 data GeneratorState = GeneratorState {
-- 14         showVhd :: Bool
-- 15     ,   showHs :: Bool
-- 16     ,   formingEntity :: Bool
-- 17     ,   useConcurrent :: Bool
-- 18     ,   useSequential :: Bool
-- 19     ,   codeLines :: [InterspersedCode]
-- 20     ,   enableAutoPortMap :: Bool
-- 21     ,   projectParameters :: ProjectParameters
-- 22     ,   pathToPresent :: [String]
-- 23     ,   entTree :: EntityTree
-- 24     ,   defaultClk :: Information
-- 25     ,   defaultRst :: Information
-- 26     ,   defaultDataType :: DataType
-- 27     ,   defaultWidth :: Width
-- 28     ,   viewVhd :: Bool
-- 29     ,   viewHs :: Bool
-- 30     ,   drinkProcess :: Bool
-- 31     ,   processUnderConstruction :: [Process]
-- 32     } deriving (Eq, Show)


-- 12 data Entity = Entity {
-- 13             entNomen :: String
-- 14         ,   generics :: [Information]
-- 15         ,   ports :: [Information]
-- 16         ,   signals :: [Information]
-- 17         ,   functions :: [Function]
-- 18         ,   procedures :: [Procedure]
-- 19         ,   routeInfinitelyUp :: [Information]
-- 20         ,   routeInfinitelyDown :: [Information]
-- 21         ,   nestedEntities :: [Entity]
-- 22         ,   childInstances :: [PortMap]
-- 23         ,   processes :: [Process]
-- 24         ,   inputBuffLayers :: Int
-- 25         ,   outputBuffLayers :: Int
-- 26         ,   recursionDepth :: Int
-- 27         ,   maxRecursionDepth :: Int -- For recursive entities, choose a termination depth
-- 28         ,   interspersedCode :: [InterspersedCode]
-- 29         ,   addToVhdBody :: [String] -- VHDL Literals
-- 30     } | TopLevelEntity
-- 31         deriving (Eq, Show)
-- 32 















appendVhdBatch :: [String] -> GeneratorState -> GeneratorState
appendVhdBatch los gS = gS { codeLines = (codeLines gS) ++ [InterspersedCode (VhdLiteral los) NoHs NoH2V]}






