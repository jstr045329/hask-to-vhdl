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
appendVhd s gS = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { addToVhdBody = (addToVhdBody x) ++ [s]})}


appendVhdBatch :: [String] -> GeneratorState -> GeneratorState
appendVhdBatch los gS = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { addToVhdBody = (addToVhdBody x) ++ los})}


