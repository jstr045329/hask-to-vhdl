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


--appendOneLine :: String -> GeneratorState -> GeneratorState
--appendOneLine s gS
--    | (drinkingVhd gS) = gS { codeLines = (codeLines gS) ++ [InterspersedCode (VhdLiteral [s]) NoHs NoH2V]}
--    | (drinkingH2VC gS) = gS { codeLines = (codeLines gS) ++ [InterspersedCode NoVhd NoHs (H2VLiteral [s])]}
--    | otherwise = gS


appendH2VC :: String -> GeneratorState -> GeneratorState
appendH2VC s gS = gS { codeLines = (codeLines gS) ++ [InterspersedCode NoVhd NoHs (H2VLiteral [s])]}


appendVhd :: String -> GeneratorState -> GeneratorState
appendVhd s gS = gS { codeLines = (codeLines gS) ++ [InterspersedCode (VhdLiteral [s]) NoHs NoH2V]}


appendVhdBatch :: [String] -> GeneratorState -> GeneratorState
appendVhdBatch los gS = gS { codeLines = (codeLines gS) ++ [InterspersedCode (VhdLiteral los) NoHs NoH2V]}


