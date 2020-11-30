module Rendering.RapidProjectGen.CommandDecoder where 
import Rendering.RapidProjectGen.GeneratorState
import Rendering.InfoTypes
import Rendering.Entity
import Tools.WhiteSpaceTools
import Tools.ListTools
import Parsing.TokenMatchingTools
import Tools.StringTools
import Parsing.GuaranteeWhitespace


------------------------------------------------------------------------------------------------------------------------
--                             Helper Functions To Decode Port, Signal & Generic Creation
------------------------------------------------------------------------------------------------------------------------
decodeGenericType :: [String] -> DataType
decodeGenericType los
    | ((head los) == "StdLogic") = StdLogic
    | ((head los) == "StdLogicVector") = StdLogicVector
    | ((head los) == "Signed") = Signed 
    | ((head los) == "Unsigned") = Unsigned
    | otherwise = UnconstrainedInt


decodeGenericWidth' :: [String] -> Width
decodeGenericWidth' los 
    | (length (afterKeyword los ["width", "="]) > 0) = Hard (read (head (afterKeyword los ["width", "="])) :: Integer)
    | otherwise = WidthNotSpecified


decodeGenericWidth :: [String] -> Width
decodeGenericWidth los
    | ((head los) == "StdLogic") = Hard 1
    | ((head los) == "StdLogicVector") = decodeGenericWidth' los
    | ((head los) == "Signed") = decodeGenericWidth' los
    | ((head los) == "Unsigned") = decodeGenericWidth' los
    | otherwise = Hard 32


------------------------------------------------------------------------------------------------------------------------
--                                              Make 1 New Generic
------------------------------------------------------------------------------------------------------------------------
makeOneNewGeneric :: String -> GeneratorState -> Information
makeOneNewGeneric oneStr generatorState =
    let tokList = words oneStr
    in Generic {
            nomen = tokList !! 0
        ,   dataType = decodeGenericType tokList
        ,   width = decodeGenericWidth tokList

        -- TODO: Replace Unspecified with parsing
        ,   sDefault = Unspecified
        ,   comments = []
        }


------------------------------------------------------------------------------------------------------------------------
--                                              Make 1 New Signal
------------------------------------------------------------------------------------------------------------------------
makeOneNewSignal :: String -> GeneratorState -> Information
makeOneNewSignal oneStr generatorState =
    let tokList = words oneStr
    in VhdSig {
            nomen = tokList !! 0
        ,   dataType = defaultDataType generatorState
        ,   width = if (length (afterKeyword tokList ["width", "="]) > 0)
                        then Hard (read (head (afterKeyword tokList ["width", "="])) :: Integer)
                        else defaultWidth generatorState
        ,   sDefault = Unspecified
        ,   sReset = makeResetVal (defaultDataType generatorState)
        ,   clocked = Nothing
        ,   comments = []
        ,   assertionLevel = Nothing
        }


------------------------------------------------------------------------------------------------------------------------
--                                                Make 1 New Port
------------------------------------------------------------------------------------------------------------------------
makeOneNewPort :: String -> GeneratorState -> Bool -> Information
makeOneNewPort oneStr generatorState isInput =
    let tokList = words oneStr
    in Port {
            nomen = tokList !! 0
        ,   dataType = defaultDataType generatorState
        ,   width = if (length (afterKeyword tokList ["width", "="]) > 0)
                        then Hard (read (head (afterKeyword tokList ["width", "="])) :: Integer)
                        else defaultWidth generatorState
        ,   direction = if isInput
                            then In
                            else Out
        ,   sDefault = Unspecified
        ,   sReset = makeResetVal (defaultDataType generatorState)
        ,   clocked = Nothing
        ,   comments = []
        ,   assertionLevel = Nothing
        }


------------------------------------------------------------------------------------------------------------------------
--                                           Update the State of Present Entity
------------------------------------------------------------------------------------------------------------------------
slurpCommand :: String -> GeneratorState -> GeneratorState
slurpCommand s gS 
    | (drinkingVhd gS) = gS { presentEntity = (presentEntity gS) { literalVhdLines = (literalVhdLines (presentEntity gS)) ++ [s]}}

    | (drinkingHs gS) = gS { presentEntity = (presentEntity gS) { literalHsLines = (literalHsLines (presentEntity gS)) ++ [s]}}
    
    | (startsWith s "gen ") = gS {presentEntity = (presentEntity gS) {generics = (generics (presentEntity gS)) ++ [makeOneNewGeneric (skipN s 4) gS]}}

    | (startsWith s "sig ") = gS {presentEntity = (presentEntity gS) {signals = (signals (presentEntity gS)) ++ [makeOneNewSignal (skipN s 4) gS]}}

    | (startsWith s "in ") = gS {presentEntity = (presentEntity gS) {ports = (ports (presentEntity gS)) ++ [makeOneNewPort (skipN s 3) gS True]}}

    | (startsWith s "out ") = gS {presentEntity = (presentEntity gS) {ports = (ports (presentEntity gS)) ++ [makeOneNewPort (skipN s 4) gS False]}}

    | otherwise = gS


------------------------------------------------------------------------------------------------------------------------
--                                                   Parse Commands
------------------------------------------------------------------------------------------------------------------------
decodeOneStr :: String -> GeneratorState -> GeneratorState
decodeOneStr oneStr presentState
    | (startsWith oneStr "<ent>") = presentState { formingEntity = True}

    -- TODO: Make a way to set name of present entity

    | (startsWith oneStr "</ent>") = presentState { formingEntity = False}

    | (startsWith oneStr "<vhd>") = presentState { drinkingVhd = True }

    | (startsWith oneStr "</vhd>") = presentState { drinkingVhd = False }

    | (startsWith oneStr "<hs>") = presentState { drinkingHs = True }

    | (startsWith oneStr "</hs>") = presentState { drinkingHs = False }

    | (startsWith oneStr "show vhd") = presentState {showVhd = True, showHs = False}

    | (startsWith oneStr "show hs") = presentState {showVhd = False, showHs = True}

    | otherwise = slurpCommand oneStr presentState

    -- TODO: Reinstate drinkingProcess
    -- Process Imbiber should parse signals.
    -- Anything to the left of a <= gets declared if it isn't already in the signal list.
    -- If you declare something after you use it, TUI fills in the missing details. 
    -- When you type </proc>, TUI prompts you to fill in any details that are still missing. 

    -- Process Imbiber should automatically reset any signals you use. 




-- TODO: Make faculties for recursive entities. 

-- TODO: Make all signals in an entity available to child entities. 
-- Automatically wire up port map. 


-- TODO: Add HashSet's for libraries needed

-- TODO: Come up with a way to try executing Hs literal lines, and 
-- Hs literals only affect rendered VHDL when compilation is successful

-- TODO: Come up with some simple rule that allows hask-to-vhdl to intersperse
-- Hs output with Vhd literal lines. 

-- TODO: Add commands to modify ProjectParameters.
-- You should be able to change reset style in all processes with a single command. 

-- TODO: Add commands to choose a different clock, different reset style for individual
-- processes. 

