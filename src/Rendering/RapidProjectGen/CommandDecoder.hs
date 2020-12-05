module Rendering.RapidProjectGen.CommandDecoder where 
import Rendering.RapidProjectGen.GeneratorState
import Rendering.InfoTypes
import Rendering.Entity
import Rendering.EntityTree
import Tools.WhiteSpaceTools
import Tools.ListTools
import Parsing.TokenMatchingTools
import Tools.StringTools
import Parsing.GuaranteeWhitespace
import Rendering.InterspersedCode


------------------------------------------------------------------------------------------------------------------------
--                             Helper Functions To Decode Port, Signal & Generic Creation
------------------------------------------------------------------------------------------------------------------------
decodeGenericType :: [String] -> DataType
decodeGenericType los
    | (elem "slv" los) = StdLogicVector
    | (elem "sl" los) = StdLogic
    | (elem "unsigned" los) = Unsigned
    | (elem "signed" los) = Signed
    | otherwise = UnconstrainedInt


decodeGenericWidth' :: [String] -> Width
decodeGenericWidth' los 
    | (length (afterKeyword los ["width", "="]) > 0) = Hard (read (head (afterKeyword los ["width", "="])) :: Integer)
    | otherwise = WidthNotSpecified


decodeGenericWidth :: [String] -> Width
decodeGenericWidth los
    | (elem "slv" los) = decodeGenericWidth' los
    | (elem "sl" los) = Hard 1
    | (elem "unsigned" los) = decodeGenericWidth' los
    | (elem "signed" los) = decodeGenericWidth' los
    | otherwise = Hard 32


decodeGenericDefault :: [String] -> DefaultValue
decodeGenericDefault los
    | (length (afterKeyword los [":="]) > 0) = Specified (head (afterKeyword los [":="]))
    | otherwise = Unspecified


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
        ,   sDefault = decodeGenericDefault tokList
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
--                                      Get Present Entity From Generator State 
------------------------------------------------------------------------------------------------------------------------
gPEnt :: GeneratorState -> String
gPEnt gS = last (pathToPresent gS)


------------------------------------------------------------------------------------------------------------------------
--                                           Update the State of Present Entity
------------------------------------------------------------------------------------------------------------------------
slurpCommand :: String -> GeneratorState -> GeneratorState
slurpCommand s gS 
    | (drinkingVhd gS) = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { interspersedCode = (interspersedCode x) ++ [InterspersedCode (VhdLiteral [s]) NoHs]})}

    | (drinkingVhd gS) = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { interspersedCode = (interspersedCode x) ++ [InterspersedCode NoVhd (HsLiteral [s])]})}

--    | (startsWith s "gen ") = gS {presentEntity = (presentEntity gS) {generics = (generics (presentEntity gS)) ++ [makeOneNewGeneric (skipN s 4) gS]}}
--
--    | (startsWith s "sig ") = gS {presentEntity = (presentEntity gS) {signals = (signals (presentEntity gS)) ++ [makeOneNewSignal (skipN s 4) gS]}}
--
--    | (startsWith s "in ") = gS {presentEntity = (presentEntity gS) {ports = (ports (presentEntity gS)) ++ [makeOneNewPort (skipN s 3) gS True]}}
--
--    | (startsWith s "out ") = gS {presentEntity = (presentEntity gS) {ports = (ports (presentEntity gS)) ++ [makeOneNewPort (skipN s 4) gS False]}}
--
    | otherwise = gS


------------------------------------------------------------------------------------------------------------------------
--                                                   Parse Commands
------------------------------------------------------------------------------------------------------------------------
decodeOneStr :: String -> GeneratorState -> GeneratorState
decodeOneStr oneStr gS
    -- The <ent> tag requires an entity name immediately after:
    | (startsWith oneStr "<ent>") = 
        gS { 
            formingEntity = True
        ,   entTree = 
                appendOneEntity 
                    (gPEnt gS) -- name of present entity (which will be the parent of the new entity)
                    (entTree gS) -- entity tree in Generator State
                    (defaultEntity { entNomen = (words oneStr) !! 1 }) -- The new entity we are creating

        ,   pathToPresent = (pathToPresent gS) ++ [(words oneStr) !! 1]
        }

    | (startsWith oneStr "</ent>") = gS { 
        -- formingEntity = False -- TODO: Think about whether formingEntity is really necessary
        pathToPresent = take ((length (pathToPresent gS)) - 1) (pathToPresent gS)
        }

    | (startsWith oneStr "<vhd>") = gS { drinkingVhd = True }

    | (startsWith oneStr "</vhd>") = gS { drinkingVhd = False }

    | (startsWith oneStr "<hs>") = gS { drinkingHs = True }

    | (startsWith oneStr "</hs>") = gS { drinkingHs = False }

    | (startsWith oneStr "show vhd") = gS {showVhd = True, showHs = False}

    | (startsWith oneStr "show hs") = gS {showVhd = False, showHs = True}

    | otherwise = slurpCommand oneStr gS

    -- TODO: Capture Infinite Signals
    --      * Infinite delay chains for signals
    --      * Infinite widths for all Information's.
    --      * Might make sense to change all [Information]'s to [[Informations]],
    --        then just use the head of each list.


    -- TODO: Reinstate drinkingProcess
    -- Process Imbiber should parse signals.
    -- Anything to the left of a <= gets declared if it isn't already in the signal list.
    -- If you declare something after you use it, TUI fills in the missing details. 
    -- When you type </proc>, TUI prompts you to fill in any details that are still missing. 
    -- Process Imbiber should automatically reset any signals you use. 


    -- TODO: Make using literals easy:
    --      * When a literal is narrower than a signal, zero pad it.
    --          * Right justify by default
    --          * Provide option to left justify
    --      * When a literal is wider than a signal, 
    --          * Raise an error (default), or
    --          * Bite off as many bits as signal can accept. 
    --          * Warning is optional if bite off enabled.


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

-- TODO: Think about how I might support infinite recursion in entities. 
-- I should be able to do something like this:
--      take 4872 myEntityList
-- and the first 4871 instances contain an instance of the same thing.


-- TODO: Write commands for making nested entities.
-- It should be easy to distinguish a child entity from a peer entity. 


-- TODO: Think about whether recursive generator state is really necessary.

-- TODO: Refactor parent & child entity structure so that it's capable of representing trees.
--
--      Would [[Entity]] work? 
--
-- Perhaps it's smarter to only store the top level entity and the present entity. 
-- That could create difficulty though because drilling down through deep state could get cumbersome. 
-- Can a recursive function find a match, modify that 1 layer, then leave the rest of the data structure
-- intact?

-- Go through all modules and explicity explort things. 
-- Avoiding name collisions is starting to be a pain. 

-- TODO: Add a subsume command. It should create a new entity which absorbs the present entity.



