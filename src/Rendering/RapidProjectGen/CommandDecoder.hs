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
import Rendering.RapidProjectGen.AppendOneLine
import Rendering.Process
import Rendering.Statement
import Tools.WhiteSpaceTools
import Rendering.RapidProjectGen.DrillDown
import Parsing.SourceSinkParser
import Parsing.InputParsingKeywords
import Data.List


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


-- Extract VHDL from InterspersedCode:
retrieveVhd :: InterspersedCode -> [String]
retrieveVhd (InterspersedCode (VhdLiteral los) _ _) = los
retrieveVhd _ = []

------------------------------------------------------------------------------------------------------------------------
--                                           Update the State of Present Entity
------------------------------------------------------------------------------------------------------------------------
slurpCommand :: String -> GeneratorState -> GeneratorState
slurpCommand s gS 


------------------------------------------------------------------------------------------------------------------------
--                                   Check If User Wants To Terminate Fast Process 
--
-- If so:
--      1) Append process under construction to the present entity's list of processes, and 
--      2) Set GeneratorState's processUnderConstruction to an empty list.
--
------------------------------------------------------------------------------------------------------------------------
    | (startsWith s "</proc>") = gS { 
            drinkProcess = False
        ,   entTree = changeOneEntity 
                        (gPEnt gS) 
                        (entTree gS) 
                        (\x -> x {
                            processes = (processes x) ++ (processUnderConstruction gS)
                        ,   addToVhdBody = (addToVhdBody x) ++ (flattenShallow (map retrieveVhd (codeLines gS)))
                        })
        ,   processUnderConstruction = [defaultProcess]}


------------------------------------------------------------------------------------------------------------------------
--                                     If Drinking A Process, Drink The Process 
-- 
-- If the drinkProcess flag is set, treat this line as a sequential statement.
--
------------------------------------------------------------------------------------------------------------------------
    | (drinkProcess gS) =  gS { processUnderConstruction = [(head (processUnderConstruction gS)) {procPlainLines = (procPlainLines (head (processUnderConstruction gS))) ++ (nZipTab 2 [s])}]}


------------------------------------------------------------------------------------------------------------------------
--                                              Check If s Is A Command 
------------------------------------------------------------------------------------------------------------------------
    | (startsWith s "gen ") = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { generics = (generics x) ++ [makeOneNewGeneric (skipN s 4) gS]})}

    | (startsWith s "sig ") = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { signals = (signals x) ++ [makeOneNewSignal (skipN s 4) gS]})}

    | (startsWith s "in ") = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { ports = (ports x) ++ [makeOneNewPort (skipN s 3) gS True]})}

    | (startsWith s "out ") = gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { ports = (ports x) ++ [makeOneNewPort (skipN s 4) gS False]})}

    -- Start Fast Process Imbiber:
    | (startsWith s "<proc>") = gS { 
            drinkProcess = True
        ,   processUnderConstruction = 
                if ((length (words s)) > 1)
                    then [defaultNamedProcess ((words s) !! 1)]
                    else [defaultProcess]}

    -- u and up move up 1 layer in hierarchy:
    | (s == "u") = gS { pathToPresent = dropLast (pathToPresent gS)}
    | (s == "up") = gS { pathToPresent = dropLast (pathToPresent gS)}

    -- dn <child_entity_name> moves down to the child entity:
    | (startsWith s "dn ") = 
        if ((length (words s)) < 2)
            then gS
            else drillDownOneLayer ((words s) !! 1) gS

------------------------------------------------------------------------------------------------------------------------
--                           If s Is None of The Above, It Must Be A Concurrent Statement 
------------------------------------------------------------------------------------------------------------------------
    | otherwise = 
        (appendVhd s 
        (changePresentEntity
            (\x -> x {parsedNames = 
                parseVhd 
                    (tokenize [intercalate " " myVhdLines])
                    IP_NoKeyword
            })
            gS)) where
        myEntity = head (fetchOneEntity (gPEnt gS) (entTree gS))
        myVhdLines = (addToVhdBody myEntity) ++ [s]




















-- TODO: Plan the demo I want to give. Ensure that scope is in tip top shape. Nothing more.


-- TODO: For all names in VHD signals, put a o_ version in outputs, and s_ version in signals.
-- TODO: For all inputs, render name as i_.
-- TODO: Create a command that replaces raw names in VHD literals with i_, s_, and o_ variants.
--       Said command should also drive o_ names.

-- TODO: Before displaying name to TUI, first check whether user has declared a similar name.
--       If so, steal datatype, width, etc. from that name.
--       NOTE: Identical name should take precedence over similar name. That way, if user 
--       doesn't like the automatically extracted version, they can declare the exact name 
--       with whatever specs they want.

-- TODO: Take Information's as they appear on the screen and render Hs to a file.
-- TODO: Append every command to a file.















-- TODO: 
-- When user uses similar names for both input and output, Hs should declare s_ and o_ versions of the same name. 
-- Anything that is a signal in an above entity should be declared as an input - If the flag enabling that feature is set.
-- Any signal that user uses but never assigns should be declared as an input.

-- TODO: Send user a message when all inputs to an entity have a signal (or input) with a similar name in the 
-- parent entity. Say, "Perfect Input Subset". That way, user instantly knows when all information going into a module
-- has been created.

-- TODO: test user messages

-- TODO: Allow user to turn Info prefixes on & off from TUI.
-- User should be able to type:
--
--          x <= din;
--
-- and either see that, or:
--
--          s_x <= i_din;
-- ...
--          o_x <= s_x;
--
-- just by changing a command. 

-- TODO: Allow user to declare constants

