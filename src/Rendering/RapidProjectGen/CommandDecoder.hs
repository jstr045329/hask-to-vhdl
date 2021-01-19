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
                        ,   parsedNames = parseVhd ((tokenize [intercalate " " myVhdLines]) ++ (addToVhdBody x) ++ (flattenShallow (map retrieveVhd (codeLines gS)))) IP_NoKeyword
                        })

-- TODO:
--      Move the business logic in this branch to a separate file, and/or replace with calls to the new file. 
--      Call new full file rendering.
--      Split 1 changeOneEntity call into 2.
--      Inner: Add process lines.
--      Outie: Parse names.
        ,   processUnderConstruction = [defaultProcess]} 
            

------------------------------------------------------------------------------------------------------------------------
--                                     If Drinking A Process, Drink The Process 
-- 
-- If the drinkProcess flag is set, treat this line as a sequential statement.
--
------------------------------------------------------------------------------------------------------------------------

    -- TODO: Refresh entire entity body when this branch runs. 
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


