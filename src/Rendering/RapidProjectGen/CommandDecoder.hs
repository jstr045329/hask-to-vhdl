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
import Rendering.RapidProjectGen.UpdateVhdRendering
import Rendering.RapidProjectGen.ExtractParsedNames
import Rendering.FilterUnique
import Rendering.RapidProjectGen.PresentEntity


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
    let tokList = words oneStr -- TODO: Change words to guaranteeWhiteSpace and see if that breaks anything
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
        ,   dataType = decodeGenericType tokList -- defaultDataType generatorState
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
        ,   dataType = decodeGenericType tokList -- defaultDataType generatorState
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
--                                         Purge Outdated Ports and Signals 
------------------------------------------------------------------------------------------------------------------------
purgeDecBrains :: [Information] -> [Information]
purgeDecBrains [] = []
purgeDecBrains infoList = 
    [x | x <- infoList, not (elem (nomen x) [nomen y | y <- purgeDecBrains (tail infoList)])] ++ 
    (purgeDecBrains (tail infoList))


purgeDeclarations :: GeneratorState -> GeneratorState
purgeDeclarations gS = gS
--    changePresentEntity 
--        (\x -> x {
--            aggInputs =  purgeDecBrains (aggInputs x)
--        ,   aggOutputs = purgeDecBrains (aggOutputs x) 
--        ,   aggSignals = purgeDecBrains (aggSignals x)
--        ,   aggGenerics = purgeDecBrains (aggGenerics x)
--        ,   parsedInputs = [y | y <- (parsedInputs x), not (elem (nomen y) (map nomen ((generics x) ++ (ports x) ++ (signals x))))]
--        ,   parsedOutputs = [y | y <- (parsedOutputs x), not (elem (nomen y) (map nomen ((generics x) ++ (ports x) ++ (signals x))))]
--        ,   parsedSignals = [y | y <- (parsedSignals x), not (elem (nomen y) (map nomen ((generics x) ++ (ports x) ++ (signals x))))]
--        })
--        gS


removeDuplicates :: GeneratorState -> GeneratorState
removeDuplicates gS = 
    changePresentEntity
        (\x -> x {
            aggInputs =  filterUnique (aggInputs x)
        ,   aggOutputs = filterUnique (aggOutputs x) 
        ,   aggSignals = filterUnique (aggSignals x)
        ,   aggGenerics = filterUnique (aggGenerics x)
        })
        gS


addDefaultClk :: GeneratorState -> GeneratorState
addDefaultClk gS
    | (elem (defaultClk gS) myInputList) = gS
    | otherwise = 
        changePresentEntity 
            (\x -> x {
                aggInputs = (aggInputs x) ++ [defaultClk gS]
            })
        gS where
            myInputList = (getInputPorts (aggInputs (getPresentEntity gS)))

addDefaultRst :: GeneratorState -> GeneratorState
addDefaultRst gS 
    | (elem (defaultRst gS) myInputList) = gS
    | otherwise = 
        changePresentEntity 
            (\x -> x {
                aggInputs = (aggInputs x) ++ [defaultRst gS]
            })
        gS where
            myInputList = (getInputPorts (aggInputs (getPresentEntity gS)))


addClkAndReset :: GeneratorState -> GeneratorState
addClkAndReset gS 
    | ((renderDefaultClk gS) && (renderDefaultRst gS)) = addDefaultRst (addDefaultClk gS)
    | (renderDefaultClk gS) = addDefaultClk gS
    | (renderDefaultRst gS) = addDefaultRst gS
    | otherwise = gS
        

------------------------------------------------------------------------------------------------------------------------
--                                   Wrap Up Everything That Happens Every Command 
--
-- Anything and everything you want to happen when the enter key is pressed should go here. 
--
------------------------------------------------------------------------------------------------------------------------
statusQuoFunctions :: [(GeneratorState -> GeneratorState)]
statusQuoFunctions = [
        updateVhdRendering
    ,   updateProcessOutputs
    ,   putInfoInPresentEntity
    ,   addClkAndReset
--    ,   purgeDeclarations
    ]


applyFuncList :: [(GeneratorState -> GeneratorState)] -> GeneratorState -> GeneratorState
applyFuncList [] gS = gS
applyFuncList funcList gS = applyFuncList (tail funcList) ((head funcList) gS)


updateGsBrains :: GeneratorState -> Int -> GeneratorState
updateGsBrains gS n
    | (n <= 0) = gS
    | otherwise = updateGsBrains (applyFuncList statusQuoFunctions gS) (n-1) -- updateGsBrains (updateVhdRendering (updateProcessOutputs (putInfoInPresentEntity gS))) (n-1)


updateGs :: GeneratorState -> GeneratorState
updateGs gS = updateGsBrains gS 2


setDefaultDataType :: String -> DataType
setDefaultDataType "sl" = StdLogic
setDefaultDataType "slv" = StdLogicVector
setDefaultDataType "sul" = StdULogic
setDefaultDataType "sulv" = StdULogicVector
setDefaultDataType "signed" = Signed
setDefaultDataType "unsigned" = Unsigned
setDefaultDataType "sgn" = Signed
setDefaultDataType "uns" = Unsigned
setDefaultDataType "unsgn" = Unsigned
setDefaultDataType "int" = UnconstrainedInt
setDefaultDataType _ = StdLogicVector


-- Note: If you don't want an empty string to return True, call this from a wrapper that 
-- returns False if you pass in an empty string. 
stringIsInt :: String -> Bool
stringIsInt "" = True
stringIsInt s
    | (elem (head s) "0123456789") = stringIsInt (tail s)
    | otherwise = False


setDefaultWidth :: String -> Width
setDefaultWidth s
    | (s == "") = Hard 32
    | (stringIsInt s) = Hard (read s :: Integer)
    | otherwise = Soft s


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
    | (startsWith s "</proc>") = updateGs
                                    ((changePresentEntity
                                    (\x -> x{ 
                                            processes = (processes x) ++ (processUnderConstruction gS)
                                    }) gS) {
                                            drinkProcess = False
                                        ,   processUnderConstruction = [defaultProcess]
                                        })


------------------------------------------------------------------------------------------------------------------------
--                                     If Drinking A Process, Drink The Process 
-- 
-- If the drinkProcess flag is set, treat this line as a sequential statement.
--
------------------------------------------------------------------------------------------------------------------------

    -- TODO: Refresh entire entity body when this branch runs. 
    | (drinkProcess gS) = updateGs (gS { processUnderConstruction = [(head (processUnderConstruction gS)) {procPlainLines = (procPlainLines (head (processUnderConstruction gS))) ++ (nZipTab 2 [s])}]})


------------------------------------------------------------------------------------------------------------------------
--                                              Check If s Is A Command 
------------------------------------------------------------------------------------------------------------------------
    | (startsWith s "gen ") = updateGs (gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { generics = (generics x) ++ [makeOneNewGeneric (skipN s 4) gS]})})

    | (startsWith s "sig ") = updateGs (gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { signals = (signals x) ++ [makeOneNewSignal (skipN s 4) gS]})})

    | (startsWith s "in ") = updateGs (gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { ports = (ports x) ++ [makeOneNewPort (skipN s 3) gS True]})})

    | (startsWith s "out ") = updateGs (gS { entTree = changeOneEntity (gPEnt gS) (entTree gS) (\x -> x { ports = (ports x) ++ [makeOneNewPort (skipN s 4) gS False]})})


    | (startsWith s "type=") = gS { defaultDataType = setDefaultDataType (skipN s 5)}
    | (startsWith s "type =") = gS { defaultDataType = setDefaultDataType (skipN s 6)}

    | (startsWith s "width=") = gS { defaultWidth = setDefaultWidth (skipN s 6)}
    | (startsWith s "width =") = gS { defaultWidth = setDefaultWidth (skipN s 7)}

    -- Start Fast Process Imbiber:
    | (startsWith s "<proc>") = (updateGs gS) { 
            drinkProcess = True
        ,   processUnderConstruction = 
                if ((length (words s)) > 1)
                    then [defaultNamedProcess ((words s) !! 1)]
                    else [defaultProcess]}

    -- u and up move up 1 layer in hierarchy:
    | (s == "u") = gS { pathToPresent = dropLast (pathToPresent gS)}
    | (s == "up") = gS { pathToPresent = dropLast (pathToPresent gS)}

    | (s == "noclk") = gS { renderDefaultClk = False}
    | (s == "norst") = gS { renderDefaultRst = False}
    | (s == "yesclk") = gS { renderDefaultClk = True}
    | (s == "yesrst") = gS { renderDefaultRst = True}



    -- dn <child_entity_name> moves down to the child entity:
    | (startsWith s "dn ") = 
        if ((length (words s)) < 2)
            then gS
            else drillDownOneLayer ((words s) !! 1) gS


------------------------------------------------------------------------------------------------------------------------
--                           If s Is None of The Above, It Must Be A Concurrent Statement 
------------------------------------------------------------------------------------------------------------------------
    | otherwise = updateGs (appendVhd s gS) where
        myEntity = head (fetchOneEntity (gPEnt gS) (entTree gS))




