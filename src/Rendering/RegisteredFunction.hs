------------------------------------------------------------------------------------------------------------------------
--                                           Pipeline Arbitrary Functions
--
-- This module accepts a list of inputs and turns them into a registered pipeline, using an arbitrary combinational
-- function. The function must be overloaded to accept 1 to lutInputsRecommended inputs, where lutInputsRecommended 
-- is defined in CodeGenerationParameters.hs.
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RegisteredFunction where
import DataStructures.TreeOfTuples
import Rendering.InfoTypes
import Tools.ListTools
import Rendering.InfoNameTools


------------------------------------------------------------------------------------------------------------------------
--                                        Assign Layer 0 Signals With Inputs
--
-- The signals already get generated by homogenousSignals, so we just need a list of strings to assign them.
--
------------------------------------------------------------------------------------------------------------------------
registerInputs :: Int -> String -> [String]
registerInputs numInputs nameStub = assignmentStrings where
    tupTree = makeTupleTree numInputs 0
    inputList = makeInList tupTree
    layer0Inputs = [x | x <- inputList, isLayer0 x]
    sigNames = map (\oneTup -> tupleToSignalName nameStub oneTup) layer0Inputs
    inputNames = map (\s -> sigName2InputName s) sigNames
    assignmentStrings = map (\(s1, s2) -> s1 ++ " <= " ++ s2 ++ ";") (zip sigNames inputNames)
    

------------------------------------------------------------------------------------------------------------------------
--                                         Same Function Applied Everywhere
--
-- exampleSig can be any Port or Signal. (Technically it can be any Information, but there's no reason to make it a 
-- constant, generic, etc.) Note that if the name of exampleSig ends with _000000 (or some other postfix number), that 
-- number will be discarded during name stub extraction. The other big difference is that exampleSig is nominally an 
-- input port, whereas generated signals are signals. In other respects, the generated signals will be like exampleSig. 
--
-- To declare the signals, call homogenousSignals.
--
-- To write the code, call homogenousCode.
--
------------------------------------------------------------------------------------------------------------------------
homogenousSignals :: Int -> Information -> [Information]
homogenousSignals numInputs exampleSig = results where
    nameStub = extractNameStub (nomen exampleSig)
    tupTree = makeTupleTree numInputs 0
    tupList = flattenShallow tupTree
    sigNames = map (\oneTup -> tupleToSignalName nameStub oneTup) tupList
    results = map (\oneName -> 
        VhdSig {
                nomen = oneName
            ,   dataType = dataType exampleSig
            ,   width = width exampleSig
            ,   sDefault = sDefault exampleSig
            ,   clocked = clocked exampleSig
            ,   comments = comments exampleSig
            ,   sReset = sReset exampleSig
            ,   assertionLevel = assertionLevel exampleSig
            }) sigNames


homogenousCode :: Int -> String -> String -> [String]
homogenousCode numInputs nameStub funcName = 
    (registerInputs numInputs nameStub) ++ (consumeChunkOfInputs funcName nameStub inputList outputList) where 
    tupTree = makeTupleTree numInputs 0
    inputList = makeInList tupTree
    outputList = makeOutputList tupTree


------------------------------------------------------------------------------------------------------------------------
--                                Pass In A List Of Function Names, One For Each Layer
--
-- The input lists are for 1) a list of function names for each signal and 2) an example signal for each layer. Name
-- stubs are extract from example signals. Any numbers at the end of the example signal will be stripped off. The
-- name of example 0 will be forced to input format (i.e. prefix will be changed to i_ if it isnt already) in order
-- to drive layer 0 signals, which of course are still signals.
--
------------------------------------------------------------------------------------------------------------------------
heterogenousSignals :: Int -> [Information] -> [Information]
heterogenousSignals numInputs exampleSigList = results where
    tupTree = makeTupleTree numInputs 0
    tupList = flattenShallow tupTree
    inflatedLayerNumbers = [getLayerNum oneTup | oneTup <- tupList] -- List of Int's showing which layer each signal is in.
    nameStubList = [extractNameStub (nomen (exampleSigList !! oneLayerNum)) | oneLayerNum <- inflatedLayerNumbers]
    nameList = map (\(oneTup, nameStub) -> tupleToSignalName nameStub oneTup) (zip tupList nameStubList)
    inflatedSignalList = [exampleSigList !! oneLayerNum | oneLayerNum <- inflatedLayerNumbers]
    results = map (\(oneName, exampleSig) -> 
        VhdSig {
                nomen = oneName
            ,   dataType = dataType exampleSig
            ,   width = width exampleSig
            ,   sDefault = sDefault exampleSig
            ,   clocked = clocked exampleSig
            ,   comments = comments exampleSig
            ,   sReset = sReset exampleSig
            ,   assertionLevel = assertionLevel exampleSig
            }) (zip nameList inflatedSignalList)


heterogenousCode' :: Int -> [String] -> [Information] -> [(Int, Int)] -> [(Int, Int)] -> Int -> [String]
heterogenousCode' _ [] _ _ _ _ = []
heterogenousCode' _ _ [] _ _ _ = []
heterogenousCode' numInputs funcNameList exampleSigList inputList outputList layerNum
    | ((length exampleSigList) == 1) = []
    | otherwise = layer0List ++ (heteroChunkOfInputs funcName nameStubSrc nameStubDest inputList' outputList') ++ anyRecursion where 
        layer0List = 
            if (layerNum == 0)
                then (registerInputs numInputs nameStubSrc)
                else []
        nameStubSrc = nomen (exampleSigList !! 0)
        nameStubDest = nomen (exampleSigList !! 1)
        inputList' = take (findLayerChange inputList) inputList
        outputList' = take (findLayerChange outputList) outputList
        inputList'' = skipN inputList (findLayerChange inputList) 
        outputList'' = skipN outputList (findLayerChange outputList) 
        funcName = head funcNameList
        anyRecursion = heterogenousCode' numInputs (tail funcNameList) (tail exampleSigList) inputList'' outputList'' (layerNum+1)


heterogenousCode :: Int -> [String] -> [Information] -> [String]
heterogenousCode _ [] _ = []
heterogenousCode _ _ [] = []
heterogenousCode numInputs funcNameList exampleSigList = 
    heterogenousCode' numInputs funcNameList exampleSigList inputList outputList 0 where 
    tupTree = makeTupleTree numInputs 0
    inputList = makeInList tupTree
    outputList = makeOutputList tupTree

