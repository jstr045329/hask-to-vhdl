------------------------------------------------------------------------------------------------------------------------
--                                                  Tree of Tuples
--
-- This module contains tools for generating and working with a tree in which each node has lutInputsRecommended
-- children (where lutInputsRecommended is defined in Rendering.CodeGenerationParameters). Each node contains a tuple
-- (signalNumber, layerNumber). These trees are useful for generating pipelined circuits that aggregate information.
--
------------------------------------------------------------------------------------------------------------------------
module DataStructures.TreeOfTuples where 
import Rendering.CodeGenerationParameters
import Text.Printf
import Tools.StringTools
import Tools.ListTools


------------------------------------------------------------------------------------------------------------------------
--                           Given Some Number of Inputs, Calculate the Number of Outputs
--
-- This function doesn't care what the signals represent. It only cares about that fact that in a layer of tree,
-- you take N inputs, and map those to ceiling(N/lutInputsRecommended) outputs. This calculates 
-- ceiling(N/lutInputsRecommended).
--
------------------------------------------------------------------------------------------------------------------------
outputNum :: Int -> Int
outputNum nInputs = 
    (div nInputs lutInputsRecommended) + 
    if ((mod nInputs lutInputsRecommended) > 0)
        then 1
        else 0


------------------------------------------------------------------------------------------------------------------------
--                                               Make a Tree of Tuples
--
-- The first parameter is how many inputs you want to feed in. The second parameter is the layer number of tree we
-- are building in a particular call. Set this to 0 when initiating the function. The output is a list of tuples
-- (Int, Int) that define how signals are combined into a tree. In a tuple (a, b), a is the signal number within
-- a particular layer, and b is the layer number. Because b also defines the delay relative to the inputs, this tuple
-- format conforms with the convention of this library that the final _000XYZ specifies the delay relative to some
-- input.
--
------------------------------------------------------------------------------------------------------------------------
makeTupleTree :: Int -> Int -> [[(Int, Int)]]
makeTupleTree numInputs layerNum 
    | (numInputs <= 0) = [] 
    | otherwise = [rawInputList] ++ [oneNewList] ++ anyRecursion where
        increaseAmount =
            if (layerNum == 0)
                then 1
                else 0
        rawInputList = 
            if (layerNum == 0)
                then [(x, 0) | x <- take numInputs [0..]]
                else []
        oneNewList = [(oneInputNum, layerNum + increaseAmount) | oneInputNum <- take (outputNum numInputs) [0..]]
        anyRecursion = 
            if (length oneNewList <= 1)
                then []
                else (makeTupleTree (length oneNewList) (layerNum + increaseAmount + 1))


------------------------------------------------------------------------------------------------------------------------
--                                       Convert One Tuple Into A Signal Name
------------------------------------------------------------------------------------------------------------------------
tupleToSignalName :: String -> (Int, Int) -> String
tupleToSignalName nameStub (a, b) = printf "s_%s_%06d_%06d" nameStub a b


------------------------------------------------------------------------------------------------------------------------
--                                Map One Chunk Of Inputs To One Output - Homo Style
--
-- This version accepts only 1 name stub, because the function being applied is homogenous. Get your head out of
-- the toilet, you pervert.
--
------------------------------------------------------------------------------------------------------------------------
mapChunkOfInputsToOneOutput :: String -> String -> [(Int, Int)] -> (Int, Int) -> String
mapChunkOfInputsToOneOutput _ _ [] _ = []
mapChunkOfInputsToOneOutput funcName nameStub inputList oneOutput = 
    outputStr ++ " <= " ++ funcName ++ "(" ++ paramList ++ ");" where 
        rawParamList = [tupleToSignalName nameStub oneTup | oneTup <- inputList]
        paramList = joinStringsWithCommas rawParamList
        outputStr = tupleToSignalName nameStub oneOutput


------------------------------------------------------------------------------------------------------------------------
--                               Map One Chunk Of Inputs To One Output - Hetero Style
--
-- This version accepts 2 name stubs, because the function being applied is heterogenous.
--
------------------------------------------------------------------------------------------------------------------------
heteroChunkOfInputsToOneOutput :: String -> String -> String -> [(Int, Int)] -> (Int, Int) -> String
heteroChunkOfInputsToOneOutput _ _ _ [] _ = []
heteroChunkOfInputsToOneOutput funcName nameStubSrc nameStubDest inputList oneOutput = 
    outputStr ++ " <= " ++ funcName ++ "(" ++ paramList ++ ");" where 
        rawParamList = [tupleToSignalName nameStubSrc oneTup | oneTup <- inputList]
        paramList = joinStringsWithCommas rawParamList
        outputStr = tupleToSignalName nameStubDest oneOutput


------------------------------------------------------------------------------------------------------------------------
--                                     Find Location Where Layer Number Changes
--
-- Pass in idx = 0 to initiate this function. findLayerChange' is the brains of this operation, but you probably
-- want to call findLayerChange because it's a nicer wrapper.
--
------------------------------------------------------------------------------------------------------------------------
findLayerChange' :: [(Int, Int)] -> Int -> Int -> Int
findLayerChange' tupList idx prevIdx
    | (tupList == []) = idx
    | ((length tupList) == 1) = idx+1
    | otherwise = 
        if (nextLayer == thisLayer)
            then findLayerChange' (tail tupList) (idx+1) (idx)
            else (idx+1) where
        (_, thisLayer) = head tupList
        (_, nextLayer) = tupList !! 1


findLayerChange :: [(Int, Int)] -> Int
findLayerChange tupList = findLayerChange' tupList 0 0


------------------------------------------------------------------------------------------------------------------------
--                                        Consume Chunk of Inputs Recursively
--
-- This function consumes lutInputsRecommended and maps those to 1 output, recursively, until all inputs are consumed.
-- Another way to think of it is that this function generates 1 layer of tree. This function is easier to use if
-- you remember that inputList and outputList overlap. Only the final output should not be in inputList, and only
-- the layer 0 inputs should not be in outputList.
--
------------------------------------------------------------------------------------------------------------------------
consumeChunkOfInputs :: String -> String -> [(Int, Int)] -> [(Int, Int)] ->  [String]
consumeChunkOfInputs _ _ [] _ = []
consumeChunkOfInputs _ _ _ [] = []
consumeChunkOfInputs funcName nameStub inputList outputList =
    [oneAssignment] ++ anyRecursion where 
    stopNum = min lutInputsRecommended (findLayerChange inputList)
    oneAssignment = mapChunkOfInputsToOneOutput funcName nameStub (take stopNum inputList) (head outputList)
    anyRecursion = consumeChunkOfInputs funcName nameStub (skipN inputList stopNum) (tail outputList)


------------------------------------------------------------------------------------------------------------------------
--                               Consume Chunk of Inputs With Heterogenous Name Stubs
--
-- This version is like the (homo) one above, but accepts different name stubs for input and output lists.
--
------------------------------------------------------------------------------------------------------------------------
heteroChunkOfInputs :: String -> String -> String -> [(Int, Int)] -> [(Int, Int)] ->  [String]
heteroChunkOfInputs _ _ _ [] _ = []
heteroChunkOfInputs funcName nameStubSrc nameStubDest inputList outputList =
    [oneAssignment] ++ anyRecursion where 
    stopNum = min lutInputsRecommended (findLayerChange inputList)
    oneAssignment = heteroChunkOfInputsToOneOutput funcName nameStubSrc nameStubDest (take stopNum inputList) (head outputList)
    anyRecursion = 
        if ((length inputList) <= lutInputsRecommended)
            then []
            else heteroChunkOfInputs funcName nameStubSrc nameStubDest (skipN inputList stopNum) (tail outputList)


------------------------------------------------------------------------------------------------------------------------
--                                       Make inList for consumeChunkOfInputs
--
-- This function takes the list of tuples from makeTupleTree and turns it into the intput list. The only tuple that
-- should not be an input is the final output. Also works for heteroChunkOfInputs.
--
------------------------------------------------------------------------------------------------------------------------
makeInList :: [[(Int, Int)]] -> [(Int, Int)]
makeInList listOfLists
    | ((length listOfLists) <= 1) = []
    | otherwise = (head listOfLists) ++ (makeInList (tail listOfLists))


------------------------------------------------------------------------------------------------------------------------
--                                         Determine If Function Is Layer 0
--
-- This will be useful for generating the output list.
------------------------------------------------------------------------------------------------------------------------
isLayer0 :: (Int, Int) -> Bool
isLayer0 (_, x) = (x == 0)


------------------------------------------------------------------------------------------------------------------------
--                                          Extract Layer Number From Tuple 
------------------------------------------------------------------------------------------------------------------------
getLayerNum :: (Int, Int) -> Int
getLayerNum (_, x) = x


------------------------------------------------------------------------------------------------------------------------
--                                         Extract Signal Number From Tuple 
------------------------------------------------------------------------------------------------------------------------
getSignalNum :: (Int, Int) -> Int
getSignalNum (x, _) = x


------------------------------------------------------------------------------------------------------------------------
--                                       Make outList for consumeChunkOfInputs
--
-- This function takes the list of tuples from makeTupleTree and turns it into the output list. The only tuples that
-- should not be outputs are layer 0. Also works for heteroChunkOfInputs.
--
------------------------------------------------------------------------------------------------------------------------
makeOutputList :: [[(Int, Int)]] -> [(Int, Int)]
makeOutputList [] = []
makeOutputList listOfLists = newResults ++ anyRecursion where 
    newResults = [oneTup | oneTup <- (head listOfLists), not (isLayer0 oneTup)]
    anyRecursion = makeOutputList (tail listOfLists)

