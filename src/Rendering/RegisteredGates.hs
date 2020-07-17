module Rendering.RegisteredGates where
import Rendering.ClkRst
import Rendering.InfoTypes
import Rendering.GlueSigNames
import Rendering.RegGateOutputPack
import Tools.ListTools
import Tools.MathTools
import Tools.XOrMore
import Tools.WhiteSpaceTools
import qualified Data.Text as T
import qualified Text.Printf as Pf

-- TODO: Add package declaration to output file

----------------------------------------------------------------------------------------------------
--                             Generating Trees of Registered Gates
----------------------------------------------------------------------------------------------------

glue' :: Information -> Information -> [Information] -> [Information] -> String
glue' clk rst sigList results = (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results))


-- Generates the name of a component that will be used in the tree. For example:
--      registeredAND1
num2RegGate :: String -> Integer -> String
num2RegGate gateName numInputs = "registered" ++ T.unpack(T.toUpper (T.pack gateName)) ++ (show numInputs)

-- Generates 1 line of port map. For example:
--      registeredAND1 port map (clk, rst, a, x);
oneLine :: String -> Integer -> Information -> Information -> [Information] -> [Information] -> [String]
oneLine gateName numInputs clk rst sigList results = 
    [(num2RegGate gateName numInputs) ++ " port map (" ++ (glue' clk rst sigList results) ++ ");"]


inputsPerGate :: Int
inputsPerGate = 3

-- This function bites off up to 3 signals from the list of inputs, routes them to a 
-- component (such as registeredNOR3), takes 1 signal from the results list and routes
-- it to the output of the component (in this example, a 3-input NOR gate), and recurses
-- until all inputs have been consumed. 
regGateMap :: String -> Information -> Information -> [Information] -> [Information] -> [String]
regGateMap gateName clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = oneLine gateName 1 clk rst sigList results
    | (length sigList) == 2 = oneLine gateName 2 clk rst sigList results
    | otherwise = (oneLine gateName 3 clk rst sigList results) ++ 
                  (regGateMap gateName clk rst (skipN sigList 3) (tail results))

-- TODO: Refactor regGateMap so that inputsPerGate can be anything.

-- We can free ourselves from needing to know the exact number of intermediate signals
-- by creating an infinite sequence. Note that since this list is infinite, you cannot
-- rely on the 6 digit numbers staying 6 digits - unless you can guarantee that 
-- whatever this list is zipped with has less than 1M things. 
intermediateSigs :: String -> Int -> [Information]
intermediateSigs nameStub layerNum = 
    [easyClockedSL s | s <- map (\n -> Pf.printf "%s_layer_%06d_output_%06d" nameStub layerNum (n :: Int)) [0..]]


makeOneGateLayer :: String -> Information -> Information -> [Information] -> String -> Int -> [String]
makeOneGateLayer gateName clk rst sigList resultNameStub layerNum =
    regGateMap gateName clk rst sigList (intermediateSigs resultNameStub layerNum)


zeroOrOne :: Double -> Double
zeroOrOne q
    | (approxEqual (fromIntegral (floor q)) q) = 0 
    | otherwise = 1


-- Calculate the number of outputs a layer will need:
--numOutputs :: (RealFrac a, Ord a, Num a, Num b, RealFrac b) => a -> a -> b
numOutputs :: Int -> Int -> Int
numOutputs inputSigs inputsPerLayer
    | (inputSigs <= inputsPerLayer) = 1
    | otherwise = round(fromIntegral(floor(quotient)) + (zeroOrOne quotient)) where
        quotient = ((fromIntegral inputSigs) / (fromIntegral inputsPerLayer)) :: Double


fullServiceRegGate :: String -> ClkRst -> [Information] -> Int -> Maybe RegGateOutputPack
fullServiceRegGate _ _ [] _ = Nothing
fullServiceRegGate gateName (ClkRst clk rst) sigList layerNum =
    Just RegGateOutputPack {
        vhdLines = makeOneGateLayer gateName clk rst newSignals gateName layerNum
    ,   allSignals = take nOutputs newSignals
    ,   myLayerNum = layerNum
    ,   outputSignals = take nOutputs newSignals
    ,   nextLayer = oneNewLayer
    } where
        nOutputs = numOutputs (length sigList) inputsPerGate
        newSignals = intermediateSigs gateName layerNum
        oneNewLayer = 
            if (nOutputs == 1)
                then Nothing
                else fullServiceRegGate gateName (ClkRst clk rst) newSignals (layerNum+1)
            
















-- TODO: PICK UP HERE: Flesh this out



































-- TODO: Refactor codebase to accept ClkRst in lieu of Information -> Information.

-- TODO: PICK UP HERE: Write a function that recursively calls makeOneGateLayer until 
-- length results == 1.

-- sigList is a list of signals you want ANDed together. 
-- results is a list of signals that will accept the output.
andSigs :: Information -> Information -> [Information] -> [Information] -> [String]
andSigs clk rst sigList results = regGateMap "and" clk rst sigList results


-- sigList is a list of signals you want ORed together. 
-- results is a list of signals that will accept the output.
orSigs :: Information -> Information -> [Information] -> [Information] -> [String]
orSigs clk rst sigList results = regGateMap "or" clk rst sigList results


-- sigList is a list of signals you want NANDed together. 
-- results is a list of signals that will accept the output.
nandSigs :: Information -> Information -> [Information] -> [Information] -> [String]
nandSigs clk rst sigList results = regGateMap "nand" clk rst sigList results


-- sigList is a list of signals you want NORed together. 
-- results is a list of signals that will accept the output.
norSigs :: Information -> Information -> [Information] -> [Information] -> [String]
norSigs clk rst sigList results = regGateMap "nor" clk rst sigList results


-- sigList is a list of signals you want XORed together. 
-- results is a list of signals that will accept the output.
xorSigs :: Information -> Information -> [Information] -> [Information] -> [String]
xorSigs clk rst sigList results = regGateMap "xor" clk rst sigList results




-- TODO: PICK UP HERE: Write a function that figures out the intermediate signals
-- and calls the descriptive name. 
-- 
-- TODO: Write a function that takes results from above and dumps results into 
-- a new entity.

-- TODO: Figure out (emphasis = design) how I might get all the functions in a project
-- to create component declarations and dump them all in a single library.

