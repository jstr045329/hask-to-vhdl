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
    [instLabel ++ (num2RegGate gateName numInputs') ++ " port map (" ++ (glue' clk rst sigList results) ++ ");\n"] where 
    numInputs' = (min numInputs (toInteger (length sigList)))
    instLabel = "INST_" ++ (nomen (head results)) ++ ": "
    

inputsPerGate :: Int
inputsPerGate = 6


-- This function bites off up to inputsPerGate signals from the list of inputs, routes them to a 
-- component (such as registeredNOR3), takes 1 signal from the results list and routes
-- it to the output of the component (in this example, a 3-input NOR gate), and recurses
-- until all inputs have been consumed. 
regGateMap :: String -> Information -> Information -> [Information] -> [Information] -> [String]
regGateMap gateName clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = oneLine gateName 1 clk rst sigList results
    | (length sigList) == 2 = oneLine gateName 2 clk rst sigList results
    | (length sigList) == 3 = oneLine gateName 3 clk rst sigList results
    | (length sigList) == 4 = oneLine gateName 4 clk rst sigList results
    | otherwise = (oneLine gateName (fromIntegral inputsPerGate) clk rst (take inputsPerGate sigList) results) ++ 
                  (regGateMap gateName clk rst (skipN sigList inputsPerGate) (tail results)) 
                  

-- We can free ourselves from needing to know the exact number of intermediate signals
-- by creating an infinite sequence. Note that since this list is infinite, you cannot
-- rely on the 6 digit numbers staying 6 digits - unless you can guarantee that 
-- whatever this list is zipped with has less than 1M things. 
intermediateSigs :: String -> Int -> [Information]
intermediateSigs nameStub layerNum = 
    [easyClockedSL s | s <- map (\n -> Pf.printf "%s_%04d_%04d" nameStub layerNum (n :: Int)) [0..]]


-- Calculate the number of outputs a layer will need:
numOutputs :: Int -> Int -> Int
numOutputs inputSigs inputsPerLayer
    | (inputSigs <= inputsPerLayer) = 1
    | otherwise = round(fromIntegral(floor(quotient)) + (zeroOrOne quotient)) where
        quotient = ((fromIntegral inputSigs) / (fromIntegral inputsPerLayer)) :: Double


makeOneGateLayer :: String -> Information -> Information -> [Information] -> String -> Int -> [String]
makeOneGateLayer gateName clk rst sigList resultNameStub layerNum =
    regGateMap gateName clk rst sigList (take nOutputs (intermediateSigs resultNameStub (layerNum+1))) where
        nOutputs = numOutputs (length sigList) inputsPerGate


-- If a number is within a few billionths of an integer, return 0.
-- Otherwise, return 1. 
-- This is useful because we need to decide whether to add more gates to layer, or not. 
zeroOrOne :: Double -> Double
zeroOrOne q
    | (approxEqual (fromIntegral (floor q)) q) = 0 
    | otherwise = 1


fullServiceRegGate :: String -> ClkRst -> [Information] -> Int -> RegGateOutputPack
fullServiceRegGate _ _ [] _ = TerminateRegGate
fullServiceRegGate gateName (ClkRst clk rst) sigList layerNum =
    RegGateOutputPack {
        vhdLines = makeOneGateLayer gateName clk rst sigList gateName (layerNum-1)
    ,   allSignals = take nOutputs newSignals
    ,   myLayerNum = layerNum
    ,   outputSignals = take nOutputs newSignals
    ,   nextLayer = oneNewLayer
    } where
        nOutputs = numOutputs (length sigList) inputsPerGate
        newSignals = take nOutputs (intermediateSigs gateName layerNum)
        oneNewLayer = 
            if (nOutputs == 1)
                then TerminateRegGate
                else fullServiceRegGate gateName (ClkRst clk rst) newSignals (layerNum+1)
 

----------------------------------------------------------------------------------------------------
--                           Descriptive Wrappers for Registered Gates
--
-- These functions give you easy-to-use names for fullServiceRegGate.
-- They also initialize layerNum to 0 so that's 1 less thing to type.
--
-- In these functions:
--      * inputList is a list of signals you want gated together. 
--      * clkRst is an instance of ClkRst, which wraps up your clock and your reset. 
--        See ClkRst.hs
--
-- After calling one of these functions, pass the result to renderFullService,
-- below, to get the rendered VHDL. 
--
----------------------------------------------------------------------------------------------------
andSigs :: ClkRst -> [Information] -> RegGateOutputPack
andSigs clkRst inputList = fullServiceRegGate "and" clkRst inputList 0


nandSigs :: ClkRst -> [Information] -> RegGateOutputPack
nandSigs clkRst inputList = fullServiceRegGate "nand" clkRst inputList 0


orSigs :: ClkRst -> [Information] -> RegGateOutputPack
orSigs clkRst inputList = fullServiceRegGate "or" clkRst inputList 0


xorSigs :: ClkRst -> [Information] -> RegGateOutputPack
xorSigs clkRst inputList = fullServiceRegGate "xor" clkRst inputList 0


norSigs :: ClkRst -> [Information] -> RegGateOutputPack
norSigs clkRst inputList = fullServiceRegGate "nor" clkRst inputList 0


------------------------------------------------------------------------------------------------------------------------
--                                            Call This To Generate Code 
--
-- Pass a RegGateOutputPack (above) into this function, and it will recursively glean the VHDL from every layer of 
-- RegGateOutputPack.
--  
------------------------------------------------------------------------------------------------------------------------
renderFullService :: RegGateOutputPack -> [String]
renderFullService TerminateRegGate = []
renderFullService oneRegGate = (vhdLines oneRegGate) ++ renderFullService (nextLayer oneRegGate)


------------------------------------------------------------------------------------------------------------------------
--                                 Call This To Generate A Complete List Of Signals 
-- 
-- This function takes the number of inputs going into the 0th layer and nibbles off the correct amount of intermediate
-- signals from each layer until the output layer contains exactly 1 signal. 
--
------------------------------------------------------------------------------------------------------------------------
totalSignalList :: String -> Int -> Int -> [Information]
totalSignalList nameStub layerNum inputSigs 
    | (inputSigs == 1) = (take 1 (intermediateSigs nameStub layerNum))
    | otherwise = 
        (take inputSigs (intermediateSigs nameStub layerNum)) ++ 
        (totalSignalList nameStub (layerNum+1) (numOutputs inputSigs inputsPerGate))

