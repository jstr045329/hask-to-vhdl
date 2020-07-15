module Rendering.RegisteredGates where
import Rendering.InfoTypes
import Rendering.GlueSigNames
import Tools.ListTools
import qualified Data.Text as T
import qualified Text.Printf as Pf
import Tools.WhiteSpaceTools

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


----------------------------------------------------------------------------------------------------
--                                      The Counting of 1's
----------------------------------------------------------------------------------------------------

-- The counting of 1's is a 2-stage process:
--      1) A layer of boxes that actually count the bits, and
--      2) Additional layers that sum the results.

mapOneInput :: Information -> Int -> Bool -> [String]
mapOneInput someSig i useComma = [(tab 1) ++ "a" ++ (show i) ++ " => " ++ (nomen someSig) ++
                                 (if useComma then "," else "")]


-- Helper function for counting 1's:
countSome1s :: [Information] -> [String]
countSome1s sigList 
    | length sigList > 8 = error "Maximum 8 inputs supported at this time."
    | length sigList == 0 = []
    | length sigList == 1 = mapOneInput (head sigList) 0 True
    | otherwise = (mapOneInput (head sigList) ((length sigList)-1) True) ++ 
                  (countSome1s (tail sigList))


-- TODO: Provide option to assign generic use_async_reset
mapOne1Counter :: Information -> Information -> [Information] -> Information -> Int -> [String]
mapOne1Counter clk rst sigList oneResultSig instNum 
    | length sigList > 8 = error "Maximum 8 inputs supported at this time."
    | length sigList == 0 = []
    | otherwise = [
        "COUNT_1S_INST_" ++ (show instNum) ++ ": " ++ "count1s"
    ,   (tab 1) ++ "port map ("
    ,   (tab 2) ++ "clk => " ++ (nomen clk) ++ ","
    ,   (tab 2) ++ "reset => " ++ (nomen rst) ++ ","] ++
    (nZipTab 1 (countSome1s sigList)) ++
    [(tab 2) ++ "q => " ++ (nomen oneResultSig)] ++ 
    [(tab 1) ++ ");"] ++ 
    [""]


--------------------------------------------------
--            Step 1: Count the 1's
--------------------------------------------------
countOnes :: Information -> Information -> [Information] -> [Information] -> Int -> [String]
countOnes clk rst sigList results instNum
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) <= 8 = mapOne1Counter clk rst sigList (head results) instNum
    | otherwise = (mapOne1Counter clk rst (take 8 sigList) (head results) instNum) ++ 
                  (countOnes clk rst (skipN sigList 8) (tail results) (instNum + 1))


--------------------------------------------------
-- Step 2: Stitch 1 Counters Together with Adders
--------------------------------------------------


-- AddTwoNumbers_0002_bits_in_0003_bits_out
-- TODO: PICK UP HERE: Write a function that generates names of the above form.
makeOneAdderName :: Int -> String
makeOneAdderName n = Pf.printf "AddTwoNumbers_%04d_bits_in_%04d_bits_out" n (n+1)


makeOneAdderInstName :: Int -> String
makeOneAdderInstName n = Pf.printf "ADD_TWO_NUMBERS_INST_%08d" n


mapOneAdderInput :: Information -> Int -> String
mapOneAdderInput someSig portNum = (tab 1) ++ "a" ++ (show portNum) ++ " => " ++ (nomen someSig) ++ ","


-- Handles the mapping of 
mapAdderGuts :: [Information] -> [String]
mapAdderGuts sigList 
    | (length sigList) > 2 = error "Max 2 signals supported at this time"
    | (length sigList) == 0 = []
    | otherwise = [mapOneAdderInput (head sigList) ((length sigList)-1)] ++
                  (mapAdderGuts (tail sigList))


mapOneAdder :: Information -> Information -> [Information] -> Information -> Int -> Int -> [String]
mapOneAdder clk rst sigList oneResult bitsIn instNum
    | (length sigList) > 2 = error "Max 2 signals supported at this time."
    | (length sigList) == 0 = []
    | otherwise = 
        [(makeOneAdderInstName instNum) ++ ": " ++ (makeOneAdderName bitsIn)] ++ 
        [(tab 1) ++ "port map ("] ++ 
        (nZipTab 1 (mapAdderGuts sigList)) ++ 
        [(tab 2) ++ "q => " ++ (nomen oneResult)] ++ 
        [(tab 1) ++ ");"] ++ 
        [""]


-- sigList is a list of signals containing counts. 
-- Signals in sigList can be the result from a count1s instance, 
-- or they can be the result of another adder. 
addCounts :: Information -> Information -> [Information] -> [Information] -> Int -> Int -> [String]
addCounts clk rst sigList results bitsIn instNum
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) <= 2 = mapOneAdder clk rst sigList (head results) bitsIn instNum
    | otherwise = (addCounts clk rst (take 2 sigList) [head results] bitsIn instNum) ++ 
                  (addCounts clk rst (skipN sigList 2) (tail results) bitsIn (instNum+1))

-- TODO: Review this file and look for design patterns I can boil down
-- TODO: Write algorithms that figure out the intermediate signals for you.


