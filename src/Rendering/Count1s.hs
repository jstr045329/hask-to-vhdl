module Rendering.Count1s where
import Rendering.InfoTypes
import qualified Text.Printf as Pf
import Tools.WhiteSpaceTools
import Tools.ListTools


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
makeOneAdderName :: Int -> String
makeOneAdderName n = Pf.printf "AddTwoNumbers_%04d_bits_in_%04d_bits_out" n (n+1)


makeOneAdderInstName :: Int -> String
makeOneAdderInstName n = Pf.printf "ADD_TWO_NUMBERS_INST_%08d" n


mapOneAdderInput :: Information -> Int -> String
mapOneAdderInput someSig portNum = (tab 1) ++ "a" ++ (show portNum) ++ " => " ++ (nomen someSig) ++ ","


-- Handles the recusive part of mapping adder inputs.
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

--------------------------------------------------
--    Step 3: Figure Out Number of Layers and
--      Intermediate Signals in Each Layer
--------------------------------------------------


