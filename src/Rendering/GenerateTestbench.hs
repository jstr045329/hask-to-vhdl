module Rendering.GenerateTestbench where
import Parsing.TokenMatchingTools
import Parsing.ExtractEntity
import Tools.ListTools
import Parsing.PortExtractor
import Rendering.InfoTypes
import Rendering.RenderPorts
import Rendering.GlueStatements


dropLastIfNotEnd :: [String] -> [String]
dropLastIfNotEnd los
    | length los < 2 = []
    | otherwise = if ((last los) == "end")
                    then los
                    else dropLast los


dropLastIfEnd :: [String] -> [String]
dropLastIfEnd los
    | length los < 1 = []
    | otherwise = if ((last los) == "end")
                    then dropLast los
                    else los


stopAtEnd :: [String] -> [String]
stopAtEnd [] = []
stopAtEnd x
    | head x == "end" = []
    | otherwise = [(head x)] ++ stopAtEnd (tail x)


stopAtPort :: [String] -> [String]
stopAtPort x = untilKeyword x ["port"] []


afterPort :: [String] -> [String]
afterPort x = afterKeyword x ["port", "("]

 
addEndIfNotLast :: [String] -> [String]
addEndIfNotLast los
    | last los == "end" = los
    | otherwise = los ++ ["end"]


generateComponentDec :: [String] -> [String]
generateComponentDec los
    | length (isolateEntityDec los) < 2 = []
    | otherwise = ["component"] ++ 
                    (addEndIfNotLast (tail (dropLastIfNotEnd (isolateEntityDec los)))) ++ 
                    ["component", ";"]


removeLastComma :: String -> String
removeLastComma s
    | last s == ',' = dropLast s
    | otherwise = s


removeLastCommaFromList :: [String] -> [String]
removeLastCommaFromList los
    | length los == 0 = []
    | length los == 1 = [dropLast (head los)]
    | otherwise = [head los] ++ (removeLastCommaFromList (tail los))


-- Generic Declaration to Map:
genDec2Map :: [String] -> [String]
genDec2Map los = ["generic map ("] ++ mList' ++ [")"]
    where 
        gList = extractGenerics (stopAtPort (dropLastIfEnd los))
        mList = mapPorts2SimilarNames gList
        mList' = removeLastCommaFromList mList


-- Port Declaration to Map:
portDec2Map :: [String] -> [String]
portDec2Map los = ["port map ("] ++ pList' ++ [");"]
    where 
        portList = extractPorts (afterPort (dropLastIfEnd los))
        pList = mapPorts2SimilarNames portList
        pList' = removeLastCommaFromList pList


generatePortMap :: String -> [String] -> [String]
generatePortMap instName los
    | length (isolateEntityDec los) < 2 = []
    | otherwise = [instName, ": ", modName] ++ genMap ++ portMap 
        where
            modName = getEntityName los
            entChunk = skipNTokens (stopAtEnd (afterKeyword los ["entity"])) 4
            genMap = genDec2Map entChunk
            portMap = portDec2Map entChunk
            


declareSignals :: [String] -> [String]
declareSignals [] = []
declareSignals los = declareBatch sigList where
        portList = extractPorts los
        sigList = map convertPort2Sig portList
        

generateTestbench :: [String] -> [String]
generateTestbench los = 
    ["library ieee;"] ++
    ["use ieee.std_logic_1164.all;"] ++
    ["use ieee.numeric_std.all;"] ++
    ["", ""] ++
    ["entity " ++ (getEntityName los) ++ "_tb is"] ++
    ["end " ++ (getEntityName los) ++ "_tb;"] ++
    ["", ""] ++
    ["architecture behavioral_" ++ (getEntityName los) ++ " is"] ++
    ["",""] ++ 
    (glueStatements (generateComponentDec los)) ++ 
    ["",""] ++ 
    (declareSignals (tail (dropLast (extractDeclaration "port" los)))) ++
    ["begin"] ++
    ["", ""] ++
    (generatePortMap "UUT" los) ++ 
    ["", ""] ++
    ["end architecture behavioral_" ++ (getEntityName los) ++ ";"] ++ 
    ["", ""]
    


