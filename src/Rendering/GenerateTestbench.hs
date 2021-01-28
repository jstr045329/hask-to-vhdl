module Rendering.GenerateTestbench where
import Parsing.TokenMatchingTools
import Parsing.ExtractEntity
import Tools.ListTools
import Parsing.PortExtractor
import Rendering.InfoTypes
import Rendering.RenderPorts
import Rendering.GlueStatements
import Tools.WhiteSpaceTools
import Rendering.CommentTools


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


stopAtGeneric :: [String] -> [String]
stopAtGeneric x = untilKeyword x ["generic"] []


afterGeneric :: [String] -> [String]
afterGeneric x = afterKeyword x ["generic"]


stopAtPort :: [String] -> [String]
stopAtPort x = untilKeyword x ["port"] []


afterPort :: [String] -> [String]
afterPort x = afterKeyword x ["port", "("]

 
addEndIfNotLast :: [String] -> [String]
addEndIfNotLast los
    | last los == "end" = los
    | otherwise = los ++ ["end"]


insertSemicolonBeforeEnd :: [String] -> [String]
insertSemicolonBeforeEnd los
    | last los == "end" = (dropLast los) ++ [";"] ++ ["end"]
    | otherwise = los


generateComponentDec :: [String] -> [String]
generateComponentDec los
    | length (isolateEntityDec los) < 2 = []
    | otherwise = ["component"] ++ 
                    (insertSemicolonBeforeEnd 
                        (addEndIfNotLast 
                        (tail 
                        (dropLastIfNotEnd 
                        (isolateEntityDec los))))) ++ 
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
        mList' = zipTab (removeLastCommaFromList mList)


-- Port Declaration to Map:
portDec2Map :: [String] -> [String]
portDec2Map los = ["port map ("] ++ pList' ++ [");"]
    where 
        portList = extractPorts (afterPort (dropLastIfEnd los))
        pList = mapPorts2SimilarNames portList
        pList' = zipTab (removeLastCommaFromList pList)


generatePortMap :: String -> [String] -> [String]
generatePortMap instName los
    | length (isolateEntityDec los) < 2 = []
    | otherwise = [instName, ": ", modName] ++ genMap ++ portMap 
        where
            modName = getEntityName los
            entChunk = stopAtEnd (afterKeyword los ["entity"])
            genMap = (if (elem "generic" los)
                        then (genDec2Map entChunk)
                        else [])
            portMap = portDec2Map entChunk


isClock :: String -> Bool
isClock x
    | length x < 3 = False
    | (take 3 x) == "clk" = True
    | (take 5 x) == "clock" = True
    | otherwise = False


isReset :: String -> Bool
isReset x
    | length x < 3 = False
    | (take 3 x) == "rst" = True
    | (take 5 x) == "reset" = True
    | otherwise = False


isNegative :: String -> Bool
isNegative s
    | lastN s 2 == "_n" = True
    | otherwise = False


setResetDefault :: Information -> Information
setResetDefault x =
    if (isNegative (nomen x))
        then VhdSig {
                nomen = nomen x
            ,   dataType = dataType x
            ,   width = width x
            ,   sDefault = Specified "'0'"
            ,   sReset = sReset x
            ,   clocked = clocked x
            ,   comments = comments x
            ,   assertionLevel = Just "'0'"
            }

        else VhdSig {
                nomen = nomen x
            ,   dataType = dataType x
            ,   width = width x
            ,   sDefault = Specified "'1'"
            ,   sReset = sReset x
            ,   clocked = clocked x
            ,   comments = comments x
            ,   assertionLevel = Just "'1'"
            }


extractResets :: [Information] -> [Information]
extractResets x = [(setResetDefault i) | i <- x, isReset (nomen i)]


removeResets :: [Information] -> [Information]
removeResets x = [i | i <- x, not (isReset (nomen i))]


setClockDefault :: Information -> Information
setClockDefault x = VhdSig {
        nomen = nomen x
    ,   dataType = dataType x
    ,   width = width x
    ,   sDefault = Specified "'1'"
    ,   sReset = sReset x
    ,   clocked = clocked x
    ,   comments = comments x
    ,   assertionLevel = Just "'1'"
    }


extractClocks :: [Information] -> [Information]
extractClocks x = [(setClockDefault i) | i <- x, isClock (nomen i)]


removeClocks :: [Information] -> [Information]
removeClocks x = [i | i <- x, not (isClock (nomen i))]


getPorts :: [String] -> [Information]
getPorts los = [x | x <- portList] where
    portList = extractPorts los


declareConstants :: [String] -> [String]
declareConstants [] = []
declareConstants los = declareBatch constList where
    genList = extractGenerics (tail' (dropLast (extractDeclaration "generic" los)))
    constList = map convertGen2Const genList


declareSignals :: [String] -> [String]
declareSignals [] = []
declareSignals los = declareBatch sigList where
    portList = extractPorts los
    rawSignals = map port2Sig portList
    clockList = extractClocks rawSignals
    resetList = extractResets rawSignals
    otherSigs = removeClocks (removeResets rawSignals)
    sigList = clockList ++ resetList ++ otherSigs


resetStimSignals :: [String] -> [String]
resetStimSignals [] = []
resetStimSignals los = resetBatch stimSigs where
    portList = removeClocks (removeResets (extractPorts los))
    inputList = [x | x <- portList, Rendering.InfoTypes.isInput x]
    stimSigs = map port2Sig inputList


-- TODO: Change the way GenerateTestbench converts port names to signal names so that 
-- UUT ports do not conform to s_..._000000 convention. Possibly write a separate function
-- for testbench use.


myBadEntity :: [String]
myBadEntity = [
        ""
    ,   ""
    ,   "library ieee;"
    ,   "use ieee.std_logic_1164.all;"
    ,   ""
    ,   "entity MyBadEntity is"
    ,   "generic ("
    ,   "    I_AM_A_GENERIC : integer := 42;"
    ,   "    ANOTHER_GENERIC : integer := 42"
    ,   ");"
    ,   "port ("
    ,   "   clk : in std_logic;"
    ,   "   rst : in std_logic;"
    ,   "   joe : out std_logic"
    ,   ");"
    ,   "end MyBadEntity;"
    ,   ""
    ,   "architecture behavioral of MyBadEntity is"
    ,   "   signal x1 : std_logic;"
    ,   "   signal x2 : std_logic;"
    ,   "begin"
    ,   "x1 <= i + j;"
    ,   ""
    ,   "process(clk)"
    ,   "begin"
    ,   "if rising_edge(clk) then"
    ,   "    x2 <= '0';"
    ,   "else"
    ,   "    x2 <= '1';"
    ,   "end if;"
    ,   "end process;"
    ,   ""
    ,   "joe <= x1 and x2;"
    ,   ""
    ,   "end behavioral;"
    ,   ""
    ,   ""
    ]

generateTestbench :: [String] -> [String]
generateTestbench [] = []
generateTestbench los 
    | ((getEntityName los) == []) = []
    | otherwise = 
        (commentBlock ["Testbench for " ++ (getEntityName los)]) ++ 
        ["library ieee;"] ++
        ["use ieee.std_logic_1164.all;"] ++
        ["use ieee.numeric_std.all;"] ++
        ["use work.VhdSimToolsPkg.all;"] ++ 
        ["", ""] ++
        ["entity " ++ (getEntityName los) ++ "_tb is"] ++
        ["end " ++ (getEntityName los) ++ "_tb;"] ++
        ["", ""] ++
        ["architecture behavioral_" ++ (getEntityName los) ++ "_tb of " ++ (getEntityName los) ++ "_tb is"] ++
        ["",""] ++ 
        (glueStatements (generateComponentDec los)) ++ 
        ["",""] ++ 
        (declareConstants los) ++ 
        (declareSignals (tail' (dropLast (extractDeclaration "port" los)))) ++
        [""] ++
        ["constant clk_per : time := 10 ns;"] ++
        ["signal sim_done : std_logic := '0';"] ++
        ["signal test_stage : integer := 0;"] ++
        ["signal s_clock_cycle_count : integer := 0;"] ++ 
        [""] ++
        [""] ++
        ["begin"] ++
        [""] ++
        [""] ++
        (commentBlock ["Boiler Plate"]) ++ 
        ["CLOCK_PROCESS: process"] ++
        ["begin"] ++
        ["    if sim_done = '1' then"] ++
        ["        wait;"] ++
        ["    else"] ++ 
        ["        wait for clk_per/2;"] ++
        ["        clk <= not clk;"] ++
        ["    end if;"] ++
        ["end process;"] ++
        [""] ++
        [""] ++
        (commentBlock ["Count Clock Cycles"]) ++ 
        ["CLOCK_CYCLE_COUNTER: process(clk)"] ++
        ["begin"] ++
        ["    if rising_edge(clk) then "] ++
        ["        s_clock_cycle_count <= s_clock_cycle_count + 1;"] ++
        ["    end if;"] ++
        ["end process;"] ++
        [""] ++
        [""] ++
        (commentBlock ["Stim Process"]) ++ 
        ["STIM_PROCESS: process"] ++
        ["begin"] ++
        (zipTab (resetStimSignals los)) ++ 
        ["    test_stage <= 0;"] ++ 
        ["    sync_wait_rising(clk, 10);"] ++
        -- Invert all resets
        -- Typically use exactly one in practice. 
        (map (\oneRst -> "    " ++ (nomen oneRst) ++ " <= not " ++ (nomen oneRst) ++ ";")
             (extractResets (Rendering.GenerateTestbench.getPorts los))) ++ 
        ["    sync_wait_rising(clk, 10);"] ++
        [""] ++
        [""] ++
        ["    -- Test ends here:"] ++ 
        ["    test_stage <= test_stage + 1;"] ++ 
        ["    sync_wait_rising(clk, 100);"] ++
        ["    sim_done <= '1';"] ++
        ["    wait;"] ++
        ["end process;"] ++
        [""] ++
        [""] ++
        [""] ++
        [""] ++
        (commentBlock ["Unit Under Test"]) ++ 
        (generatePortMap "UUT" los) ++ 
        [""] ++
        [""] ++
        ["end architecture behavioral_" ++ (getEntityName los) ++ "_tb;"] ++ 
        [""] ++
        [""] 


