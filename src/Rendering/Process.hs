------------------------------------------------------------------------------------------------------------------------
--                                                      Process 
--
-- This module represents and renders process statements. 
--
-- NOTE: At this time, inferred latches are intentionally not supported. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.Process where
import Rendering.Assignment
import Rendering.Condition
import Rendering.InfoTypes
import Rendering.FilterUnique
import Rendering.Statement
import Rendering.ProjectParameters
import Tools.WhiteSpaceTools
import Data.List


------------------------------------------------------------------------------------------------------------------------
--                                       Define A Struct To Represent Process 
------------------------------------------------------------------------------------------------------------------------
data Process = Process {
          procNomen :: String
        , pClk :: Information
        , pRst :: Information
        , sensitivityList :: [Information]
        , procInputs :: [Information]
        , variables :: [Information]
        
        -- The internalState list should ONLY contain informations that are ALREADY both in the procInputs and procOutputSignals lists.
        -- Note that variables have memory, so internalState is normally only for signals. 
        -- If VHDL is pre-2008, then anything in internalState must be a signal, since outputs cannot be read.
        , internalState :: [Information] 
        , procOutputSignals :: [Information]
        , isClocked :: Bool
        , sequentialCode :: [SequentialStatement]
        , procPlainLines :: [String]
        } deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                                  Default Process 
--
-- Make commonly-used assumptions to form a good starting point for our process. 
--
------------------------------------------------------------------------------------------------------------------------
defaultProcess :: Process
defaultProcess = Process {
        procNomen = ""
    ,   pClk = easyClk
    ,   pRst = easyRst
    ,   sensitivityList = [easyClk]
    ,   procInputs = []
    ,   variables = []
    ,   internalState = []
    ,   procOutputSignals = []
    ,   isClocked = True
    ,   sequentialCode = []
    ,   procPlainLines = []
    }


defaultNamedProcess :: String -> Process
defaultNamedProcess thisNomen = defaultProcess {procNomen = thisNomen}


------------------------------------------------------------------------------------------------------------------------
--                                           Render Sequential Assignments 
------------------------------------------------------------------------------------------------------------------------
-- renderProcBusinessLogic :: Process -> [String]
-- renderProcBusinessLogic oneProc = 
    -- map 

------------------------------------------------------------------------------------------------------------------------
--                                              Render Sensitivity List 
------------------------------------------------------------------------------------------------------------------------

-- Return a List of Strings containing the reset name if reset is asyncronous. 
-- Return a blank list otherwise. 
syncRst2Str :: Process -> ProjectParameters -> [String]
syncRst2Str oneProc projParams =
    if (elem (rstStyle projParams) [SyncPositive, SyncNegative])
        then []
        else [(nomen (pRst oneProc))]


-- Generate a List of Strings representing senstivity list:
sensitivityLOS :: Process -> ProjectParameters -> [String]
sensitivityLOS oneProc projParams =
    if (isClocked oneProc)
        then ([(nomen (pClk oneProc))] ++ (syncRst2Str oneProc projParams))
        else 
            map
                nomen 
                (filterUnique ([pRst oneProc] ++ (sensitivityList oneProc) ++ (procInputs oneProc)))


renderSensitivity :: Process -> ProjectParameters -> String
renderSensitivity oneProc projParams = "(" ++ (intercalate ", " (sensitivityLOS oneProc projParams)) ++ ")"


------------------------------------------------------------------------------------------------------------------------
--                                               Render Process Header 
------------------------------------------------------------------------------------------------------------------------
renderProcessHeader :: Process -> String 
renderProcessHeader oneProc = 
    if (length (procNomen oneProc) > 0)
        then ((procNomen oneProc) ++ ": process")
        else "process"


------------------------------------------------------------------------------------------------------------------------
--                                           Render First Line Of Process 
------------------------------------------------------------------------------------------------------------------------
renderProcessFirstLine :: Process -> ProjectParameters -> String 
renderProcessFirstLine oneProc projParams = (renderProcessHeader oneProc) ++ (renderSensitivity oneProc projParams)


------------------------------------------------------------------------------------------------------------------------
--                                       Render Clock and Reset If Statements 
------------------------------------------------------------------------------------------------------------------------
resetEverything :: Process -> [String]
resetEverything oneProc = (resetBatch (filterUnique (procOutputSignals oneProc)))


renderClockAndResetIfStatements :: Process -> ProjectParameters -> [String]
renderClockAndResetIfStatements oneProc projParams
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == SyncPositive)) = 
        ["if rising_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '1' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == SyncPositive)) = 
        ["if falling_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '1' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == SyncNegative)) = 
        ["if rising_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '0' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == SyncNegative)) = 
        ["if falling_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '0' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == AsyncPositive)) = 
        ["if rising_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '1' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == AsyncPositive)) = 
        ["if falling_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '1' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == AsyncNegative)) = 
        ["if rising_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '0' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == AsyncNegative)) = 
        ["if falling_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '0' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    | otherwise = ["    -- Process not clocked and/or reset style not relevant."]


renderClockAndResetIfStatementFooter :: Process -> ProjectParameters -> [String]
renderClockAndResetIfStatementFooter oneProc projParams
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == SyncPositive)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == SyncPositive)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == SyncNegative)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == SyncNegative)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == AsyncPositive)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == AsyncPositive)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == AsyncNegative)) = [(tab 1) ++ "end if;", "end if;"]
    | ((isClocked oneProc) && ((clkStyle projParams) == FallingEdge) && ((rstStyle projParams) == AsyncNegative)) = [(tab 1) ++ "end if;", "end if;"]
    | otherwise = []


------------------------------------------------------------------------------------------------------------------------
--                                                  Render Process 
------------------------------------------------------------------------------------------------------------------------
renderProcess :: Process -> ProjectParameters -> [String]
renderProcess oneProc projParams =
    [renderProcessFirstLine oneProc projParams] ++ 
    ["begin"] ++
    (renderClockAndResetIfStatements oneProc projParams) ++ 
    -- TODO: Render the SequentialCode field in Process.
    (procPlainLines oneProc) ++ 
    (renderClockAndResetIfStatementFooter oneProc projParams) ++
    ["end process;"] ++ 
    ["", ""]


