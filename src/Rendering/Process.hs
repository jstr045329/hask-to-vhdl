------------------------------------------------------------------------------------------------------------------------
--                                                      Process 
--
-- This module represents and renders process statements. 
--
-- Note that at this time, inferred latches are intentionally not supported. 
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
        } deriving (Eq, Show)


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
resetEverything oneProc = (resetBatch (filterUnique ((sensitivityList oneProc) ++ (procInputs oneProc) ++ (internalState oneProc))))

renderClockAndResetIfStatements :: Process -> ProjectParameters -> [String]
renderClockAndResetIfStatements oneProc projParams
    | ((isClocked oneProc) && ((clkStyle projParams) == RisingEdge) && ((rstStyle projParams) == SyncPositive)) = 
        ["if rising_edge(" ++ (nomen (pClk oneProc)) ++ ") then"] ++
        [(tab 1) ++ "if " ++ (nomen (pRst oneProc)) ++ " = '1' then"] ++
        (nZipTab 2 (resetEverything oneProc)) ++ 
        [(tab 1) ++ "else"] ++
        []
        
    
        
    | otherwise = []
        -- (zZipTab 2 (
        

------------------------------------------------------------------------------------------------------------------------
--                                                  Render Process 
------------------------------------------------------------------------------------------------------------------------
renderProcess :: Process -> ProjectParameters -> [String]
renderProcess oneProc projParams =
    [renderProcessFirstLine oneProc projParams] ++ 
    ["begin"] ++
    []
    
    
    
