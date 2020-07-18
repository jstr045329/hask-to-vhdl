module Rendering.RegGateOutputPack where
import Rendering.InfoTypes

----------------------------------------------------------------------------------------------------
--                          Define a Type to Convey Registered Gate Output
----------------------------------------------------------------------------------------------------
data RegGateOutputPack = 
    TerminateRegGate |
    RegGateOutputPack {
        vhdLines :: [String]
    ,   allSignals :: [Information]
    ,   outputSignals :: [Information]
    ,   myLayerNum :: Int
    ,   nextLayer :: RegGateOutputPack
    } deriving (Eq, Show)

