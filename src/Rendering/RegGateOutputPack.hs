module Rendering.RegGateOutputPack where
import Rendering.InfoTypes

----------------------------------------------------------------------------------------------------
--                          Define a Type to Convey Registered Gate Output
----------------------------------------------------------------------------------------------------
data RegGateOutputPack = RegGateOutputPack {
        vhdLines :: [String]
    ,   allSignals :: [Information]
    ,   outputSignals :: [Information]
    ,   myLayerNum :: Int
    ,   nextLayer :: Maybe RegGateOutputPack
    } deriving (Eq, Show)

