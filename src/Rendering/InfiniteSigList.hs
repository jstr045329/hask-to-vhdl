------------------------------------------------------------------------------------------------------------------------
--                                         Create Infinite Lists of Signals 
--
-- Information Overload!!! This module lets you create infinite sequences of ports, signals, whatever, to your heart's 
-- content. This is useful because often it is more convenient to presuppose the existence of many variants of the 
-- same thing (such as some_sig_xxxxxx) without knowing a priori how many you need. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.InfiniteSigList where 
import Rendering.InfoTypes
import Text.Printf
import Rendering.InfoNameTools


infiniteSignals :: String -> DataType -> Width -> DefaultValue -> String -> Maybe Bool -> [String] -> Maybe String -> [Information]
infiniteSignals nomen0 dt0 w0 default0 sReset0 clocked0 comments0 assertionLevel0 =
    [VhdSig {
            nomen = (nomen0 ++ "_" ++ (show n))
        ,   dataType = dt0
        ,   width = w0
        ,   sDefault = default0
        ,   sReset = sReset0
        ,   clocked = clocked0
        ,   comments = comments0
        ,   assertionLevel = assertionLevel0
        } | n <- [0..]]
        

infiniteSlvBus :: String -> DataType -> Width -> DefaultValue -> String -> Maybe Bool -> [String] -> Maybe String -> [Information]
infiniteSlvBus nomen0 dt0 w0 default0 sReset0 clocked0 comments0 assertionLevel0 =
    [VhdSig {
            nomen = (nomen0 ++ "(" ++ (show n) ++ ")")
        ,   dataType = dt0
        ,   width = w0
        ,   sDefault = default0
        ,   sReset = sReset0
        ,   clocked = clocked0
        ,   comments = comments0
        ,   assertionLevel = assertionLevel0
        } | n <- [0..]]

-- TODO: Check if some functions in this file are reinventing other ones.

------------------------------------------------------------------------------------------------------------------------
--                                          Create Infinite List of Signals
--
-- This function takes an input port and creates an infinite list of signals with the same name stub. See 
-- assignSignalChainWithInput in Assignment.hs to assign the signals. This list is more useful for declaring the 
-- signals.
------------------------------------------------------------------------------------------------------------------------
endlessSignals :: Information -> [Information]
endlessSignals someInput = 
    [VhdSig {
                nomen = (nomen someInput) ++ (signalSuffix n)
            ,   dataType = dataType someInput
            ,   width = width someInput
            ,   sDefault = sDefault someInput
            ,   sReset = sReset someInput
            ,   clocked = clocked someInput
            ,   comments = (if (direction someInput == In)
                                then ["Driven by " ++ (nomen someInput)]
                                else ["Drives " ++ (nomen someInput)]) ++ (comments someInput)
            ,   assertionLevel = assertionLevel someInput}            
        
        | n <- [0..]]