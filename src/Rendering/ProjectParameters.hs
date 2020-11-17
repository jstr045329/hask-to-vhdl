------------------------------------------------------------------------------------------------------------------------
--                                                Project Parameters 
--
-- Defines a type you can use for rendering parameters, and provides an example function that makes it easy to render 
-- large projects with the same parameters. You can change clocking and reset styles across entire projects just 
-- by changing 1 function. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.ProjectParameters where 


------------------------------------------------------------------------------------------------------------------------
--                                      A Datatype For Representing Clock Style 
------------------------------------------------------------------------------------------------------------------------
data ClockStyle = RisingEdge | FallingEdge deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                      A Datatype For Representing Reset Style 
------------------------------------------------------------------------------------------------------------------------
data ResetStyle = SyncPositive | SyncNegative | AsyncPositive | AsyncNegative deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                  A Datatype For Representing Project Parameters 
------------------------------------------------------------------------------------------------------------------------
data ProjectParameters = ProjectParameters {
        clkStyle :: ClockStyle
    ,   rstStyle :: ResetStyle
    ,   formatString :: String -- For integers
    ,   givePortsZeroLikeDefault :: Bool    
    } deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                              An Example Function That Makes Project Parameters Easy 
------------------------------------------------------------------------------------------------------------------------
easyProjParams = ProjectParameters {
        clkStyle = RisingEdge
    ,   rstStyle = SyncPositive
    ,   formatString = "%06d"
    ,   givePortsZeroLikeDefault = False
    }
