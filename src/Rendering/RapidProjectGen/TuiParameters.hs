------------------------------------------------------------------------------------------------------------------------
--                                               Define TUI Parameters 
--
-- This module defines parameters that dictate size of TUI windows. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.TuiParameters where


------------------------------------------------------------------------------------------------------------------------
--                                               Height of Generic Box 
------------------------------------------------------------------------------------------------------------------------
genericsToShow :: Int
genericsToShow = 5


------------------------------------------------------------------------------------------------------------------------
--                                                Height of Port Box 
------------------------------------------------------------------------------------------------------------------------
portsToShow :: Int
portsToShow = 20


------------------------------------------------------------------------------------------------------------------------
--                                               Height of Signal Box 
------------------------------------------------------------------------------------------------------------------------
signalsToShow :: Int
signalsToShow = 20


------------------------------------------------------------------------------------------------------------------------
--                                             Height of Entity Body Box 
------------------------------------------------------------------------------------------------------------------------
renderedLinesToShow :: Int
renderedLinesToShow = 27


------------------------------------------------------------------------------------------------------------------------
--                                           Height of Command History Box 
------------------------------------------------------------------------------------------------------------------------
commandHistoryTraceback :: Int
commandHistoryTraceback = 10


------------------------------------------------------------------------------------------------------------------------
--                                               Width of Signal Names 
------------------------------------------------------------------------------------------------------------------------
nomenWidth :: Int
nomenWidth = 20


------------------------------------------------------------------------------------------------------------------------
--                                                Width of Data Type 
------------------------------------------------------------------------------------------------------------------------
dtWidth :: Int
dtWidth = 10


------------------------------------------------------------------------------------------------------------------------
--                                               Width of Signal Width 
------------------------------------------------------------------------------------------------------------------------
widthWidth :: Int
widthWidth = 10


------------------------------------------------------------------------------------------------------------------------
--                                              Width of Default Value 
------------------------------------------------------------------------------------------------------------------------
defaultStrWidth :: Int
defaultStrWidth = 10


------------------------------------------------------------------------------------------------------------------------
--                                        Glean Information's From TUI State 
------------------------------------------------------------------------------------------------------------------------

-- TODO: Figure out what makes this different from the similarly named thing above; and, whether both are necessary. 
numGenericsToDisplay :: Int
numGenericsToDisplay = 12


-- TODO: Figure out what makes this different from the similarly named thing above; and, whether both are necessary. 
numPortsToDisplay :: Int
numPortsToDisplay = 18


-- TODO: Figure out what makes this different from the similarly named thing above; and, whether both are necessary. 
numSignalsToDisplay :: Int
numSignalsToDisplay = 18


------------------------------------------------------------------------------------------------------------------------
--                                             Width of Entity Body Box 
------------------------------------------------------------------------------------------------------------------------
renderedCodeWidth :: Int 
renderedCodeWidth = 75


