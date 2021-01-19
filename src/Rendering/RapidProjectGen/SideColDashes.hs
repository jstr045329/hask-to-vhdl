------------------------------------------------------------------------------------------------------------------------
--                                          Define Dashes For A Side Column 
--
-- Define a string intended to separate 1 box off to either side from another. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.SideColDashes where
import Rendering.RapidProjectGen.ScreenParameters
import Tools.StringTools


sideColDashes :: String
sideColDashes = ctrString (take (sideColumn - 2) dashes) sideColumn where 
    dashes = repeat '-'


