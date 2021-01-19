------------------------------------------------------------------------------------------------------------------------
--                                             Display InfoTypes on TUI 
--
-- This module contains tools for displaying Information's on TUI. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.DisplayInformation where
import Rendering.InfoTypes 
import Data.List
import Tools.StringTools
import Rendering.RapidProjectGen.DisplayDataTypes
import Rendering.RapidProjectGen.TuiParameters


showOneInfo :: Information -> String
showOneInfo (Port n dt w dir _ _ _ _ _) = intercalate " " [ljs n nomenWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10, show dir]
showOneInfo (VhdSig n dt w _ _ _ _ _) = intercalate " " [ljs n nomenWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10] 
showOneInfo (Generic n dt w (Specified dV) _) = intercalate " " [ljs n nomenWidth, ljs dV defaultStrWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10] 
showOneInfo (Generic n dt w Unspecified _) = intercalate " " [ljs n nomenWidth, ljs "" defaultStrWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10] 
showOneInfo _ = ""









