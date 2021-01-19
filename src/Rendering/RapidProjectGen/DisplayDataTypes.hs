------------------------------------------------------------------------------------------------------------------------
--                                            Convert Data Type To String 
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.DisplayDataTypes where 
import Rendering.InfoTypes


dt2Str :: DataType -> String
dt2Str StdLogic = "SL"
dt2Str StdLogicVector = "SL Vec"
dt2Str StdULogic = "SUL"
dt2Str StdULogicVector = "SUL Vec"
dt2Str Signed = "Signed"
dt2Str Unsigned = "Unsigned"
dt2Str Bit = "Bit"
dt2Str UnconstrainedInt = "Int"
dt2Str (ConstrainedInt a b) = "Int " ++ (show a) ++ " " ++ (show b)
dt2Str _ = ""


