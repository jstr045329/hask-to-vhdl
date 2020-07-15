module Rendering.RegisterPorts where
import Rendering.InfoTypes
import Tools.WhiteSpaceTools

-- TODO: Write functions that wrap results from this function in an entity, and 
-- handle component declaration. 
-- TODO: Test results
registerPorts :: Information -> Information -> [Information] -> [String]
registerPorts clkIn rstIn portList = 
    ["process(" ++ (nomen clkIn) ++ ")"] ++ 
    ["begin"] ++ 
    [(tab 1) ++ "if rising_edge(" ++ (nomen clkIn) ++ ") then"] ++ 
    [(tab 2) ++ "if " ++ (nomen rstIn) ++ " = " ++ (liftAssertionLevel (assertionLevel rstIn)) ++ " then"] ++ 

    -- Reset the signals:
    (map (\oneSig -> (tab 3) ++ (nomen oneSig) ++ " <= " ++ (sReset oneSig) ++ ";")
         (map (\(_, oneSig) -> oneSig) (portToSig portList))) ++ 

    -- Reset the output ports:
    (map (\(onePort, oneSig) -> (tab 3) ++ (nomen onePort) ++ " <= " ++ (sReset oneSig) ++ ";")
         (portToSig [p | p <- portList, direction p == Out])) ++

    [(tab 2) ++ "else"] ++ 

    -- Register the inputs:
    (map (\(onePort, oneSig) -> (tab 3) ++ (nomen oneSig) ++ " <= " ++ (nomen onePort) ++ ";")
         (portToSig [p | p <- portList, direction p == In])) ++

    -- Register the outputs:
    (map (\(onePort, oneSig) -> (tab 3) ++ (nomen onePort) ++ " <= " ++ (nomen oneSig) ++ ";")
         (portToSig [p | p <- portList, direction p == Out])) ++ 

    [(tab 2) ++ "end if;"] ++ 
    [(tab 1) ++ "end if;"] ++ 
    ["end process;"]
    

