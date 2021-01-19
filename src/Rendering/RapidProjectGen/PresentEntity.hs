------------------------------------------------------------------------------------------------------------------------
--                                               Present Entity Tools 
--
-- This module contains tools for displaying the present entity on the TUI. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.PresentEntity where
import Rendering.RapidProjectGen.GeneratorState
import Rendering.RapidProjectGen.RapidTuiState


-----------------------------------------------------------------------------------------------------------------------
--                                         Get Present Entity From TUI State 
------------------------------------------------------------------------------------------------------------------------
pEnt :: TuiState -> String
pEnt ts = last (pathToPresent (generatorState ts))


------------------------------------------------------------------------------------------------------------------------
--                                              Display Present Entity 
------------------------------------------------------------------------------------------------------------------------
displayPresentEnt :: TuiState -> [String]
displayPresentEnt ts = ["Present Entity: " ++ (pEnt ts)]


