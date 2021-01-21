------------------------------------------------------------------------------------------------------------------------
--                                               Present Entity Tools 
--
-- This module contains tools for displaying the present entity on the TUI. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.PresentEntity where
import Rendering.RapidProjectGen.GeneratorState
import Rendering.RapidProjectGen.RapidTuiState
import Rendering.Entity
import Rendering.EntityTree


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


------------------------------------------------------------------------------------------------------------------------
--                                      Get Present Entity from Generator State 
------------------------------------------------------------------------------------------------------------------------
getPresentEntity :: GeneratorState -> Entity
getPresentEntity gS = head (fetchOneEntity (gPEnt gS) (entTree gS))


