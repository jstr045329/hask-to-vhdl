module Rendering.RapidProjectGen.DecodeOneString where
import Rendering.RapidProjectGen.GeneratorState
import Rendering.InfoTypes
import Rendering.Entity
import Rendering.EntityTree
import Tools.WhiteSpaceTools
import Tools.ListTools
import Parsing.TokenMatchingTools
import Tools.StringTools
import Parsing.GuaranteeWhitespace
import Rendering.InterspersedCode
import Rendering.RapidProjectGen.AppendOneLine
import Rendering.RapidProjectGen.CommandDecoder

------------------------------------------------------------------------------------------------------------------------
--                                                  Command Parser 
-- 
-- This function parses some input string for commands pertaining to:
--      * Start a new entity,
--      * Conclude that entity,
--      * Move focus up to a parent entity,
--      * Drill down to a child entity,
--      * Select VHDL or Hs view.
--
------------------------------------------------------------------------------------------------------------------------

-- TODO: Delete this function
decodeOneStr :: String -> GeneratorState -> GeneratorState
decodeOneStr oneStr gS 
    | (startsWith oneStr "<ent>") =
        gS {
            formingEntity = True
        ,   entTree =
                appendOneEntity
                    (gPEnt gS) -- name of present entity (which will be the parent of the new entity)
                    (entTree gS) -- entity tree in Generator State
                    (defaultEntity { entNomen = (words oneStr) !! 1 }) -- The new entity we are creating

        ,   pathToPresent = (pathToPresent gS) ++ [(words oneStr) !! 1]
        }

    | (startsWith oneStr "</ent>") = gS {
        pathToPresent = take ((length (pathToPresent gS)) - 1) (pathToPresent gS)
        }

    | (startsWith oneStr "show vhd") = gS {showVhd = True, showHs = False}

    | (startsWith oneStr "show hs") = gS {showVhd = False, showHs = True}

    | otherwise = slurpCommand oneStr gS


