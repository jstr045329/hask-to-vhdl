module TestHsProjects.PwmConverter where
import Rendering.InfoTypes
import Rendering.Condition
import Rendering.Statement
import Rendering.CommonLiterals
import Rendering.Assignment


counter = hardWidthSigned "s_counter" 10
thresh = hardWidthSigned "s_thresh" 10
sFetDriver = easyClockedSL "fetGate"


-- Decide when FET should be turned on:
turnFetOn = LesserT counter thresh

pwmBusinessLogic = 
    IfStatement [
            (turnFetOn, [Assignment sFetDriver stdLogicHi], [])
        ,   (TerminalElse, [Assignment sFetDriver stdLogicLo], [])]


-- The scope of this project is to:
--      1.) Generate repetitive circuit structures, and
--      2.) Automate repetitive tasks.

-- Files having to do with language modeling can be split into a 3rd 

-- TODO: 
--      1.) Move files that model the language into a LanguageModeling directory, 
--      2.) Move files that clearly fit one of the 2 objectives into the appropriate directory, 
--      3.) Delete everything else. 

-- Note that making everything represent ARBITRARY VHDL is not necessarily persuant of objectives.
-- For instance, perhaps it's appropriate to assume that clock is always named clk, etc. 
