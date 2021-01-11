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


-- HIGH PRIORITY:

-- Show interspersed VHDL

-- TODO: When user types </vhd>, this module scans VHD for signals & ports not declared. 
-- When it finds one, TUI gives user option to 1) Press enter and accept a suggested declaration, or 
-- 2) type their own declaration (using normal abbreviated commands, of course. sig, in, out, etc.)
--
-- When an undeclared thing is found, Hs should check for similar names that have been declared, 
-- and if it finds one, ask if it should duplicate the declaration for the new name. 

-- TODO: When user types </vhd>, Hs should scan for infinite signals. 

-- TODO: When user types </vhd>, if an output has a similar name to an infinite signal, 
-- Hs asks user if they want that output driven by some tap in the infinite signal. 
-- Default = last tap # used. 
-- User can of course enter any number they want. 


-- TODO: Go through all modules and explicity export things. 
-- Avoiding name collisions is starting to be a pain. 


-- TODO: Go through all the TODO's below, and prioritize everything that's useful for 
-- skeleton generation & rapid prototyping. Move everything else to a ticket. 

-- TODO: Make all GUI boxes scrollable

-- TODO: Delete <vhd> and replace with <con>
-- TODO: Replace <proc> with <seq>
-- Process Imbiber should parse signals.
-- Anything to the left of a <= gets declared if it isn't already in the signal list.
-- If you declare something after you use it, TUI fills in the missing details. 
-- When you type </seq>, TUI prompts you to fill in any details that are still missing. 
-- Process Imbiber should automatically reset any signals you use. 


-- TODO: Capture Infinite Signals
--      * Infinite delay chains for signals
--      * Infinite widths for all Information's.
--      * Might make sense to change all [Information]'s to [[Informations]],
--        then just use the head of each list.
--        Or, maintain [Int] that tracks how many of each thing has been used, 
--        then take whateverInt from each [Information].

-- TODO: Make using literals easy:
--      * When a literal is narrower than a signal, zero pad it.
--          * Right justify by default
--          * Provide option to left justify
--      * When a literal is wider than a signal, 
--          * Raise an error (default), or
--          * Bite off as many bits as signal can accept. 
--          * Warning is optional if bite off enabled.


-- TODO: Make faculties for recursive entities. 

-- TODO: Make all signals in an entity available to child entities. 
-- Automatically wire up port map. 

-- TODO: Come up with a way to try executing Hs literal lines, and 
-- Hs literals only affect rendered VHDL when compilation is successful
-- See System.Eval.Haskell

-- TODO: Add commands to modify ProjectParameters.
-- You should be able to change reset style in all processes with a single command. 

-- TODO: Add commands to alter any parameter in Entity. 

-- TODO: Make a set of all inputs and signals in parent entity, and make those available to child. 
-- Haskell should automatically:
--      1) Figure out which ones are actually used, and put those in the entity declaration, and 
--      2) Map Information's with similar names in the port map,
-- unless the user commands something else. 
--
-- RELATED TODO: Figure out the best way to command Something Else. 

-- TODO: Add an <inst> command, which creates a new instance of a given child.
--       Figure out rules for whether & how that's going to modify existing port maps. 
--       For instance, if instance 0 is already prewired, you might need to modify that 
--       when you create instance 1.  

-- TODO: Add a nomen prefix field to Entity, which is:
--      intercalate "_" [pathToPresent gS]
--
-- This will give all entities a name format like this:
--
--      Top_FirstLevelModule_SecondLevelModule_ThirdLevel_...
-- 
-- Perhaps only use this feature when an entity is pulling in Information's from above.


-- This allows user to avoid naming conflicts. Plus, when you look at the VHDL, you can 
-- see the full path to every entity in the project.


-- Lower Priority:

-- TODO: Think about whether I need to develop a system of name transformation conventions. 
-- For instance, maybe all signals should be an infinite list from the get go. 
-- Append the six digit number, but that only appears in rendered code. User does not need to 
-- look at appended numbers. 

-- TODO: Add userMessages field to generatorState, so that user can be informed when a command is rejected.

-- TODO: Figure out how to display infinite entities. 
-- Could be as simple as, "When list is longer than 10, write ..."

-- TODO: Add commands to choose a different clock, different reset style for individual
-- processes. 

-- TODO: Think about how I might support infinite recursion in entities. 
-- I should be able to do something like this:
--      take 4872 myEntityList
-- and the first 4871 instances contain an instance of the same thing (but with a different name)

-- TODO: Add a window that highlights changes to port maps. 
--       Present entity is instantiated in parent by a port map, 
--       and this window shows how signals from parent are being routed to child. 
--


-- TODO: No need to do this immediately, but once scripting is supported, recursive Generator State
-- would allow each script to have its own state. 





