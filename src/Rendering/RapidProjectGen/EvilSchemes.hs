module Rendering.RapidProjectGen.EvilSchemes where







-- TODO: PICK UP HERE: Add scripting capability. Scripting will help make demo nice & automate testing.











-- TODO: Rehearse the exact demo. Make everything perfect.


-- This is the demo I want to give:
--      * Declare nested entities under Top, 
--      * 2 nested entities under 1 of the babies,
--      * In each entity, type:
--          * 1 input,
--          * 1 output,
--          * 1 signal,
--          * Something that is memorable
--      * Processes automatically reset every name that is assigned in that process
--      * Apply my coding conventions:
--          * Convert all inputs to i_ versions,
--          * For every signal-like name, generate s_ and o_ versions,
--          * Automatically drive all o_ versions,
--      * Move up and down the hierarchy a couple times. Bounce from one to another, adding a line here or there.
--      * Show audience rendered files:
--          * Every entity is named something like this: parentLevel_1_parentLevel_2_..._myChildName
--            which prevents naming conflicts;
--          * A package which declares all components, 
--          * A testbench for every component.
--
--
-- BPSK modulator would be a good example, because it's practical yet easy to write in a few minutes.
--      * Set default width to ADC_WIDTH when declaring ADC signals
--      * Set default width to ACCUMULATOR_WIDTH when declaring acculator.
--
-- The goal is to show how powerful it is when you can iterate through code, just like you would iterate 
-- through requirements. In each pass, you keep some kind of improvement from the last one, yet you reduce
-- one or more other modules to a skeleton; furthermore, editing that skeleton is quick and easy. 








































-- TODO: Add commands: delay parsing, start parsing, parse now
-- TODO: Replace String with Text




-- TODO: Signals should be added to entity as whatever the defaults were at the time. 


-- TODO:
--     * Generate a package holding all component declarations
--     * For each entity, generate a package w/ component declaration for that package and its children. 
--     * Render entire GeneratorState to Hs.
--     * Give this thing the ability to execute scripts.


-- TODO: Write a command called delayParsing, which simply enters lines into GeneratorState.
-- TODO: Write a command called parseNow, which runs the N^2 stuff. 
-- The idea is run the program in N^2 time, instead of turning it into N^3 by running N^2 after every command.




-- TODO: Add the "inst" command, which instantiates some already-declared entity.
-- After typing the command, TUI shows user every port one at a time. 
-- User types in a name that each pin should map to.
-- If user enters a blank string, that creates a signal with the same name as component I/O pin.




-- TODO: Write a function like this:
-- GeneratorState -> String
-- It intercalates "_" in between entity names to get to present entity. 

-- TODO: Render component declarations:
-- Write a GeneratorState -> [String] that fills a package will every component in the hierarchy.

-- TODO: Render port map for child entities.
-- TODO: Write the poop command, which poops entire entity hierarchy into 1 big file. 


-- TODO: Make these defaults commandable:
--      * clock name
--      * reset name
--      * data type
--      * width
--      * Every screen parameter
--      * Whether or not GeneratorState embeds all process activity in an if en = '1' statement,
--        and if so, the name of that enable should be programmable. 
--
-- Screen parameters and InfoAssumptions need to be 2 separate structs. 
-- InfoAssumptions is germaine to scripting, while screen parameters are not. 

-- TODO: Change <proc> to proc. Eliminate all the <'s and >'s for things that user will type a lot.

-- TODO: Give proc command an optional sensitivity list. If user wants to use clk100 instead of clk, 
-- or write a combinational process, user can do it.

-- TODO: Give /proc the option to put all inputs in sensitivity list. 

-- TODO: In outermost parser (DecodeOneLine?) make string "ctrl-d" same as holding Ctrl and pressing D, 
-- so that those commands are scriptable. 

-- TODO:
--      * Add 2 sets to Entity:
--          * DeclaredNames
--          * ParsedNames
--          * DeclaredNames takes precedence over ParsedNames
--      * The process has to automatically reset all signals, using declaredNames if available, 
--        otherwise parsedNames. 


-- TODO: Go through all modules and explicity export things. 
-- Avoiding name collisions is starting to be a pain. 

-- Replace all width & height parameters with a struct that can be changed through command.


-- TODO: Add rising edge and falling edge commands. 
--       re <signal_name>
-- instantiates a rising edge detector like this:
--
-- SIGNAL_NAME_RISING_EDGE: rising_edge_detector port map(
--      clk => clk,
--      ...
--      din => <signal_name>
--      dout => <signal_name>_re
--      );
--
-- Likewise for falling edge.


-- TODO: Think of a way a single Hs file can run TuiState through its paces. 
-- Ask on a forum if I need to. 
-- We're getting to the point we need to run a battery of tests before every git commit:
--      * Several dozen tests testing individual features, 
--      * A few dozen more performing slightly harder tests,
--      * A small number (1 < n < 10) writing useful projects.
--      * Every test needs to be provably the same as a user typing on keyboard.
--      * This would also be a great student project, esp. for someone who wants to be an SE in test.

-- TODO: Create a command that wraps any entity in a buffer layer:
--       Mode 1: Single buffer all inputs, all outputs,
--       Mode 2: Double buffer all inputs, all outputs,
--       Mode 3: Double buffer all inputs.

-- TODO: Write a forum post asking why entity box is getting cut off at knees.  

-- TODO: Define TUI commands to change default datatype & width. 

-- TODO: Plan the demo I want to give. Ensure that scope is in tip top shape. Nothing more.
-- Take all other TODO's and either turn them into a ticket or delete.

-- TODO: Write my coding conventions and enable user to invoke them (or not) from TUI.

-- TODO: Eliminate InterspersedCode.

-- TODO: Write a coding convention such that, for all names in VHD signals, put a o_ version in outputs, 
--       and s_ version in signals.
-- TODO: For all inputs, render name as i_.
-- TODO: Create a command that replaces raw names in VHD literals with i_, s_, and o_ variants.
--       Said command should also drive o_ names.

-- TODO: Break all my coding conventions down into small functions. 
-- Create TUI commands that add or remove each function to/from a list. Then, that list 
-- operates on rendered VHDL before user sees it (and/or it is dumped to a file.)
--
-- Think/ask whether there's a way to use type system to enforce applying functions in any order
-- gives the same result. 


-- TODO: Give child entities read-only access to signals assigned in a parent.

-- TODO: By default, reading a name from a parent routes that signal infinitely down. If any child in 
-- any generation reads that signal but does not write it, that name is routed through the hierarchy.
-- If a child entity assigns the same name, then the above rule nolonger applies. The parent name cannot
-- dig deeper than a child that assigns it, and a new signal is declared in the child entity.
-- This command has 2 modifiers:
--      1) Turn it off completely, 
--      2) The route down command, which disallows child entities from assigning the same name. 

-- TODO: Create the route down command, which puts a signal assigned in a parent in scope of all of its
-- children. In other words, the same name cannot be assigned in a child entity. 

-- TODO: Create the route up command, which puts a signal assigned in a child in the scope of all of its
-- parents. A parent cannot assign the same name. 






-- TODO: Recruit 1+ people to perform:
--       * Convert GeneratorState into a Hs file that reproduces an identical GeneratorState.
--       * Record all TUI commands and run them to create an identical GeneratorState.
--
-- There is a workflow reason to do both:
--       * User may need several TUI sessions to get GeneratorState right.
--       * User may then want to take the resulting Hs file and modify it, call additional Hs,
--         feed the script into some other Hs, etc. 


-- TODO: Before displaying name to TUI, first check whether user has declared a similar name.
--       If so, steal datatype, width, etc. from that name.
--       NOTE: Identical name should take precedence over similar name. That way, if user 
--       doesn't like the automatically extracted version, they can declare the exact name 
--       with whatever specs they want.

-- TODO: Take Information's as they appear on the screen and render Hs to a file.
-- TODO: Append every command to a file.

-- TODO: Define an interface for automatically applying coding convention. 
--
-- Perhaps define a struct called RenderedCodePack, which contains:
-- 
--      Everything user entered :: [String]
--      Parsed Names :: InfoPack
--      What User Sees :: [String]
--
-- Coding conventions accept the interface RenderedCodePack -> RenderedCodePack.

-- TODO: Create a command that allows user to change default datatype, default width.


-- TODO: 
-- When user uses similar names for both input and output, Hs should declare s_ and o_ versions of the same name. 
-- Anything that is a signal in an above entity should be declared as an input - If the flag enabling that feature is set.
-- Any signal that user uses but never assigns should be declared as an input.

-- TODO: Send user a message when all inputs to an entity have a signal (or input) with a similar name in the 
-- parent entity. Say, "Perfect Input Subset". That way, user instantly knows when all information going into a module
-- has been created.

-- TODO: test user messages

-- TODO: Allow user to turn Info prefixes on & off from TUI.
-- User should be able to type:
--
--          x <= din;
--
-- and either see that, or:
--
--          s_x <= i_din;
-- ...
--          o_x <= s_x;
--
-- just by changing a command. 

-- TODO: Allow user to declare constants


-- TODO: Make using literals easy:
--      * When a literal is narrower than a signal, zero pad it.
--          * Right justify by default
--          * Provide option to left justify
--      * When a literal is wider than a signal, 
--          * Highlight it in red


-- TODO: Make faculties for recursive entities. 

-- TODO: Give user the freedom to declare infinite signals. 
-- TODO: Give user the freedom to say myInfiniteSignal{x+42} which takes some variable 
-- x, adds 42 to it, and uses that delay number for that call.
-- The larger a block of infinite signals which are used, the closer that block is to the end 
-- of the .vhd file. 

-- TODO: Supposing I did support infinite widths, think about what the termination condition would 
-- be. Don't support infinite widths unless I can find a termination condition that somehow saves
-- time compared to writing a constant. Alternatively, give user a command which converts every 
-- signal name into a constant, for instance:
-- 
--      mySignalName : std_logic_vector(31 downto 0);
-- 
-- becomes:
--      
--      MY_SIGNAL_NAME_WIDTH : integer := 32;
--
-- and every signal for which mySignalName is a stub would then have width:
--
--      mySignalName_0431 : std_logic_vector(MY_SIGNAL_NAME_WIDTH-1 downto 0);
--


-- TODO: Add commands to modify ProjectParameters.
-- You should be able to change reset style in all processes with a single command. 

-- TODO: Make every parameter of code generation modifiable by a command.

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

-- TODO: Extend VHDL to include the uEntity. 

-- TODO: Extend VHDL to allow passing in of architectures. 
-- From the top of my hierarchy, I should be able to pass in any architecture that conforms to whatever entity, 
-- and pass that architecture down through several layers until it is instantiated. 
-- (Might be transpiled to a configuration in the output VHDL, but what's important is that user can easily modify
-- program from the top.)

-- TODO: Allow architectures to be output signals. Hs should be able to analyze anything & everything in GeneratorState
-- in order to design an architecture that can be passed to some other module. 

-- TODO: Think about a subset of the C language I can embed in this project. 
-- The difference between this and normal HLS stuff is that mine will have separate loop types.


