module Rendering.VhdLibraries where


data StandardLibs = 
            StdLogicLib
        |   StdArithLib
        |   NumericBitLib
        |   NumericStdLib
        |   SignedLib
        |   UnsignedLib
        |   RealLib
        |   ComplexLib
        |   TextIOLib
            deriving (Eq, Show)


data VhdLib = 
          UseAll String  
        | UseSome String [String]
          deriving (Eq, Show)

-- TODO: Think about the differences between use IEEE.std_logic_1164.all and
-- use work.my_pkg.all;

-- TODO: Use set to detect whether lib declaration is necessary

-- TODO: Write functions that make common libs

-- TODO: Divide codebase into parsing and generation (separate directories)

-- TODO: Write a function that:
--          1) Blows away all existing function/procedure prototypes
--          2) Extracts all subprograms from a package,
--          3) Isolates prototype for each, and 
--          4) Inserts prototypes in package header.
-- When done, you should be able to simply write a package body, optionally
-- put constants/types in package header, and then this function will fill in 
-- all the prototypes for you, even if existing prototypes are wrong. 

-- TODO: After the previous function is finished, write a function that:
--          1) Scans your entire project,
--          2) Finds any port maps that don't contain the latest interface,
--          3) Comments out any ports that were removed,
--          4) Inserts any ports that were added, 
--          5) Appends a comment to added ports like this: -- *** Fill this in ***
-- Do something similar for generics
-- Since this function will require an I/O layer, the I/O layer can print out 
-- a report of what was done, so the user has a convenient little checklist of 
-- things that need to be done. 


-- TODO: When making I/O layer for all this, store every change to user's code
-- in a database. Then, it should be possible to create an animation that shows
-- user step-by-step how their code changed.

