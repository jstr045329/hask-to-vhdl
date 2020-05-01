module Rendering.Function where
import Rendering.InfoTypes
import Rendering.VhdMath
import Tools.WhiteSpaceTools


-- fInputs should be VhdSig's
data Function = FunctionDef {
      fNomen :: String
    , fInputs :: [Information]
    , fConstants :: [Information]
    , fVariables :: [Information]
    , returnTypeAs :: Information
    , fLines :: [String]
    }

    | FunctionCall {
      fNomen :: String
    , fInputs :: [Information]
    , returnDest :: Information
    } deriving (Eq, Show)


renderFunction :: Function -> [String]
renderFunction (FunctionCall _ _ _) = error "This should never happen"
renderFunction fDef =
    ["function " ++ (fNomen fDef) ++ "("] ++ 
    (zipTab (declareBatch (fInputs fDef))) ++
    [(tab 1) ++ " return " ++ (datatypeToStr (dataType (returnTypeAs fDef)) (width (returnTypeAs fDef))) ++ " is"] ++
    (zipTab (fLines fDef)) ++
    ["end function;"]


-- TODO: Modify nomenIsFormatted in InfoTypes so that generics, constants, and literals do not
--       need _d0000
-- TODO: Add to InfoTypes functions that convert a signal to a variable,
--      signal to port, port to signal, etc. 
--      Not just the name, but the whole hashmap.
-- FIRST THOUGH verify I haven't already done that.

getFuncInputList :: [Information] -> String
getFuncInputList fInList
    | fInList == [] = ""
    | (length fInList) == 1 = (nomen (head fInList))
    | otherwise = (nomen (head fInList)) ++ ", " ++ (getFuncInputList (tail fInList))


callFunction :: Function -> [String]
callFunction (FunctionDef _ _ _ _ _ _) = error "This should never happen"
callFunction fCall =
    [(nomen rDest) ++ " " ++ (assignmentOperator rDest) ++ " " ++ (fNomen fCall) ++
    "(" ++ (getFuncInputList (fInputs fCall)) ++ ");"]
    where
        rDest = returnDest fCall

-- Note: It is okay if the library is large, so long as
-- (code generated) / (code written) is also large. 
--
-- Is this concept only practical for large, repetitive structures?
-- VHDL is strange in that it offers some automation features, yet
-- little in the way of abstraction. What exactly is the goal of 
-- this library? Is it to generate a lot of code with a little?
-- Or is it to offer abstractions that VHDL does not have?
-- If I want to write Haskell and get VHDL, perhaps the most 
-- straightforward way to do that is to use Clash, for Clash is 
-- an implementation of the Haskell language. On the other hand,
-- if the goal is to parse and manipulate VHDL, that is a very
-- different goal. In that case, writing a module for every language
-- feature makes sense. 

-- I can see now that I've been conflating 2 goals that, while they
-- might be semi-harmonious, are different:
--
--      1)  Write a MATLAB-like environment for manipulating code,
--
--      2)  Generate a lot of VHDL from a little Haskell.
--          The MATLAB-like environment for maniputing code should also be
--          able to generate code. That's one reason why I'm having a hard time
--          making up my mind: Goal 1 encompasses both goals. 
--
-- All the modeling/parsing modules give the computer a way to "understand"
-- the code it is parsing; another folder could be dedicated to recognizing
-- design patterns. That said, goal 1 does not require goal 2, and vice 
-- versa. That may be why I feel conflicted as to whether I am moving 
-- towards my goal or not - I'm moving towards 2 goals that are not the same.
-- 
-- If the sole goal of this library were to generate a lot of VHDL from a
-- small amount of Haskell (ratio > 5, say), the results of that goal 
-- would be of limited usefulness. That goal really does help with things
-- like large look up tables and other formulaic structures. However,
-- formulaic structures are not profound. They are formulaic. An import exception
-- is when VHDL requires you to type in monads by hand. Some of the most time
-- consuming VHDL coding is spoon feeding a Kronecker product to the compiler:
--      x0001 <= f0001(m0001, n0001, y0001...);
--      x0002 <= f0002(m0002, n0002, y0002...);
--      ...
-- 
-- Perhaps the hardest thing to accept about VHDL, and the biggest time waster,
-- is the fact that you have to change a lot of things in 3-4 places. One 
-- reason I went down this path originally is to let the user write nested
-- entities as easily as you write nested classes in Java, or new functions in
-- Python. Once you take a small design structure, perhaps only 1 line of code,
-- and wrap it in its own entity, you have made that module self contained, 
-- maintainable; and in some cases, easier to read. A lot of the 3-4 places of
-- changing just to accomodate a trivial change has to do with ports. To give
-- a port a more expressive name, you have to change it everywhere it is used
-- or assigned; then you have to change it in the entity declaration; then the
-- component declaration; then, by convention, the testbench. In Haskell, that
-- signal becomes a function, and you can pass the same function into everything
-- that uses the signal it represents. 
-- 
-- To meet goal #2, generating lots of VHDL with a little Haskell, perhaps a 
-- better goal than huge ratios is:
--
--      2.A)    In most cases, shoot for 2:1, but when you change a signal name in 1 
--              place, you change it in 1 place. 
--
--      2.B)    Make it easier to write 1 line of VHDL, then spawn that off as an
--              entity. 
--
-- One reason VHDL cannot accept terser code in the first place is that you have
-- to spell out how many bits every signal/port needs for almost all synthesizable
-- situations. Even some simulation situations require this. In support of goal 
-- 2.B, let's add another:
-- 
--      2.C)    Write port declarations without explicitly stating signal width,
--              but give Haskell the tools it needs to fill in those details. 
--
-- For example, write an entity like this:
--
--  entity my_entity is
--      port(
--          x : in std_logic;
--          ...
--          );
--  end my_entity;
--
-- and give Haskell the tools it needs to figure out whether x should be std_logic,
-- std_logic_vector, how wide it is, etc. Perhaps a better way of 
-- achieving the same end is to write an entity like this:
--
--  entity my_entity is
--      port(
--          x : in;
--          ...
--      )
--  end my_entity;
--
-- The ";" right after "in" is a way of telling Haskell to figure out what datatype x should
-- be, how wide, etc. This figuring out of the details happens every time the 
-- entity is INSTANTIATED, not when it is written. Therefore, if Haskell needs to generate 
-- 10,000 different variations of the entity, we don't care, because Haskell will generate human
-- readable files that allow us to see exactly what the compiler sees. 
-- 
-- An example of goal 2.B is to write a line that looks like this:
--
--      spawn_reg_ent ["y <= x;"]
--
-- and Haskell will generate something like this:
--
--  library IEEE;
--  use IEEE.std_logic_1164.all;
--
--  entity my_entity_0x43D04F90 is
--      port(
--          clk : in std_logic;
--          rst : in std_logic;
--          x : in std_logic_vector(47 downto 0);
--          y : out std_logic_vector(47 downto 0)
--      );
--  end my_entity_0x43D04F90;
--
--  architecture behavioral_0x43D0F90 of my_entity_0x43D04F90 is
--      signal y0 : std_logic_vector(47 downto 0);
--  begin
--
--  process(clk)
--  begin
--      if rising_edge(clk) then
--          if rst = '1' then
--              y0 <= (others => '0') after 1 ns;
--          else
--              y0 <= x after 1 ns;
--          end if;
--      end if;
--  end process;
--  
--  y <= y0;
--
--  end behavioral_0x43D0F90;
--
-- Remember, details such as std_logic_vector(47 downto 0) are calculated
-- every time the design pattern is USED, not when you type in "y <= x;".
-- That way, you can use the same design pattern 1000's of times, with 
-- many different datatypes, and the VHDL compiler is always spoon fed
-- the correct details. This gives the user the ability to think at the 
-- level of design patterns, rather than trying (in vain) to write abstract
-- VHDL. The astute reader might wonder, "Isn't this just undoing some of
-- the effort that went into the VHDL compiler in the first place?" and the
-- answer is yes... in a way. It helps to remember the original motivation
-- of VHDL. The language was originally designed by the US Government as a 
-- way of documenting existing circuits, which explains why the language 
-- intentionally allows you to design bad circuits, infer latches even though
-- inferred latches are bad; and some features of the language that seem
-- bizarre at first. The reason VHDL cannot be more general in useful ways
-- is that it is overly general in counterproductive ways. In the early 80's,
-- Xilinx supported VHDL for synthesis, partly because the US Government already
-- had large amounts of VHDL that it wanted to replicate in FPGAs, and partly
-- because the only other HDL (Verilog) was in its infancy. VHDL won by 
-- default, and Xilinx people, funded by a bloated bureacracy, decided to 
-- implement the language in all its flaws, rather than design a VHDL-inspired
-- language capable of making reasonable assumptions on your behalf.
-- (Which, by the way, is what every successful engineer does. Managers do not 
-- like engineers who take the Somerville approach of asking lots of questions.
-- FWIW, Somerville is the worst thing that ever happened to me. It took me
-- the better part of a decade to figure out that, while that book makes a
-- lot of sense in an academic vacuum, few coworkers are willing to cooperate
-- with the level of dialog Somerville demands. In other words, asking
-- lots of questions about what software should do, use cases, etc., almost
-- always engenders ill will. You're almost always better off building something 
-- you know is wrong and letting people tell you what's wrong with it.)
--  
-- In short, the single biggest win for this library is to support single 
-- line spawning. 
--
--      spawn_reg_ent [list of strings]
--
-- The other single biggest win for this library is to be able to pass a monadic
-- expression into some function, and have Haskell render the Kronecker product.
-- 
-- Recreating (in Haskell) the VH-REPL I started in 2017 might be really useful 
-- for the code manipulation environment. Creating a REPL-like window that 
-- allows new modules to be spun off in a whim with just a few keystrokes would 
-- be useful in any language. A few more keystrokes would create a new library
-- containing a Kronecker product of constants. 




 
