------------------------------------------------------------------------------------------------------------------------
--                                 Many Input Definitions For Basic Logic Functions 
--
-- This module contains definitions for basic logic functions 1) using prefix functional notation (as opposed to 
-- infix operator notation) and 2) with most inputs optional. Note that NAND and NOR are restricted to 2 inputs for 
-- a reason. (They are not like AND and OR where you can reverse the associativity and get the same result.) Therefore 
-- make sure the ()'s used in NAND and NOR definitions are the way you want. At the time of this writing (2020), 
-- Xilinx LUTs are 6-input. I defined these functions with up to 8 inputs so that the library could grow with the 
-- technology for a few years, but it might be necessary at some point to define greater numbers of inputs in order 
-- to make use of additional LUT inputs. You should only use as many inputs as your device can accept in a single 
-- LUT, otherwise the synthesizer will start adding layers without necessarily telling you; which, obviously, can 
-- cause timing problems. Only the first input is mandatory. All inputs after the first get some reasonable default 
-- value so you don't have to use them. That effectively makes these functions variadic. You can use as many or as 
-- few as you like. My motivation for doing this is that ceil(log5(128)) = 4. In other words, if you use 5 inputs 
-- in each LUT, you only need 4 layers of LUTs to accept 128 inputs which obviously is quite useful. I thought about 
-- using a for-loop for this logic, which is good for about 5-10 layers of LUTs and therefore 5-10 inputs. So I said, 
-- For only a few minutes of work, using hask-to-vhdl, I can get 128 inputs and still come out with slightly better 
-- timing. So I did. In principle you could get 256*5 = 1280 inputs in a single clock while only using about 6 layers, 
-- so you could definitely take this idea farther; however, I decided that 127 was probably overkill as it is. But 
-- if there's ever a reason to in the future, you could probably get more aggressive with this idea if you wanted. 
-- If you do, however, you should also think about how easy or difficult it will be for the synthesizer to place 
-- that many LUTs in close enough proximity. In other words, if you decide to get aggressive with this idea, the 
-- limiting factor may become the propagation time from point A to point B in the fabric (i.e. trace capacitance), 
-- even if LUT response time seems low. 
--
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


package PrefixFunctions is


function and_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic;


function or_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic;


function nand_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic;


function nor_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic;


function xor_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic;


end package PrefixFunctions;


package body PrefixFunctions is

function and_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic is 
begin 
    return x0 and x1 and x2 and x3 and x4 and x5 and x6 and x7;
end function;


function or_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic is 
begin 
    return x0 or x1 or x2 or x3 or x4 or x5 or x6 or x7;
end function;


function nand_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic is 
begin 
    return ((x0 nand x1) nand (x2 nand x3)) nand ((x4 nand x5) nand (x6 nand x7));
end function;


function nor_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic is 
begin 
    return ((x0 nor x1) nor (x2 nor x3)) nor ((x4 nor x5) nor (x6 nor x7));
end function;


function xor_function(
    x0 : std_logic;
    x1 : std_logic := '0';
    x2 : std_logic := '0';
    x3 : std_logic := '0';
    x4 : std_logic := '0';
    x5 : std_logic := '0';
    x6 : std_logic := '0';
    x7 : std_logic := '0'
    ) return std_logic is 
begin 
    return x0 xor x1 xor x2 xor x3 xor x4 xor x5 xor x6 xor x7;
end function;


end package body PrefixFunctions;
