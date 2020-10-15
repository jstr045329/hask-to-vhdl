library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.VhdSynthToolsPkg.all;


package PrefixGatesPkg is


function prefix_and (
    x0 : boolean
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return std_logic;


function prefix_and (
    x0 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return boolean;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return std_logic;


function prefix_or (
    x0 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return boolean;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return boolean;


end package PrefixGatesPkg;


package body PrefixGatesPkg is

function prefix_and (
    x0 : boolean
    ) return std_logic is 
begin 
    return bool2SL(x0);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2) and bool2SL(x3);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2) and bool2SL(x3) and bool2SL(x4);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2) and bool2SL(x3) and bool2SL(x4) and bool2SL(x5);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2) and bool2SL(x3) and bool2SL(x4) and bool2SL(x5) and bool2SL(x6);
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) and bool2SL(x1) and bool2SL(x2) and bool2SL(x3) and bool2SL(x4) and bool2SL(x5) and bool2SL(x6) and bool2SL(x7);
end function;


function prefix_and (
    x0 : boolean    
    ) return boolean is 
begin 
    return x0;
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2, x3));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2, x3, x4));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2, x3, x4, x5));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2, x3, x4, x5, x6));
end function;


function prefix_and (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_and(x0, x1, x2, x3, x4, x5, x6, x7));
end function;


function prefix_or (
    x0 : boolean
    ) return std_logic is 
begin 
    return bool2SL(x0);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2) or bool2SL(x3);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2) or bool2SL(x3) or bool2SL(x4);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2) or bool2SL(x3) or bool2SL(x4) or bool2SL(x5);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2) or bool2SL(x3) or bool2SL(x4) or bool2SL(x5) or bool2SL(x6);
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return std_logic is 
begin 
    return bool2SL(x0) or bool2SL(x1) or bool2SL(x2) or bool2SL(x3) or bool2SL(x4) or bool2SL(x5) or bool2SL(x6) or bool2SL(x7);
end function;


function prefix_or (
    x0 : boolean    
    ) return boolean is 
begin 
    return x0;
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2, x3));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2, x3, x4));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2, x3, x4, x5));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2, x3, x4, x5, x6));
end function;


function prefix_or (
    x0 : boolean; 
    x1 : boolean; 
    x2 : boolean; 
    x3 : boolean; 
    x4 : boolean; 
    x5 : boolean; 
    x6 : boolean; 
    x7 : boolean    
    ) return boolean is 
begin 
    return sl2Bool(prefix_or(x0, x1, x2, x3, x4, x5, x6, x7));
end function;


end package body PrefixGatesPkg;
