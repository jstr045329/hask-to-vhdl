library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use IEEE.math_real.ALL;

package ClockCrossingPkg is


function calculateClockRatio (
    f1 : real;
    f2 : real
    ) return positive;


function calculateBias (
    raw_int : integer;
    demand_odd : std_logic;
    demand_minimum_1 : std_logic
    ) return integer;


function calculateNumDelays (
    f1 : real;
    f2 : real
    ) return positive;

end package ClockCrossingPkg;


package body ClockCrossingPkg is


function calculateClockRatio (
    f1 : real;
    f2 : real
    ) return positive is 
begin 
    if (f1 < f2) then 
        report "f1 must be greater than f2!" severity error;
    end if;
    return positive(ceil(f1 / f2));
end function;


function calculateBias (
    raw_int : integer;
    demand_odd : std_logic;
    demand_minimum_1 : std_logic
    ) return integer is 
variable y : integer;
begin 
    return 1;
end function;


function calculateNumDelays (
    f1 : real;
    f2 : real
    ) return positive is 
    constant demand_odd : std_logic := '1';
    constant demand_minimum_1 : std_logic := '1';
    variable raw_int : positive;
    variable bias : positive;
begin 
    -- TODO: Draw a picture to decide whether output must be > raw_int, whether result must be odd, etc. 
    raw_int := calculateClockRatio(f1, f2);
    bias := calculateBias(raw_int, demand_odd, demand_minimum_1);
    return raw_int + bias;
end function;

end package body ClockCrossingPkg;

