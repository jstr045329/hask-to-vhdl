library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;


entity MinFunction is
    generic (
        -- Only use i_distance_xx when an input is smaller than this:
        max_integer : integer := (2**30)-2
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        i_soft_rst : in std_logic;
        <inputs_here>
        o_min : out integer;
        o_latency : out integer
    );
end MinFunction;


architecture behavioral_MinFunction of MinFunction is


<signals_here>

signal s_smallest_value : integer; 

begin

process(clk)
<variables_here>
begin
    if rising_edge(clk) then 
        if (i_soft_rst or rst) = '1' then 
            <reset_intermediates_here>
            
        else
            
            <business_logic_here>
            
        end if;
    end if;
end process;

<drive_output_here>
<latency_here>

end behavioral_MinFunction;

