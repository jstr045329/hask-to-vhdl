----------------------------------------------------------------------------------------------------
--                                  Testbench for StickyWarning
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;

entity StickyWarning_tb is
end StickyWarning_tb;


architecture behavioral_StickyWarning_tb of StickyWarning_tb is


component StickyWarning is generic ( counter_width : integer := 8 ; 
clks_high : integer := 128 ) ; 
port ( clk : in std_logic ; 
rst : in std_logic ; 
en : in std_logic ; 
trigger : in std_logic ; 
soft_sticky : out std_logic ; 
hard_sticky : out std_logic
) ; 
end component ; 


constant counter_width : integer := 8;
constant clks_high : integer := 128;
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal en : std_logic;
signal trigger : std_logic;
signal soft_sticky : std_logic;
signal hard_sticky : std_logic;

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';
signal test_stage : integer := 0;


begin


----------------------------------------------------------------------------------------------------
--                                          Boiler Plate
----------------------------------------------------------------------------------------------------
CLOCK_PROCESS: process
begin
    if sim_done = '1' then
        wait;
    else
        wait for clk_per/2;
        clk <= not clk;
    end if;
end process;


----------------------------------------------------------------------------------------------------
--                                          Stim Process
----------------------------------------------------------------------------------------------------
STIM_PROCESS: process
begin
    en <= '0';
    trigger <= '0';
    wait for clk_per*10;
    rst <= not rst;
    wait for clk_per*10;
    
    -- Strobe trigger for 1 clk:
    strobe_rising(clk, trigger);
    
    -- Wait a few clocks to see the effects:
    wait for clk_per*300;
    
    -- ...and we're done!
    sim_done <= '1';
    wait;
end process;




----------------------------------------------------------------------------------------------------
--                                        Unit Under Test
----------------------------------------------------------------------------------------------------
UUT
: 
StickyWarning
generic map (
    clks_high => clks_high,
    counter_width => counter_width
)
port map (
    clk => clk,
    rst => rst,
    en => en,
    trigger => trigger,
    soft_sticky => soft_sticky,
    hard_sticky => hard_sticky
);


end architecture behavioral_StickyWarning_tb;


