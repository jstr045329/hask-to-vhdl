library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.BitPaddingTools.all;


entity BitPaddingTools_tb is 
end BitPaddingTools_tb;

architecture behavioral_BitPaddingTools_tb of BitPaddingTools_tb is 


signal clk : std_logic := '1';
signal rst : std_logic := '1';
constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';
signal test_stage : integer := 0;

signal x : std_logic_vector(7 downto 0) := (others => '1');

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
    rst <= '1';
    x <= (others => '1');
    wait for clk_per*10;
    rst <= '0';
    x <= zeros(4) & x(3 downto 0);
    wait for clk_per*10;
    sim_done <= '1';
    wait;
end process;


end behavioral_BitPaddingTools_tb;

