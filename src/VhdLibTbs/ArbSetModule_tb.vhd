------------------------------------------------------------------------------------------------------------------------
--                                             Testbench for ArbSetModule
------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;
use work.PrefixGatesPkg.all;
use work.VhdSynthToolsPkg.all;
use work.ArbSetPkg.all;


entity ArbSetModule_tb is
end ArbSetModule_tb;


architecture behavioral_ArbSetModule_tb of ArbSetModule_tb is

signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal s_din : std_logic_vector(NUMBER_WIDTH-1 downto 0);
signal s_add_enable : std_logic;
signal s_remove_enable : std_logic;
signal s_test_enable : std_logic;
signal s_occupied : std_logic;
signal s_membership : std_logic;

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';
signal test_stage : integer := 0;
signal s_clock_cycle_count : integer := 0;


begin


------------------------------------------------------------------------------------------------------------------------
--                                                    Boiler Plate
------------------------------------------------------------------------------------------------------------------------
CLOCK_PROCESS: process
begin
    if sim_done = '1' then
        wait;
    else
        wait for clk_per/2;
        clk <= not clk;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                 Count Clock Cycles
------------------------------------------------------------------------------------------------------------------------
CLOCK_CYCLE_COUNTER: process(clk)
begin
    if rising_edge(clk) then 
        s_clock_cycle_count <= s_clock_cycle_count + 1;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                    Stim Process
------------------------------------------------------------------------------------------------------------------------
STIM_PROCESS: process
begin
    s_din <= (others => '0');
    s_add_enable <= '0';
    s_remove_enable <= '0';
    s_test_enable <= '0';
    test_stage <= 0;
    sync_wait_rising(clk, 10);
    rst <= not rst;
    sync_wait_rising(clk, 10);

    s_din <= x"0000000000005432";
    strobe_rising(clk, s_add_enable);
    s_din <= x"0000000123456789";
    strobe_rising(clk, s_add_enable);
    s_din <= x"0000006666666666";
    strobe_rising(clk, s_add_enable);
    s_din <= x"0000aaaaaaaaaaaa";
    strobe_rising(clk, s_add_enable);
    s_din <= x"7777777777777777";
    strobe_rising(clk, s_add_enable);
    
    s_din <= (others => '0');
    sync_wait_rising(clk, 10);
    
    s_din <= x"0000000000005432";
    strobe_rising(clk, s_test_enable);
    s_din <= x"0000000123456789";
    strobe_rising(clk, s_test_enable);
    s_din <= x"0000006666666666";
    strobe_rising(clk, s_test_enable);
    s_din <= x"0000aaaaaaaaaaaa";
    strobe_rising(clk, s_test_enable);
    s_din <= x"7777777777777777";
    strobe_rising(clk, s_test_enable);
    
    sync_wait_rising(clk, 10);

    -- Test ends here:
    test_stage <= test_stage + 1;
    sync_wait_rising(clk, 100);
    sim_done <= '1';
    wait;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                  Unit Under Test
------------------------------------------------------------------------------------------------------------------------
UUT: ArbSetModule
    port map (
        clk => clk,
        rst => rst,
        i_din => s_din,
        i_add_enable => s_add_enable,
        i_remove_enable => s_remove_enable,
        i_test_enable => s_test_enable,
        o_occupied => s_occupied,
        o_membership => s_membership
    );


end architecture behavioral_ArbSetModule_tb;


