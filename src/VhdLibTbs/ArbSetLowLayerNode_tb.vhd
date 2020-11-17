------------------------------------------------------------------------------------------------------------------------
--                                          Testbench for ArbSetLowLayerNode
------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;
use work.PrefixGatesPkg.all;
use work.VhdSynthToolsPkg.all;
use work.ArbSetPkg.all;


entity ArbSetLowLayerNode_tb is
end ArbSetLowLayerNode_tb;


architecture behavioral_ArbSetLowLayerNode_tb of ArbSetLowLayerNode_tb is

signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal s_number_to_add : std_logic_vector(NUMBER_WIDTH-1 downto 0);
signal s_add_enable : std_logic;
signal s_number_to_remove : std_logic_vector(NUMBER_WIDTH-1 downto 0);
signal s_remove_enable : std_logic;
signal s_number_to_test : std_logic_vector(NUMBER_WIDTH-1 downto 0);
signal s_test_enable : std_logic;
signal s_predecessor_occupied : std_logic;
signal s_number_stored : std_logic_vector(NUMBER_WIDTH-1 downto 0);
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
    s_number_to_add <= (others => '0');
    s_add_enable <= '0';
    s_number_to_remove <= (others => '0');
    s_remove_enable <= '0';
    s_number_to_test <= (others => '0');
    s_test_enable <= '0';
    s_predecessor_occupied <= '1';
    test_stage <= 0;
    sync_wait_rising(clk, 10);
    rst <= not rst;
    sync_wait_rising(clk, 10);

    -- Verify number is added:
    s_number_to_add <= x"0000000000abcdef";
    strobe_rising(clk, s_add_enable);
    sync_wait_rising(clk, 10);
    assert s_number_stored = x"0000000000abcdef";
    sync_wait_rising(clk, 10);
    
    -- Verify number is NOT added:
    s_number_to_add <= x"0000000000123456";
    strobe_rising(clk, s_add_enable);
    s_number_to_add <= (others => '0');
    sync_wait_rising(clk, 10);
    assert s_number_stored = x"0000000000abcdef";
    assert s_occupied = '1';
    sync_wait_rising(clk, 10);
    
    -- Verify number is removed:
    s_number_to_remove <= x"0000000000abcdef";
    strobe_rising(clk, s_remove_enable);
    sync_wait_rising(clk, 10);
    assert s_number_stored = x"0000000000000000";
    assert s_occupied = '0';
    sync_wait_rising(clk, 10);
        
    -- Verify number is added:
    s_number_to_add <= x"0000037012345678";
    strobe_rising(clk, s_add_enable);
    sync_wait_rising(clk, 10);
    s_number_to_add <= (others => '0');
    sync_wait_rising(clk, 10);
    assert s_number_stored = x"0000037012345678";
    sync_wait_rising(clk, 10);
    
    -- Verify number belongs:
    s_number_to_test <= x"0000037012345678";
    strobe_rising(clk, s_test_enable);
    assert s_membership = '1';
    sync_wait_rising(clk, 10);
    
    -- Verify some other number does NOT belong:
    s_number_to_test <= x"0001111110000000";
    strobe_rising(clk, s_test_enable);
    assert s_membership = '0';
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
UUT: ArbSetLowLayerNode
    port map (
        clk => clk,
        rst => rst,
        i_number_to_add => s_number_to_add,
        i_add_enable => s_add_enable,
        i_number_to_remove => s_number_to_remove,
        i_remove_enable => s_remove_enable,
        i_number_to_test => s_number_to_test,
        i_test_enable => s_test_enable,
        i_predecessor_occupied => s_predecessor_occupied,
        o_number_stored => s_number_stored,
        o_occupied => s_occupied,
        o_membership => s_membership
    );


end architecture behavioral_ArbSetLowLayerNode_tb;

