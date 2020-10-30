------------------------------------------------------------------------------------------------------------------------
--                                          Testbench for PowerSet_0016_0016
------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;
use work.PowerSetPkg.all;


entity PowerSet_0016_0016_tb is
end PowerSet_0016_0016_tb;


architecture behavioral_PowerSet_0016_0016_tb of PowerSet_0016_0016_tb is


constant rst_level : std_logic := '1';
constant rst_value : std_logic := '0';
constant clk_edge : std_logic := '1';
constant allow_fast_reset : std_logic := '1';
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal s_bit_num_1_000000 : integer := 0;
signal s_set_num_1_000000 : integer := 0;
signal s_add_000000 : std_logic;
signal s_rem_000000 : std_logic;
signal s_set_num_2_000000 : integer;
signal s_dest_num_000000 : integer;
signal s_union_000000 : std_logic;
signal s_intersection_000000 : std_logic;
signal s_complement_000000 : std_logic;
signal s_duplicate_000000 : std_logic;
signal s_read_set_num_0000_000000 : integer;
signal s_read_bit_num_0000_000000 : integer;
signal s_read_enable_0000_000000 : std_logic;
signal s_member_0000_000000 : std_logic;
signal s_out_valid_0000_000000 : std_logic;
signal s_read_set_num_0001_000000 : integer;
signal s_read_bit_num_0001_000000 : integer;
signal s_read_enable_0001_000000 : std_logic;
signal s_member_0001_000000 : std_logic;
signal s_out_valid_0001_000000 : std_logic;
signal s_read_set_num_0002_000000 : integer;
signal s_read_bit_num_0002_000000 : integer;
signal s_read_enable_0002_000000 : std_logic;
signal s_member_0002_000000 : std_logic;
signal s_out_valid_0002_000000 : std_logic;
signal s_read_set_num_0003_000000 : integer;
signal s_read_bit_num_0003_000000 : integer;
signal s_read_enable_0003_000000 : std_logic;
signal s_member_0003_000000 : std_logic;
signal s_out_valid_0003_000000 : std_logic;
signal s_collision_warning_000000 : std_logic;

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
    s_bit_num_1_000000 <= 0;
    s_set_num_1_000000 <= 0;
    s_add_000000 <= '0';
    s_rem_000000 <= '0';
    s_set_num_2_000000 <= 0;
    s_dest_num_000000 <= 0;
    s_union_000000 <= '0';
    s_intersection_000000 <= '0';
    s_complement_000000 <= '0';
    s_duplicate_000000 <= '0';
    s_read_set_num_0000_000000 <= 0;
    s_read_bit_num_0000_000000 <= 0;
    s_read_enable_0000_000000 <= '0';
    s_read_set_num_0001_000000 <= 0;
    s_read_bit_num_0001_000000 <= 0;
    s_read_enable_0001_000000 <= '0';
    s_read_set_num_0002_000000 <= 0;
    s_read_bit_num_0002_000000 <= 0;
    s_read_enable_0002_000000 <= '0';
    s_read_set_num_0003_000000 <= 0;
    s_read_bit_num_0003_000000 <= 0;
    s_read_enable_0003_000000 <= '0';
    test_stage <= 0;
    sync_wait_rising(clk, 10);
    rst <= not rst;
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
UUT: PowerSet_0016_0016
    generic map (
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        allow_fast_reset => allow_fast_reset
    )
    port map (
        clk => clk,
        rst => rst,
        i_bit_num_1 => s_bit_num_1_000000,
        i_set_num_1 => s_set_num_1_000000,
        i_add => s_add_000000,
        i_rem => s_rem_000000,
        i_set_num_2 => s_set_num_2_000000,
        i_dest_num => s_dest_num_000000,
        i_union => s_union_000000,
        i_intersection => s_intersection_000000,
        i_complement => s_complement_000000,
        i_duplicate => s_duplicate_000000,
        i_read_set_num_0000 => s_read_set_num_0000_000000,
        i_read_bit_num_0000 => s_read_bit_num_0000_000000,
        i_read_enable_0000 => s_read_enable_0000_000000,
        o_member_0000 => s_member_0000_000000,
        o_out_valid_0000 => s_out_valid_0000_000000,
        i_read_set_num_0001 => s_read_set_num_0001_000000,
        i_read_bit_num_0001 => s_read_bit_num_0001_000000,
        i_read_enable_0001 => s_read_enable_0001_000000,
        o_member_0001 => s_member_0001_000000,
        o_out_valid_0001 => s_out_valid_0001_000000,
        i_read_set_num_0002 => s_read_set_num_0002_000000,
        i_read_bit_num_0002 => s_read_bit_num_0002_000000,
        i_read_enable_0002 => s_read_enable_0002_000000,
        o_member_0002 => s_member_0002_000000,
        o_out_valid_0002 => s_out_valid_0002_000000,
        i_read_set_num_0003 => s_read_set_num_0003_000000,
        i_read_bit_num_0003 => s_read_bit_num_0003_000000,
        i_read_enable_0003 => s_read_enable_0003_000000,
        o_member_0003 => s_member_0003_000000,
        o_out_valid_0003 => s_out_valid_0003_000000,
        o_collision_warning => s_collision_warning_000000
    );


end architecture behavioral_PowerSet_0016_0016_tb;


