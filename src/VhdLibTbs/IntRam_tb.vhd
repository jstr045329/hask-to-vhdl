----------------------------------------------------------------------------------------------------
--                                      Testbench for IntRam
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;


entity IntRam_tb is
end IntRam_tb;


architecture behavioral_IntRam_tb of IntRam_tb is


component IntRam is generic ( addr_width : integer := 8 ; 
use_rising_edge : std_logic := '1' ; 
use_sync_reset : std_logic := '1' ; 
rst_level : std_logic := '1' ) ; 
port ( clk : in std_logic ; 
rst : in std_logic ; 
we : in std_logic ; 
addr : in integer range 0 to 2 ** addr_width - 1 ; 
din : in integer ; 
dout : out integer 
) ; 
end component ; 


constant addr_width : integer := 8;
constant use_rising_edge : std_logic := '1';
constant use_sync_reset : std_logic := '1';
constant rst_level : std_logic := '1';
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal we : std_logic;
signal addr : integer range 0 to 2**addr_width-1;
signal din : integer;
signal dout : integer;

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
    we <= '0';
    addr <= 0;
    din <= 0;
    sync_wait_rising(clk, 10);
    rst <= not rst;
    sync_wait_rising(clk, 10);

    -- Write some data:
    for i in 0 to 50 loop 
        addr <= i;
        din <= i + 78;
        strobe_rising(clk, we);
    end loop;
    
    sync_wait_rising(clk, 20);
    
    -- Now read that data out:
    for i in 0 to 50 loop 
        addr <= i;
        sync_wait_rising(clk, 1);
    end loop;

    sync_wait_rising(clk, 100);
    sim_done <= '1';
    wait;
end process;




----------------------------------------------------------------------------------------------------
--                                        Unit Under Test
----------------------------------------------------------------------------------------------------
UUT: IntRam
    generic map (
        use_rising_edge => use_rising_edge,
        use_sync_reset => use_sync_reset,
        rst_level => rst_level
    )
    port map (
        clk => clk,
        rst => rst,
        we => we,
        addr => addr,
        din => din,
        dout => dout
    );


end architecture behavioral_IntRam_tb;


