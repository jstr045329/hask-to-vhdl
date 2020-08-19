----------------------------------------------------------------------------------------------------
--                                      Testbench for OneRam
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;


entity OneRam_tb is
end OneRam_tb;


architecture behavioral_OneRam_tb of OneRam_tb is


component OneRam is generic ( data_width : integer := 8 ; 
addr_width : integer := 8 ; 
use_rising_edge : std_logic := '1' ; 
use_sync_reset : std_logic := '1' ; 
rst_level : std_logic := '1' ) ; 
port ( clk : in std_logic ; 
rst : in std_logic ; 
we : in std_logic ; 
addr : in std_logic_vector ( addr_width - 1 downto 0 ) ; 
din : in std_logic_vector ( data_width - 1 downto 0 ) ; 
dout : out std_logic_vector ( data_width - 1 downto 0 ) ) ; 
end component ; 


constant data_width : integer := 8;
constant addr_width : integer := 8;
constant use_rising_edge : std_logic := '1';
constant use_sync_reset : std_logic := '1';
constant rst_level : std_logic := '1';
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal we : std_logic;
signal addr : std_logic_vector(addr_width-1 downto 0);
signal din : std_logic_vector(data_width-1 downto 0);
signal dout : std_logic_vector(data_width-1 downto 0);

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';


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
    addr <= (others => '0');
    din <= (others => '0');
    --wait for clk_per*10;
    sync_wait_rising(clk, 10);
    rst <= not rst;
    sync_wait_rising(clk, 10);
    
    addr <= x"01";
    din <= x"37";
    strobe_rising(clk, we);
    sync_wait_rising(clk, 1);
    
    addr <= x"02";
    din <= x"38";
    strobe_rising(clk, we);
    sync_wait_rising(clk, 1);
    
    addr <= x"42";
    din <= x"ff";
    strobe_rising(clk, we);
    sync_wait_rising(clk, 1);
    
    addr <= (others => '0');
    sync_wait_rising(clk, 10);
    
    addr <= x"01";
    sync_wait_rising(clk, 1);
    
    addr <= x"02";
    sync_wait_rising(clk, 1);
    
    addr <= x"42";
    sync_wait_rising(clk, 1);
    
    sync_wait_rising(clk, 100);
    sim_done <= '1';
    wait;
end process;


----------------------------------------------------------------------------------------------------
--                                        Unit Under Test
----------------------------------------------------------------------------------------------------
UUT
: 
OneRam
generic map (
    data_width => data_width,
    addr_width => addr_width,
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


end architecture behavioral_OneRam_tb;


