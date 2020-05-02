library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity my_ent_tb is
end my_ent_tb;


architecture behavioral_my_ent is


component my_ent is generic ( num_delays : integer := 16 ; 
w : integer := 8 ) ; 
port ( clk : in std_logic ; 
rst : in std_logic ; 
x : in std_logic_vector ( w - 1 downto 0 ) ; 
y : out std_logic_vector ( w - 1 downto 0 ) ) end component ; 


signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal x : std_logic;
signal y : std_logic;

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';


begin


process
begin
    if sim_done /= '1' then
        wait for clk_per/2;
        clk <= not clk;
    end if;
end process;


process
begin
    wait for clk_per*10;
    reset <= not reset;
    wait;
end process;




UUT
: 
my_ent
generic map (
    num_delays => num_delays,
    w => w
)
port map (
    clk => clk,
    rst => rst,
    x => x,
    y => y
);


end architecture behavioral_my_ent;


