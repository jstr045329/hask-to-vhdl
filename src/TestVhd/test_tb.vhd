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


signal clk : std_logic
signal rst : std_logic
signal x : std_logic
signal y : std_logic
begin


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


