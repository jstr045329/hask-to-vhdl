library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;


package ArbSetPkg is

constant NUMBER_WIDTH : integer := 64;

component ArbSetLowLayerNode is 
    port(
        clk : in std_logic;
        rst : in std_logic;
        i_number_to_add : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_add_enable : in std_logic;
        i_number_to_remove : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_remove_enable : in std_logic;
        i_number_to_test : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_test_enable : in std_logic;
        i_predecessor_occupied : in std_logic;
        o_number_stored : out std_logic_vector(NUMBER_WIDTH-1 downto 0);
        o_occupied : out std_logic;
        o_membership : out std_logic 
   );
end component;


component ArbSetModule is 
    generic(
        num_nodes : integer := 16
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        i_number_to_add : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_add_enable : in std_logic;
        i_number_to_remove : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_remove_enable : in std_logic;
        i_number_to_test : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_test_enable : in std_logic;
        o_occupied : out std_logic;
        o_membership : out std_logic
    );
end component;


end package ArbSetPkg;


package body ArbSetPkg is


end package body ArbSetPkg;
