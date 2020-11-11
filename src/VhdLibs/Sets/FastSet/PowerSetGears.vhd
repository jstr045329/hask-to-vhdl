library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;


package PowerSetGears is

component OneSetBit is
    generic (
        my_row : integer;
        my_col : integer;
        rst_level : std_logic := '1'; -- Level of global reset that resets the device.
        rst_value : std_logic := '0'; -- Value that s_member is assigned during reset.
        clk_edge : std_logic;
        num_rows : integer;
        num_cols : integer
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        
        -- Enable operations on individual bits:
        i_row : in integer;
        i_col : in integer;
        i_add : in std_logic;
        i_rem : in std_logic;

        -- Enable direct write from outside
        -- Supports operations between sets:
        i_din : in std_logic; 
        i_we : in std_logic; 
        o_member : out std_logic        
    );
end component;

end package PowerSetGears;


package body PowerSetGears is

end package body PowerSetGears;
