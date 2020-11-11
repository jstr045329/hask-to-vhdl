------------------------------------------------------------------------------------------------------------------------
--                                               One Bottom Layer Node 
--
-- This module stores 1 number and tests that number for equality with a search input. 
-- W.r.t. i_predecessor_occupied, nodes form a linked list. This makes locating the next unused node trivial. 
-- (The alternative is messing around with trees, which is practical but more work.) 
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PrefixGatesPkg.all;
use work.VhdSynthToolsPkg.all;
use work.ArbSetPkg.all;


entity ArbSetLowLayerNode is
    port (
        clk : in std_logic;
        rst : in std_logic;
        i_number_to_add : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_add_enable : in std_logic; 
        i_number_to_remove : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_remove_enable : in std_logic;
        i_number_to_test : in std_logic_vector(NUMBER_WIDTH-1 downto 0); -- Test if some number is stored in this node.
        i_test_enable : in std_logic; -- Start the process of testing for equality
        i_predecessor_occupied : in std_logic;
        o_number_stored : out std_logic_vector(NUMBER_WIDTH-1 downto 0);
        o_occupied : out std_logic;
        o_membership : out std_logic
    );
end ArbSetLowLayerNode;


architecture behavioral_ArbSetLowLayerNode of ArbSetLowLayerNode is

signal s_number_stored : std_logic_vector(NUMBER_WIDTH-1 downto 0);
signal s_occupied : std_logic;
signal s_membership : std_logic;

------------------------------------------------------------------------------------------------------------------------
--                                                      Begin 
------------------------------------------------------------------------------------------------------------------------
begin

------------------------------------------------------------------------------------------------------------------------
--                                                  Business Logic 
------------------------------------------------------------------------------------------------------------------------
STORE_A_NUMBER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            s_number_stored <= (others => '0');
            s_occupied <= '0';
            s_membership <= '0';
        else
            -- Handle the case of adding a number:
            if b2SL_And(i_predecessor_occupied = '1', s_occupied = '0', i_add_enable = '1') then 
                s_number_stored <= i_number_to_add;
                s_occupied <= '1';
            end if;
            
            -- Handle the case of removing a number:
            if b2SL_And(s_occupied = '1', i_remove_enable = '1', s_number_stored = i_number_to_remove) then 
                s_number_stored <= (others => '0');
                s_occupied <= '0';
            end if;
            
            -- Test For Equality:
            s_membership <= s_occupied and i_test_enable and b2SL_And(s_number_stored = i_number_to_test);
        end if;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                   Drive Outputs 
------------------------------------------------------------------------------------------------------------------------
o_occupied <= s_occupied;
o_membership <= s_membership;
o_number_stored <= s_number_stored;

end behavioral_ArbSetLowLayerNode;

