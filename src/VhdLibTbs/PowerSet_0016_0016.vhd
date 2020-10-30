------------------------------------------------------------------------------------------------------------------------
--                     Power Set With 0016 Sets And 0016 Elements In Each Set 
--
-- This file is only for the purpose of generating a testbench.
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PowerSetGears.all;


entity PowerSet_0016_0016 is
    generic (
        rst_level : std_logic := '1'; -- Level of global reset that resets the device.
        rst_value : std_logic := '0'; -- Value that s_member is assigned during reset.
        clk_edge : std_logic := '1';
        allow_fast_reset : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        
        -- Enable operations on individual bits:
        i_bit_num_1 : in integer; -- Bit number = row number
        i_set_num_1 : in integer; -- Set number = column number
        i_add : in std_logic;
        i_rem : in std_logic;
        
        -- Enable operations between sets:
        i_set_num_2 : in integer;
        i_dest_num : in integer;
        i_union : in std_logic;
        i_intersection : in std_logic;
        
        -- The following operations only use i_set_num_1 and i_dest_num
        -- (i_set_num_2 is ignored):
        i_complement : in std_logic; 
        i_duplicate : in std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0000
        --------------------------------------------------------------------------------
        i_read_set_num_0000 : in integer;
        i_read_bit_num_0000 : in integer;
        i_read_enable_0000 : in integer;
        o_member_0000 : out std_logic;
        o_out_valid_0000 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0001
        --------------------------------------------------------------------------------
        i_read_set_num_0001 : in integer;
        i_read_bit_num_0001 : in integer;
        i_read_enable_0001 : in integer;
        o_member_0001 : out std_logic;
        o_out_valid_0001 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0002
        --------------------------------------------------------------------------------
        i_read_set_num_0002 : in integer;
        i_read_bit_num_0002 : in integer;
        i_read_enable_0002 : in integer;
        o_member_0002 : out std_logic;
        o_out_valid_0002 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0003
        --------------------------------------------------------------------------------
        i_read_set_num_0003 : in integer;
        i_read_bit_num_0003 : in integer;
        i_read_enable_0003 : in integer;
        o_member_0003 : out std_logic;
        o_out_valid_0003 : out std_logic;
        
        -- If both ports try to write to the same column at the same time, this warning goes hot. 
        -- Note that the potential for collisions is between i_set_num_1 and i_dest_num, not 
        -- i_set_num_1 and i_set_num_2.
        o_collision_warning : out std_logic 
    );
end PowerSet_0016_0016;
