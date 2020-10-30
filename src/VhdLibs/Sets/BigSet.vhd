----------------------------------------------------------------------------------------------------
-- Author: Jim Strieter
-- Date: 9/14/2020
-- Client: University of Dayton Research Institute
--
-- This file uses a 1-bit wide RAM to implement a set.
-- A set is an unordered data structure with 3 operations:
--      1) Add element
--      2) Remove element
--      3) Check membership of an element
-- 
-- There are other mathematical operations that can be performed on sets, 
-- such as unions, intersections, etc. but that is beyond the scope of this file.
-- The reason is that most devices cannot infer block RAM and also support operations 
-- between columns in a RAM. For instance, most devices cannot do this:
--
--      column_3 <= column_1 or column_2; -- union between 2 sets
--      column_3 <= column_1 and column_2; -- intersection between 2 sets
-- 
-- in all 65536 rows of a 16 address bit RAM in 1 clock cycle.
-- 
-- That said, you can make smaller sets with say, 256 members, that do support those
-- operations.
--
-- This implementation has 1 write port and 2 read ports.
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity BigSet is 
    generic(
        addr_bits : integer := 8;
        allow_fast_reset : std_logic := '0'
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        i_din_1 : in unsigned(addr_bits-1 downto 0);
        i_din_2 : in unsigned(addr_bits-1 downto 0);
        i_add_1 : in std_logic;
        i_remove_1 : in std_logic;
        i_check_membership_1 : in std_logic;
        i_check_membership_2 : in std_logic;
        o_is_member_1 : out std_logic;
        o_out_valid_1 : out std_logic;
        o_is_member_2 : out std_logic;
        o_out_valid_2 : out std_logic
    );
end BigSet;

architecture behavioral_BigSet of BigSet is 

constant num_rows : integer := 2 ** addr_bits;
type t_my_set is array (0 to num_rows-1) of std_logic;
signal s_my_set : t_my_set;
signal s_address_int_1 : integer range 0 to num_rows-1;
signal s_address_int_2 : integer range 0 to num_rows-1;

begin 

s_address_int_1 <= to_integer(i_din_1);
s_address_int_2 <= to_integer(i_din_2);

process(clk)
begin
    if rising_edge(clk) then 
        -- Port 1 is both read and write capable:
        if 
        if i_add_1 = '1' then 
            s_my_set(s_address_int_1) <= '1';
            o_is_member_1 <= '0';
            out_valid <= '0';
        elsif i_remove_1 = '1' then 
            s_my_set(s_address_int_1) <= '0';
            o_is_member_1 <= '0';
            out_valid <= '0';
        elsif i_check_membership_1 <= '1' then 
            o_is_member_1 <= s_my_set(s_address_int_1);
            out_valid <= '1';
        end if;
        
        -- Port 2 is read only:
        if i_check_membership_2 = '1' then 
            o_is_member_2 <= s_my_set(s_address_int_2);
            o_out_valid_2 <= '1';
        else
            o_is_member_2 <= '0';
            o_out_valid_2 <= '0';
        end if;
        
    end if;
end process;

end behavioral_BigSet;
