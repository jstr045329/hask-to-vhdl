------------------------------------------------------------------------------------------------------------------------
--                                                    One Set Bit 
--
-- This module represents 1 bit in 1 set. It aspires to have the smallest possible amount of logic to make operations 
-- on individual bits easy, while giving a higher level wrapper the access necessary to support operations between sets.
--
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.VhdSynthToolsPkg.ALL;
use work.PrefixGatesPkg.all;


entity OneSetBit is
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
        i_row : in integer range 0 to num_rows;
        i_col : in integer range 0 to num_cols;
        i_add : in std_logic;
        i_rem : in std_logic;

        -- Enable direct write from outside
        -- Supports operations between sets:
        i_din : in std_logic; 
        i_we : in std_logic; 
        o_member : out std_logic
    );
end OneSetBit;


architecture behavioral_OneSetBit of OneSetBit is

signal s_member : std_logic;

begin

RISING_CLOCK_GENERATE: if clk_edge = '1' generate 
    CLOCK_RISING_DRIVER: process(clk)
    begin
        if rising_edge(clk) then 
            if rst = rst_level then 
                s_member <= rst_value;
            else
                if b2SL_And(i_row = my_row, i_col = my_col) then 
                    if i_add = '1' then 
                        s_member <= '1';
                    elsif i_rem = '1' then 
                        s_member <= '0';
                    elsif i_we = '1' then 
                        s_member <= i_din;
                    end if;
                end if;
            end if;
        end if;
    end process;
end generate;

FALLING_CLOCK_GENERATE: if clk_edge = '0' generate
    CLOCK_FALLING_DRIVER: process(clk)
    begin
        if falling_edge(clk) then 
            if rst = rst_level then 
                s_member <= rst_value;
            else
                if b2SL_And(i_row = my_row, i_col = my_col) then 
                    if i_add = '1' then 
                        s_member <= '1';
                    elsif i_rem = '1' then 
                        s_member <= '0';
                    elsif i_we = '1' then 
                        s_member <= i_din;
                    end if;
                end if;
            end if;
        end if;
    end process;
end generate;


end behavioral_OneSetBit;

