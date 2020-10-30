-- This module makes creative use of a 1-bit wide RAM to implement a set.
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.IntegerSetPkg.ALL;
use work.DijkstraPkg.ALL;


entity IntegerSet is
    generic (
        allow_fast_reset : std_logic := '0' -- Should only be set to 1 for sims. Synthesis should always have this set to 0.
    );
    port (
        clk : in std_logic;
        rst : in std_logic := '0'; -- unused
        i_din : in integer;
        i_include_number : in std_logic;
        i_exclude_number : in std_logic;
        i_check_for_membership : in std_logic;
        o_full_set : out t_integer_set;
        o_dout : out integer; -- Mostly useful when checking a number for membership. Also helpful if you want to see what the latency is.
        o_number_added : out std_logic;
        o_number_removed : out std_logic;
        o_number_is_member : out std_logic;
        o_out_valid : out std_logic;
        o_memory_changed : out std_logic
    );
end IntegerSet;


architecture behavioral_IntegerSet of IntegerSet is

signal s_my_set : t_integer_set;
signal s_dout : integer;
signal s_number_added : std_logic;
signal s_number_removed : std_logic;
signal s_number_is_member : std_logic;
signal s_out_valid : std_logic;
signal s_memory_changed : std_logic;

begin

MEMORY_DRIVER: process(clk) 
begin 
    if rising_edge(clk) then 
        if rst = '1' then 
            if allow_fast_reset = '1' then 
                -- Note: This statement is not supported in block RAM, which is why 
                -- allow_fast_reset should be set to 0 when testing a project intended 
                -- for synthesis. I only included this because you need to simulate 
                -- > 1 second of wall time to initialize a large RAM, which 
                -- can take hours in simulation. 
                s_my_set <= (others => '0');
            end if;
            s_number_added <= '0';
            s_number_removed <= '0';
            s_number_is_member <= '0';
            s_dout <= 0;
            s_out_valid <= '0';
            s_memory_changed <= '0';
        else 
            if include_number = '1' then 
                s_memory_changed <= not s_my_set(i_din);
                s_my_set(i_din) <= '1';
                s_number_added <= '1';
                s_number_removed <= '0';
                s_number_is_member <= '1';
                s_dout <= i_din;
                s_out_valid <= '1';
                
            elsif exclude_number = '1' then 
                s_memory_changed <= s_my_set(i_din);
                s_my_set(din) <= '0';
                s_number_added <= '0';
                s_number_removed <= '1';
                s_number_is_member <= '0';
                s_dout <= i_din;
                s_out_valid <= '1';
                
            elsif check_for_membership = '1' then 
                s_memory_changed <= '0';
                s_number_added <= '0';
                s_number_removed <= '0';
                s_number_is_member <= s_my_set(din);
                s_dout <= i_din;
                s_out_valid <= '1';

            else 
                s_memory_changed <= '0';
                s_number_added <= '0';
                s_number_removed <= '0';
                s_number_is_member <= '0';
                s_dout <= 0;
                s_out_valid <= '0';
            end if;
        end if;
    end if;
end process;

-- Drive Output:
o_dout <= s_dout;
o_number_added <= s_number_added;
o_number_removed <= s_number_removed;
o_number_is_member <= s_number_is_member;
o_memory_changed <= s_memory_changed;
o_out_valid <= s_out_valid;

end behavioral_IntegerSet;

