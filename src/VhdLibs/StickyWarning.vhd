-- This module provides both a hard sticky and soft sticky output. 
-- Both outputs are asserted after trigger is asserted.
-- Soft sticky is asserted for approximately 2**(counter_width-1).
-- Hard sticky is asserted until a hardware reset. 
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity StickyWarning is
    generic (
        counter_width : integer := 8;
        clks_high : integer := 128
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic;
        trigger : in std_logic;
        soft_sticky : out std_logic;
        hard_sticky : out std_logic
    );
end StickyWarning;


architecture behavioral_StickyWarning of StickyWarning is

signal soft_sticky0 : std_logic;
signal hard_sticky0 : std_logic;
signal counter : unsigned(counter_width-1 downto 0);
constant counter_rollover : unsigned(counter_width-1 downto 0) := to_unsigned(clks_high-1, counter_width);
constant counter_start : unsigned(counter_width-1 downto 0) := (others => '1');
constant counter_reset : unsigned(counter_width-1 downto 0) := to_unsigned(2**counter_width-2, counter_width);
begin

BUSINESS_LOGIC: process(clk) 
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            hard_sticky0 <= '0';
            soft_sticky0 <= '0';
            counter <= counter_reset;
        else
            if clks_high > ((2 ** counter_width)-10) then 
                report "clks_high is getting dangerously close to a bad edge condition. Please reduce." severity warning;
            end if;

            if clks_high > ((2 ** counter_width)-3) then 
                report "clks_high is getting really, really close to a bad edge condition. Please reduce now." severity error;
            end if;

            if trigger = '1' then 
                hard_sticky0 <= '1';
                soft_sticky0 <= '1';
                counter <= counter_start;
            end if;

            if counter = counter_start then 
                soft_sticky0 <= '1';
                counter <= (others => '0');

            elsif counter < counter_rollover then 
                soft_sticky0 <= '1';
                counter <= counter + "1";

            elsif counter = counter_rollover then 
                soft_sticky0 <= '0';
                counter <= counter_reset;

            end if;
        end if;
    end if;
end process;

-- Drive outputs:
soft_sticky <= soft_sticky0;
hard_sticky <= hard_sticky0;

end behavioral_StickyWarning;
