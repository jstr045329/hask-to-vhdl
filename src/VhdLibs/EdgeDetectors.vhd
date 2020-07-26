----------------------------------------------------------------------------------------------------
--                                      Rising Edge Detector
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity detectRisingEdge is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );
end detectRisingEdge;

architecture arch_rising_edge of detectRisingEdge is
    signal d0 : std_logic;
    signal q0 : std_logic;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            d0 <= '0';
            q0 <= '0';
        else
            if en = '1' then
                if d = '1' and d0 = '0' then
                    q0 <= '1';
                else
                    q0 <= '0';
                end if;

                d0 <= d; 
            end if;
        end if;
    end if;
end process;
q <= q0;
end arch_rising_edge;


----------------------------------------------------------------------------------------------------
--                                      Falling Edge Detector
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity detectFallingEdge is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );
end detectFallingEdge;

architecture arch_falling_edge of detectFallingEdge is
    signal d0 : std_logic;
    signal q0 : std_logic;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            d0 <= '0';
            q0 <= '0';
        else
            if en = '1' then
                if d = '0' and d0 = '1' then
                    q0 <= '1';
                else
                    q0 <= '0';
                end if;

                d0 <= d; 
            end if;
        end if;
    end if;
end process;
q <= q0;
end arch_falling_edge;

