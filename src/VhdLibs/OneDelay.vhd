library IEEE;
use IEEE.std_logic_1164.all;

----------------------------------------------------------------------------------------------------
--                                          Delay Std Logic
----------------------------------------------------------------------------------------------------
entity delayStdLogic is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );
end entity;

architecture arch_sl of delayStdLogic is
begin
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= '0';
        else
            if en = '1' then
                q <= d;
            end if;
        end if;
    end if;
end process;
end arch_sl;


----------------------------------------------------------------------------------------------------
--                                       Delay Std Logic Vector
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity delayStdLogicVector is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic_vector(w-1 downto 0);
        q : out std_logic_vector(w-1 downto 0)
    );
end entity;

architecture arch_slv of delayStdLogicVector is
begin
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= (others => '0');
        else
            if en = '1' then
                q <= d;
            end if;
        end if;
    end if;
end process;
end arch_slv;


----------------------------------------------------------------------------------------------------
--                                           Delay Signed
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delaySigned is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in signed(w-1 downto 0);
        q : out signed(w-1 downto 0)
    );
end entity;

architecture arch_signed of delaySigned is
begin
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= (others => '0');
        else
            if en = '1' then
                q <= d;
            end if;
        end if;
    end if;
end process;
end arch_signed;


----------------------------------------------------------------------------------------------------
--                                          Delay Unsigned
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayUnsigned is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in unsigned(w-1 downto 0);
        q : out unsigned(w-1 downto 0)
    );
end entity;

architecture arch_unsigned of delayUnsigned is
begin
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= (others => '0');
        else
            if en = '1' then
                q <= d;
            end if;
        end if;
    end if;
end process;
end arch_unsigned;


----------------------------------------------------------------------------------------------------
--                                            Delay Integer
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayInteger is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in integer;
        q : out integer
    );
end entity;

architecture arch_int of delayInteger is
begin
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= 0;
        else
            if en = '1' then
                q <= d;
            end if;
        end if;
    end if;
end process;
end arch_int;


