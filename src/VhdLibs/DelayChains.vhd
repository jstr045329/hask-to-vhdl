----------------------------------------------------------------------------------------------------
--                                  Delay Chain for Std Logic
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity delayChainStdLogic is 
    generic(
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );
end delayChainStdLogic;

architecture behavioral_sl of delayChainStdLogic is
    type t_my_chain is array(0 to n-1) of std_logic;
    signal my_chain : t_my_chain;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            my_chain <= (others => '0');
        else
            if en = '1' then
                my_chain <= d & my_chain(0 to n-2);
            end if;
        end if;
    end if;
end process;

q <= my_chain(n-1);

end behavioral_sl;            


----------------------------------------------------------------------------------------------------
--                                Delay Chain for Std Logic Vector
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayChainStdLogicVector is 
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic_vector(w-1 downto 0);
        q : out std_logic_vector(w-1 downto 0)
    );
end delayChainStdLogicVector;

architecture behavioral_slv of delayChainStdLogicVector is
    type t_my_chain is array(0 to n-1) of std_logic_vector(w-1 downto 0);
    signal my_chain : t_my_chain;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            my_chain <= (others => (others => '0'));
        else
            if en = '1' then
                my_chain <= d & my_chain(0 to n-2);
            end if;
        end if;
    end if;
end process;

q <= my_chain(n-1);

end behavioral_slv;            


----------------------------------------------------------------------------------------------------
--                                Delay Chain for Signed
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayChainSigned is 
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in signed(w-1 downto 0);
        q : out signed(w-1 downto 0)
    );
end delayChainSigned;

architecture behavioral_signed of delayChainSigned is
    type t_my_chain is array(0 to n-1) of signed(w-1 downto 0);
    signal my_chain : t_my_chain;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            my_chain <= (others => (others => '0'));
        else
            if en = '1' then
                my_chain <= d & my_chain(0 to n-2);
            end if;
        end if;
    end if;
end process;

q <= my_chain(n-1);

end behavioral_signed;
 

----------------------------------------------------------------------------------------------------
--                                  Delay Chain for Unsigned
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayChainUnsigned is 
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in unsigned(w-1 downto 0);
        q : out unsigned(w-1 downto 0)
    );
end delayChainUnsigned;

architecture behavioral_unsigned of delayChainUnsigned is
    type t_my_chain is array(0 to n-1) of unsigned(w-1 downto 0);
    signal my_chain : t_my_chain;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            my_chain <= (others => (others => '0'));
        else
            if en = '1' then
                my_chain <= d & my_chain(0 to n-2);
            end if;
        end if;
    end if;
end process;

q <= my_chain(n-1);

end behavioral_unsigned;


----------------------------------------------------------------------------------------------------
--                                Delay Chain for Integer
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity delayChainInteger is 
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in integer;
        q : out integer
    );
end delayChainInteger;

architecture behavioral_int of delayChainInteger is
    type t_my_chain is array(0 to n-1) of integer;
    signal my_chain : t_my_chain;
begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            my_chain <= (others => 0);
        else
            if en = '1' then
                my_chain <= d & my_chain(0 to n-2);
            end if;
        end if;
    end if;
end process;

q <= my_chain(n-1);

end behavioral_int;


