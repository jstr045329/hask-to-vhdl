library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


package VhdSimToolsPkg is

----------------------------------------------------------------------------------------------------
--                                    Clock & Reset Driver
----------------------------------------------------------------------------------------------------
component clk_rst_tb is
    generic(period : time := 12 ns);
    port(
        clk0 : out std_logic;
        clk1 : out std_logic;
        clk2 : out std_logic;
        clk3 : out std_logic;
        rst_p : out std_logic;
        rst_n : out std_logic;
        sim_done : in std_logic := '0';
        clk_count : out integer
    );
end component;


----------------------------------------------------------------------------------------------------
--                                      Syncronous Delay
----------------------------------------------------------------------------------------------------
-- These procedures allow you to wait a specified number of clks
-- without the next event being off by 1 ps.
procedure sync_wait_rising(
    signal clk : in std_logic;
    constant n : in integer
    );

procedure sync_wait_falling(
    signal clk : in std_logic;
    constant n : in integer
    );


-- These procedures allow you to strobe an enable signal (or anything
-- else you might want) for 1 clk. They do not care whether signal
-- is asserted high or asserted low. Technically you could use them 
-- to de-assert a signal for 1 clk so long as the signal is asserted
-- before calling the procedure.
procedure strobe_rising(
    signal clk : in std_logic;
    signal en : inout std_logic
    );

procedure strobe_falling(
    signal clk : in std_logic;
    signal en : inout std_logic
    );

-- Determine if a signal is a 0 or 1 (i.e. not U, X, etc.)
function demand_0_or_1(
    x : std_logic
    ) return std_logic;

-- Throw an error if a signal is not 0 or 1. 
procedure freak_out(
    signal x : in unsigned
    );

end package VhdSimToolsPkg ;


package body VhdSimToolsPkg is

----------------------------------------------------------------------------------------------------
--                                         Syncronous Delay
----------------------------------------------------------------------------------------------------

-- Ever spend 4 hours ripping your hair out only to find that some signal
-- is changing state 1 ps later than the clock edge? 
--
-- That will not happen with this. 
--
-- You're welcome. 
procedure sync_wait_rising(
    signal clk : in std_logic;
    constant n : in integer
    ) is
begin
    for i in 0 to n-1 loop
        wait until rising_edge(clk);
    end loop;
end sync_wait_rising;


procedure sync_wait_falling(
    signal clk : in std_logic;
    constant n : in integer
    ) is
begin
    for i in 0 to n-1 loop
        wait until falling_edge(clk);
    end loop;
end sync_wait_falling;


-- Waits until a clock edge, then reverses en.
-- Repeats once.
-- The effect this has is by asserting en for 1 clock, 
-- without caring whether en is asserted hi or asserted lo.
-- What DOES matter, however, is that en is not asserted before this procedure
-- is called, which is not normally a difficult assumption to meet. 
procedure strobe_rising(
    signal clk : in std_logic;
    signal en : inout std_logic
    ) is
begin
    en <= not en;
    wait until rising_edge(clk);
    en <= not en;
    wait until rising_edge(clk);
end strobe_rising;


procedure strobe_falling(
    signal clk : in std_logic;
    signal en : inout std_logic
    ) is
begin
    en <= not en;
    wait until falling_edge(clk);
    en <= not en;
    wait until falling_edge(clk);
end strobe_falling;


function demand_0_or_1(
    x : std_logic
    ) return std_logic is
begin
    if x = '0' then 
        return '1';
    elsif x = '1' then 
        return '1';
    else
        return '0';
    end if;
end function;

-- Throw an error if a signal is not 0 or 1. 
procedure freak_out(
    signal x : in unsigned
    ) is
begin
    for i in 0 to x'left loop
        if demand_0_or_1(x(i)) = '0' then 
            report "bad value" severity error;
        end if;
    end loop;
end procedure;

end package body VhdSimToolsPkg ;


