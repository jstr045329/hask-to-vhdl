library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity BitsAreEqual is
    generic (
        use_async_reset : std_logic := '1'
        -- TODO: Test generate blocks on RegisteredGates before investing time in this one.
    );
    port (
        clk : in std_logic;
        <reset_name_here>
        a : in std_logic;
        b : in std_logic;
        q : out std_logic
    );
end BitsAreEqual;


architecture behavioral_BitsAreEqual of BitsAreEqual is
begin

process(clk, <reset_name_here>
begin
    <test_reset_here>
        q <= '0';
    elsif rising_edge(clk) then
        q <= not (a xor b);
    end if;
end process;


end behavioral_BitsAreEqual;

