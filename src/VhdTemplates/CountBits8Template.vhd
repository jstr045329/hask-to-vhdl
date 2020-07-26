library ieee;
use ieee.std_logic_1164.all;


entity count1s is
    generic (
        use_async_reset : std_logic := '1'
    );
    port (
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        a5 : in std_logic := '0';
        a6 : in std_logic := '0';
        a7 : in std_logic := '0';
        q : out std_logic_vector(3 downto 0)
    );
end count1s;


architecture behavioral_count1s of count1s is

-- Following should have same number of bits as this module has inputs:
signal agg : std_logic_vector(7 downto 0);

<insert_logic_here>

begin

agg <= a0 & a1 & a2 & a3 & a4 & a5 & a6 & a7;

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= count_bits(agg);
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                q <= (others => '0');
            else
                q <= count_bits(agg);
            end if;
        end if;
    end process;
end generate;

end behavioral_count1s;


