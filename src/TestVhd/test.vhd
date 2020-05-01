library IEEE;
use IEEE.std_logic_1164.ALL;

entity my_ent is
    generic(
        num_delays:integer := 16;
        w : integer := 8
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        x:in std_logic_vector(w-1 downto 0);
        y:out std_logic_vector(w-1 downto 0)
    );
end my_ent;


architecture behavioral of my_ent is

signal x0 : std_logic;
signal x1 : std_logic_vector(w-1 downto 0);
signal x2 : std_logic;
signal u1 : unsigned(w-1 downto 0);

begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            y <= (others => '0');
        else
            y <= x;
        end if;
    end if;
end process;


process(clk, rst)
begin
    if rst = '1' then
        x0 <= '0';
        x1 <= (others => '0');
    elsif rising_edge(clk) then
        x0 <= '1';
        x1 <= '1' & x1(w-1 downto 1);
    end if;
end process;


ramp_generator: process(clk, rst)
begin
    if rst = '1' then
        u1 <= (others => '0');
    elsif rising_edge(clk) then
        if u1 < x"fe" then
            u1 <= x"01" + u1;
        else
            u1 <= (others => '0');
        end if;
    end if;
end process;


end behavioral;


