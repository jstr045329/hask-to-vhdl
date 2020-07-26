-- ArcaneFlopsPkg .vhd
-- Contains flip flops that occasionally come in handy.
-- Note that the package containing component declarations is in this file, 
-- as are the entities. 


library IEEE;
use IEEE.std_logic_1164.all;

entity srff is
    generic(
        rst_val : std_logic := '0'
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        s : in std_logic;
        r : in std_logic;
        q : out std_logic
    );
end entity srff;

architecture arch_srff of srff is

begin

process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q <= rst_val;
        else
            if en = '1' then
                if s = '1' then
                    q <= '1';
                elsif r = '1' then
                    q <= '0';
                end if;
            end if;
        end if;
    end if;
end process;

end arch_srff;


library IEEE;
use IEEE.std_logic_1164.all;

entity tff is
    generic(
        rst_val : std_logic := '0'
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        load : in std_logic := '0';
        load_val : in std_logic := '0';
        t : in std_logic;
        q : out std_logic
    );
end entity tff;

architecture arch_tff of tff is
signal q0 : std_logic;
begin
q <= q0;
process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            q0 <= rst_val;
        else
            if en = '1' then
                if load = '1' then
                    q0 <= load_val;
                elsif t = '1' then
                    q0 <= not q0;
                end if;
            end if;
        end if;
    end if;
end process;

end arch_tff;


library IEEE;
use IEEE.std_logic_1164.all;

package ArcaneFlopsPkg is

component srff is
    generic(
        rst_val : std_logic := '0'
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        s : in std_logic;
        r : in std_logic;
        q : out std_logic
    );
end component srff;


component tff is
    generic(
        rst_val : std_logic := '0'
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        load : in std_logic := '0';
        load_val : in std_logic := '0';
        t : in std_logic;
        q : out std_logic
    );
end component tff;


end package ArcaneFlopsPkg ;


package body ArcaneFlopsPkg is
end package body ArcaneFlopsPkg ;

