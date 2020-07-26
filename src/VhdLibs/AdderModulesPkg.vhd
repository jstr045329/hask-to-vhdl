----------------------------------------------------------------------------------------------------
--                                 Adder With 2 Bits In, 3 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0002_bits_in_0003_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(1 downto 0);
        a1 : in std_logic_vector(1 downto 0) := (others => '0');
        q : out std_logic_vector(2 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0002_bits_in_0003_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0002_bits_in_0003_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 3 Bits In, 4 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0003_bits_in_0004_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(2 downto 0);
        a1 : in std_logic_vector(2 downto 0) := (others => '0');
        q : out std_logic_vector(3 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0003_bits_in_0004_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0003_bits_in_0004_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 4 Bits In, 5 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0004_bits_in_0005_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(3 downto 0);
        a1 : in std_logic_vector(3 downto 0) := (others => '0');
        q : out std_logic_vector(4 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0004_bits_in_0005_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0004_bits_in_0005_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 5 Bits In, 6 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0005_bits_in_0006_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(4 downto 0);
        a1 : in std_logic_vector(4 downto 0) := (others => '0');
        q : out std_logic_vector(5 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0005_bits_in_0006_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0005_bits_in_0006_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 6 Bits In, 7 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0006_bits_in_0007_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(5 downto 0);
        a1 : in std_logic_vector(5 downto 0) := (others => '0');
        q : out std_logic_vector(6 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0006_bits_in_0007_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0006_bits_in_0007_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 7 Bits In, 8 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0007_bits_in_0008_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(6 downto 0);
        a1 : in std_logic_vector(6 downto 0) := (others => '0');
        q : out std_logic_vector(7 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0007_bits_in_0008_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0007_bits_in_0008_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 8 Bits In, 9 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0008_bits_in_0009_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(7 downto 0);
        a1 : in std_logic_vector(7 downto 0) := (others => '0');
        q : out std_logic_vector(8 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0008_bits_in_0009_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0008_bits_in_0009_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 9 Bits In, 10 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0009_bits_in_0010_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(8 downto 0);
        a1 : in std_logic_vector(8 downto 0) := (others => '0');
        q : out std_logic_vector(9 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0009_bits_in_0010_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0009_bits_in_0010_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 10 Bits In, 11 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0010_bits_in_0011_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(9 downto 0);
        a1 : in std_logic_vector(9 downto 0) := (others => '0');
        q : out std_logic_vector(10 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0010_bits_in_0011_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0010_bits_in_0011_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 11 Bits In, 12 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0011_bits_in_0012_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(10 downto 0);
        a1 : in std_logic_vector(10 downto 0) := (others => '0');
        q : out std_logic_vector(11 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0011_bits_in_0012_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0011_bits_in_0012_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 12 Bits In, 13 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0012_bits_in_0013_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(11 downto 0);
        a1 : in std_logic_vector(11 downto 0) := (others => '0');
        q : out std_logic_vector(12 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0012_bits_in_0013_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0012_bits_in_0013_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 13 Bits In, 14 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0013_bits_in_0014_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(12 downto 0);
        a1 : in std_logic_vector(12 downto 0) := (others => '0');
        q : out std_logic_vector(13 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0013_bits_in_0014_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0013_bits_in_0014_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 14 Bits In, 15 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0014_bits_in_0015_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(13 downto 0);
        a1 : in std_logic_vector(13 downto 0) := (others => '0');
        q : out std_logic_vector(14 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0014_bits_in_0015_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0014_bits_in_0015_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 15 Bits In, 16 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0015_bits_in_0016_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(14 downto 0);
        a1 : in std_logic_vector(14 downto 0) := (others => '0');
        q : out std_logic_vector(15 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0015_bits_in_0016_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0015_bits_in_0016_bits_out;


----------------------------------------------------------------------------------------------------
--                                 Adder With 16 Bits In, 17 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AddTwoNumbers_0016_bits_in_0017_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(15 downto 0);
        a1 : in std_logic_vector(15 downto 0) := (others => '0');
        q : out std_logic_vector(16 downto 0)
    );
end entity;

architecture behavioral_AddTwoNumbers_0016_bits_in_0017_bits_out is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned(a0) + unsigned(a1));
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
                q <= (others => '0');
                q <= std_logic_vector(unsigned(a0) + unsigned(a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0016_bits_in_0017_bits_out;


----------------------------------------------------------------------------------------------------
--                                    Adder Module Package
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package AdderModulePkg is
component AddTwoNumbers_0002_bits_in_0003_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(1 downto 0);
        a1 : in std_logic_vector(1 downto 0) := (others => '0');
        q : out std_logic_vector(2 downto 0)
    );
end component;

component AddTwoNumbers_0003_bits_in_0004_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(2 downto 0);
        a1 : in std_logic_vector(2 downto 0) := (others => '0');
        q : out std_logic_vector(3 downto 0)
    );
end component;

component AddTwoNumbers_0004_bits_in_0005_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(3 downto 0);
        a1 : in std_logic_vector(3 downto 0) := (others => '0');
        q : out std_logic_vector(4 downto 0)
    );
end component;

component AddTwoNumbers_0005_bits_in_0006_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(4 downto 0);
        a1 : in std_logic_vector(4 downto 0) := (others => '0');
        q : out std_logic_vector(5 downto 0)
    );
end component;

component AddTwoNumbers_0006_bits_in_0007_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(5 downto 0);
        a1 : in std_logic_vector(5 downto 0) := (others => '0');
        q : out std_logic_vector(6 downto 0)
    );
end component;

component AddTwoNumbers_0007_bits_in_0008_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(6 downto 0);
        a1 : in std_logic_vector(6 downto 0) := (others => '0');
        q : out std_logic_vector(7 downto 0)
    );
end component;

component AddTwoNumbers_0008_bits_in_0009_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(7 downto 0);
        a1 : in std_logic_vector(7 downto 0) := (others => '0');
        q : out std_logic_vector(8 downto 0)
    );
end component;

component AddTwoNumbers_0009_bits_in_0010_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(8 downto 0);
        a1 : in std_logic_vector(8 downto 0) := (others => '0');
        q : out std_logic_vector(9 downto 0)
    );
end component;

component AddTwoNumbers_0010_bits_in_0011_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(9 downto 0);
        a1 : in std_logic_vector(9 downto 0) := (others => '0');
        q : out std_logic_vector(10 downto 0)
    );
end component;

component AddTwoNumbers_0011_bits_in_0012_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(10 downto 0);
        a1 : in std_logic_vector(10 downto 0) := (others => '0');
        q : out std_logic_vector(11 downto 0)
    );
end component;

component AddTwoNumbers_0012_bits_in_0013_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(11 downto 0);
        a1 : in std_logic_vector(11 downto 0) := (others => '0');
        q : out std_logic_vector(12 downto 0)
    );
end component;

component AddTwoNumbers_0013_bits_in_0014_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(12 downto 0);
        a1 : in std_logic_vector(12 downto 0) := (others => '0');
        q : out std_logic_vector(13 downto 0)
    );
end component;

component AddTwoNumbers_0014_bits_in_0015_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(13 downto 0);
        a1 : in std_logic_vector(13 downto 0) := (others => '0');
        q : out std_logic_vector(14 downto 0)
    );
end component;

component AddTwoNumbers_0015_bits_in_0016_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(14 downto 0);
        a1 : in std_logic_vector(14 downto 0) := (others => '0');
        q : out std_logic_vector(15 downto 0)
    );
end component;

component AddTwoNumbers_0016_bits_in_0017_bits_out is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        reset : in std_logic;
        a0 : in std_logic_vector(15 downto 0);
        a1 : in std_logic_vector(15 downto 0) := (others => '0');
        q : out std_logic_vector(16 downto 0)
    );
end component;


end package AdderModulePkg;


package body AdderModulePkg is
end package body AdderModulePkg;

