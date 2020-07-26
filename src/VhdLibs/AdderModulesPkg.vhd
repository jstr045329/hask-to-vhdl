----------------------------------------------------------------------------------------------------
--                                 Adder With 2 Bits In, 3 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0002_0003 is
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

architecture behavioral_AddTwoNumbers_0002_0003 of AddTwoNumbers_0002_0003 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0002_0003;


----------------------------------------------------------------------------------------------------
--                                 Adder With 3 Bits In, 4 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0003_0004 is
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

architecture behavioral_AddTwoNumbers_0003_0004 of AddTwoNumbers_0003_0004 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0003_0004;


----------------------------------------------------------------------------------------------------
--                                 Adder With 4 Bits In, 5 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0004_0005 is
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

architecture behavioral_AddTwoNumbers_0004_0005 of AddTwoNumbers_0004_0005 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0004_0005;


----------------------------------------------------------------------------------------------------
--                                 Adder With 5 Bits In, 6 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0005_0006 is
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

architecture behavioral_AddTwoNumbers_0005_0006 of AddTwoNumbers_0005_0006 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0005_0006;


----------------------------------------------------------------------------------------------------
--                                 Adder With 6 Bits In, 7 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0006_0007 is
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

architecture behavioral_AddTwoNumbers_0006_0007 of AddTwoNumbers_0006_0007 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0006_0007;


----------------------------------------------------------------------------------------------------
--                                 Adder With 7 Bits In, 8 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0007_0008 is
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

architecture behavioral_AddTwoNumbers_0007_0008 of AddTwoNumbers_0007_0008 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0007_0008;


----------------------------------------------------------------------------------------------------
--                                 Adder With 8 Bits In, 9 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0008_0009 is
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

architecture behavioral_AddTwoNumbers_0008_0009 of AddTwoNumbers_0008_0009 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0008_0009;


----------------------------------------------------------------------------------------------------
--                                 Adder With 9 Bits In, 10 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0009_0010 is
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

architecture behavioral_AddTwoNumbers_0009_0010 of AddTwoNumbers_0009_0010 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0009_0010;


----------------------------------------------------------------------------------------------------
--                                 Adder With 10 Bits In, 11 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0010_0011 is
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

architecture behavioral_AddTwoNumbers_0010_0011 of AddTwoNumbers_0010_0011 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0010_0011;


----------------------------------------------------------------------------------------------------
--                                 Adder With 11 Bits In, 12 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0011_0012 is
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

architecture behavioral_AddTwoNumbers_0011_0012 of AddTwoNumbers_0011_0012 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0011_0012;


----------------------------------------------------------------------------------------------------
--                                 Adder With 12 Bits In, 13 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0012_0013 is
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

architecture behavioral_AddTwoNumbers_0012_0013 of AddTwoNumbers_0012_0013 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0012_0013;


----------------------------------------------------------------------------------------------------
--                                 Adder With 13 Bits In, 14 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0013_0014 is
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

architecture behavioral_AddTwoNumbers_0013_0014 of AddTwoNumbers_0013_0014 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0013_0014;


----------------------------------------------------------------------------------------------------
--                                 Adder With 14 Bits In, 15 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0014_0015 is
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

architecture behavioral_AddTwoNumbers_0014_0015 of AddTwoNumbers_0014_0015 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0014_0015;


----------------------------------------------------------------------------------------------------
--                                 Adder With 15 Bits In, 16 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0015_0016 is
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

architecture behavioral_AddTwoNumbers_0015_0016 of AddTwoNumbers_0015_0016 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0015_0016;


----------------------------------------------------------------------------------------------------
--                                 Adder With 16 Bits In, 17 Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
entity AddTwoNumbers_0016_0017 is
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

architecture behavioral_AddTwoNumbers_0016_0017 of AddTwoNumbers_0016_0017 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset)
        
    begin
        if reset = '1' then
            q <= (others => '0');
        elsif rising_edge(clk) then
            q <= (others => '0');
            q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
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
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_AddTwoNumbers_0016_0017;


----------------------------------------------------------------------------------------------------
--                                    Adder Module Package
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package AdderModulePkg is
component AddTwoNumbers_0002_0003 is
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

component AddTwoNumbers_0003_0004 is
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

component AddTwoNumbers_0004_0005 is
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

component AddTwoNumbers_0005_0006 is
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

component AddTwoNumbers_0006_0007 is
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

component AddTwoNumbers_0007_0008 is
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

component AddTwoNumbers_0008_0009 is
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

component AddTwoNumbers_0009_0010 is
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

component AddTwoNumbers_0010_0011 is
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

component AddTwoNumbers_0011_0012 is
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

component AddTwoNumbers_0012_0013 is
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

component AddTwoNumbers_0013_0014 is
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

component AddTwoNumbers_0014_0015 is
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

component AddTwoNumbers_0015_0016 is
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

component AddTwoNumbers_0016_0017 is
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

