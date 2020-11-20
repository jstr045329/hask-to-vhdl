library ieee;
use ieee.std_logic_1164.all;


entity registeredAND1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND1;


architecture behavioral_registeredAND1 of registeredAND1 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND1;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND2;


architecture behavioral_registeredAND2 of registeredAND2 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND2;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND3;


architecture behavioral_registeredAND3 of registeredAND3 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1 and a2;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1 and a2;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND3;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND4;


architecture behavioral_registeredAND4 of registeredAND4 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1 and a2 and a3;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1 and a2 and a3;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND4;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND5;


architecture behavioral_registeredAND5 of registeredAND5 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1 and a2 and a3 and a4;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1 and a2 and a3 and a4;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND5;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        a5 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND6;


architecture behavioral_registeredAND6 of registeredAND6 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1 and a2 and a3 and a4 and a5;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1 and a2 and a3 and a4 and a5;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND6;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredAND7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        a5 : in std_logic := '0';
        a6 : in std_logic := '0';
        q : out std_logic
    );
end registeredAND7;


architecture behavioral_registeredAND7 of registeredAND7 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 and a1 and a2 and a3 and a4 and a5 and a6;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 and a1 and a2 and a3 and a4 and a5 and a6;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredAND7;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR1;


architecture behavioral_registeredOR1 of registeredOR1 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR1;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR2;


architecture behavioral_registeredOR2 of registeredOR2 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR2;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR3;


architecture behavioral_registeredOR3 of registeredOR3 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1 or a2;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1 or a2;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR3;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR4;


architecture behavioral_registeredOR4 of registeredOR4 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1 or a2 or a3;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1 or a2 or a3;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR4;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR5;


architecture behavioral_registeredOR5 of registeredOR5 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1 or a2 or a3 or a4;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1 or a2 or a3 or a4;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR5;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        a5 : in std_logic := '0';
        q : out std_logic
    );
end registeredOR6;


architecture behavioral_registeredOR6 of registeredOR6 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1 or a2 or a3 or a4 or a5;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1 or a2 or a3 or a4 or a5;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR6;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end registeredOR7;


architecture behavioral_registeredOR7 of registeredOR7 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 or a1 or a2 or a3 or a4 or a5 or a6;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 or a1 or a2 or a3 or a4 or a5 or a6;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredOR7;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end registeredNAND1;


architecture behavioral_registeredNAND1 of registeredNAND1 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND1;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end registeredNAND2;


architecture behavioral_registeredNAND2 of registeredNAND2 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 nand a1;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 nand a1;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND2;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end registeredNAND3;


architecture behavioral_registeredNAND3 of registeredNAND3 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nand (a1 nand a2));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nand (a1 nand a2));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND3;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end registeredNAND4;


architecture behavioral_registeredNAND4 of registeredNAND4 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nand (a1 nand (a2 nand a3)));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nand (a1 nand (a2 nand a3)));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND4;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end registeredNAND5;


architecture behavioral_registeredNAND5 of registeredNAND5 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nand (a1 nand (a2 nand (a3 nand a4))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nand (a1 nand (a2 nand (a3 nand a4))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND5;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end registeredNAND6;


architecture behavioral_registeredNAND6 of registeredNAND6 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nand (a1 nand (a2 nand (a3 nand (a4 nand a5)))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nand (a1 nand (a2 nand (a3 nand (a4 nand a5)))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND6;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNAND7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end registeredNAND7;


architecture behavioral_registeredNAND7 of registeredNAND7 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nand (a1 nand (a2 nand (a3 nand (a4 nand (a5 nand a6))))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nand (a1 nand (a2 nand (a3 nand (a4 nand (a5 nand a6))))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNAND7;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end registeredNOR1;


architecture behavioral_registeredNOR1 of registeredNOR1 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR1;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end registeredNOR2;


architecture behavioral_registeredNOR2 of registeredNOR2 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 nor a1;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 nor a1;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR2;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end registeredNOR3;


architecture behavioral_registeredNOR3 of registeredNOR3 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nor (a1 nor a2));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nor (a1 nor a2));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR3;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end registeredNOR4;


architecture behavioral_registeredNOR4 of registeredNOR4 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nor (a1 nor (a2 nor a3)));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nor (a1 nor (a2 nor a3)));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR4;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end registeredNOR5;


architecture behavioral_registeredNOR5 of registeredNOR5 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nor (a1 nor (a2 nor (a3 nor a4))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nor (a1 nor (a2 nor (a3 nor a4))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR5;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end registeredNOR6;


architecture behavioral_registeredNOR6 of registeredNOR6 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nor (a1 nor (a2 nor (a3 nor (a4 nor a5)))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nor (a1 nor (a2 nor (a3 nor (a4 nor a5)))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR6;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredNOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end registeredNOR7;


architecture behavioral_registeredNOR7 of registeredNOR7 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= (a0 nor (a1 nor (a2 nor (a3 nor (a4 nor (a5 nor a6))))));
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= (a0 nor (a1 nor (a2 nor (a3 nor (a4 nor (a5 nor a6))))));
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredNOR7;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end registeredXOR1;


architecture behavioral_registeredXOR1 of registeredXOR1 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR1;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end registeredXOR2;


architecture behavioral_registeredXOR2 of registeredXOR2 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR2;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end registeredXOR3;


architecture behavioral_registeredXOR3 of registeredXOR3 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1 xor a2;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1 xor a2;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR3;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end registeredXOR4;


architecture behavioral_registeredXOR4 of registeredXOR4 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1 xor a2 xor a3;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1 xor a2 xor a3;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR4;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end registeredXOR5;


architecture behavioral_registeredXOR5 of registeredXOR5 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1 xor a2 xor a3 xor a4;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1 xor a2 xor a3 xor a4;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR5;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end registeredXOR6;


architecture behavioral_registeredXOR6 of registeredXOR6 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1 xor a2 xor a3 xor a4 xor a5;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1 xor a2 xor a3 xor a4 xor a5;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR6;
        
library ieee;
use ieee.std_logic_1164.all;


entity registeredXOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end registeredXOR7;


architecture behavioral_registeredXOR7 of registeredXOR7 is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, rst)
    begin
        if rst = rst_level then
            q <= '0';
        elsif rising_edge(clk) then
            q <= a0 xor a1 xor a2 xor a3 xor a4 xor a5 xor a6;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = rst_level then
                q <= '0';
            else
                q <= a0 xor a1 xor a2 xor a3 xor a4 xor a5 xor a6;
            end if;
        end if;
    end process;
end generate;


end behavioral_registeredXOR7;
        

library ieee;
use ieee.std_logic_1164.all;

package RegisteredGatesPkg is 


component registeredAND1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end component registeredAND1;


component registeredAND2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end component registeredAND2;


component registeredAND3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end component registeredAND3;


component registeredAND4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end component registeredAND4;


component registeredAND5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end component registeredAND5;


component registeredAND6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end component registeredAND6;


component registeredAND7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end component registeredAND7;


component registeredOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end component registeredOR1;


component registeredOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end component registeredOR2;


component registeredOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end component registeredOR3;


component registeredOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end component registeredOR4;


component registeredOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end component registeredOR5;


component registeredOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic := '0';
        a1 : in std_logic := '0';
        a2 : in std_logic := '0';
        a3 : in std_logic := '0';
        a4 : in std_logic := '0';
        a5 : in std_logic := '0';
        q : out std_logic
    );
end component registeredOR6;


component registeredOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end component registeredOR7;


component registeredNAND1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end component registeredNAND1;


component registeredNAND2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end component registeredNAND2;


component registeredNAND3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end component registeredNAND3;


component registeredNAND4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end component registeredNAND4;


component registeredNAND5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end component registeredNAND5;


component registeredNAND6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end component registeredNAND6;


component registeredNAND7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end component registeredNAND7;


component registeredNOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end component registeredNOR1;


component registeredNOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end component registeredNOR2;


component registeredNOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end component registeredNOR3;


component registeredNOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end component registeredNOR4;


component registeredNOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end component registeredNOR5;


component registeredNOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end component registeredNOR6;


component registeredNOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end component registeredNOR7;


component registeredXOR1 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        q : out std_logic
    );
end component registeredXOR1;


component registeredXOR2 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        q : out std_logic
    );
end component registeredXOR2;


component registeredXOR3 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        q : out std_logic
    );
end component registeredXOR3;


component registeredXOR4 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        q : out std_logic
    );
end component registeredXOR4;


component registeredXOR5 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        q : out std_logic
    );
end component registeredXOR5;


component registeredXOR6 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        q : out std_logic
    );
end component registeredXOR6;


component registeredXOR7 is
    generic (
        use_async_reset : std_logic := '0';
        rst_level : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        a0 : in std_logic;
        a1 : in std_logic;
        a2 : in std_logic;
        a3 : in std_logic;
        a4 : in std_logic;
        a5 : in std_logic;
        a6 : in std_logic;
        q : out std_logic
    );
end component registeredXOR7;


end package RegisteredGatesPkg;

package body RegisteredGatesPkg is 
end package body RegisteredGatesPkg;
