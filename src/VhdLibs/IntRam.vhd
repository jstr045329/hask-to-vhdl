-- Models a single port RAM with single cycle lookup. 
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;

entity IntRam is
    generic(
        addr_width : integer := 8;
        use_rising_edge : std_logic := '1';
        use_sync_reset : std_logic := '1';
        rst_level : std_logic := '1' -- 0 for asserted low, 1 for asserted high
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        we : in std_logic;
        addr : in integer range 0 to 2**addr_width-1;
        din : in integer;
        dout : out integer
    );
end entity IntRam;

architecture behavioral_IntRam of IntRam is

constant num_rows : integer := 2**addr_width;
type t_memory is array(0 to num_rows-1) of integer;
signal memory : t_memory;

begin

USE_ASYNC_RESET_BLOCK: if use_sync_reset = '0' generate

    ------------------------------------------------------------------------------------------------
    --                                       Async Reset, Falling Edge
    ------------------------------------------------------------------------------------------------
    USE_RISING_EDGE_00: if use_rising_edge = '0' generate
        process(clk, rst)
        begin
            if rst = rst_level then
                memory <= (others => 0);
                dout <= 0;
            elsif falling_edge(clk) then
                if we = '1' then
                    memory(addr) <= din;
                end if;
                
                dout <= memory(addr);
            end if;
        end process;
    end generate;
    
    ------------------------------------------------------------------------------------------------
    --                                       Async Reset, Rising Edge
    ------------------------------------------------------------------------------------------------
    USE_RISING_EDGE_01: if use_rising_edge = '1' generate
        process(clk, rst)
        begin
            if rst = rst_level then
                memory <= (others => 0);
                dout <= 0;
            elsif rising_edge(clk) then
                if we = '1' then
                    memory(addr) <= din;
                end if;
                
                dout <= memory(addr);
            end if;
        end process;
    end generate;
end generate;

USE_SYNC_RESET_BLOCK: if use_sync_reset = '1' generate

    ------------------------------------------------------------------------------------------------
    --                                       Sync Reset, Falling Edge
    ------------------------------------------------------------------------------------------------
    USE_RISING_EDGE_10: if use_rising_edge = '0' generate
        process(clk)
        begin
            if falling_edge(clk) then
                if rst = rst_level then
                    memory <= (others => 0);
                    dout <= 0;
                else
                    if we = '1' then
                        memory(addr) <= din;
                    end if;
                    
                    dout <= memory(addr);
                end if;
            end if;
        end process;
    end generate;
    
    ------------------------------------------------------------------------------------------------
    --                                      Sync Reset, Rising Edge
    ------------------------------------------------------------------------------------------------
    USE_RISING_EDGE_11: if use_rising_edge = '1' generate
        process(clk)
        begin
            if rising_edge(clk) then
                if rst = rst_level then
                    memory <= (others => 0);
                    dout <= 0;
                else
                    if we = '1' then
                        memory(addr) <= din;
                    end if;
                    
                    dout <= memory(addr);
                end if;
            end if;
        end process;
    end generate;
end generate;
end architecture behavioral_IntRam;
