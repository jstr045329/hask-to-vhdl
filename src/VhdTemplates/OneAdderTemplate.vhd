----------------------------------------------------------------------------------------------------
--                                 Adder With <bits_in_here> Bits In, <bits_out_here> Bits Out
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- The naming convention is AddTwoNumbers_<bits_in>_<bits_out>
-- <entity_declaration_start>
entity <entity_name_here> is
    generic(
        use_async_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        <reset_name_here> : in std_logic;
        a0 : in std_logic_vector(<bits_in_msb_here> downto 0);
        a1 : in std_logic_vector(<bits_in_msb_here> downto 0) := (others => '0');
        q : out std_logic_vector(<bits_out_msb_here> downto 0)
    );
end entity;
-- <entity_declaration_stop>

architecture behavioral_<entity_name_here> of <entity_name_here> is

begin

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, <reset_name_here>)
        
    begin
        <test_reset_here>
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
            <test_reset_here>
                q <= (others => '0');
            else
                q <= (others => '0');
                q <= std_logic_vector(unsigned('0' & a0) + unsigned('0' & a1));
            end if; 
        end if; 
    end process;
end generate;

end behavioral_<entity_name_here>;


