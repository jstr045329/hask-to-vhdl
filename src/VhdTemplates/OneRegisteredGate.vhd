library ieee;
use ieee.std_logic_1164.all;


entity <entity_name_here> is
    generic (
        use_async_reset : std_logic := '1'
    );
    port (
        clk : in std_logic;
        <reset_name_here>;
        <input_0_here>;
        <input_1_here>;
        <input_2_here>;
        <input_3_here>;
        <input_4_here>;
        <input_5_here>;
        <input_6_here>;
        q : out std_logic
    );
end <entity_name_here>;


architecture behavioral_<entity_name_here> of <entity_name_here> is

USE_ASYNC_RESET_BLOCK: if use_async_reset = '1' generate 
    process(clk, reset_n)
    begin
        <test_reset_here>
            q <= '0';
        elsif rising_edge(clk) then
            q <= <insert_logic_here>;
        end if;
    end process;
end generate;

USE_SYNC_RESET_BLOCK: if use_async_reset = '0' generate
    process(clk, reset_n)
    begin
        if rising_edge(clk) then
            <test_reset_here>
                q <= '0';
            else
                q <= <insert_logic_here>;
            end if;
        end if;
    end process;
end generate;


end behavioral_<entity_name_here>;
        
