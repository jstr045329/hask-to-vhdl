------------------------------------------------------------------------------------------------------------------------
--                                                Cross Clock Domains 
--
-- This module provides tools for crossing from 1 clock domain to another. Frequency 1 is the higher of the two. 
-- The clock_ratio generic is approximately ceil(f1/f2), where f1 is the higher of the two frequencies. Sometimes 
-- clock_ratio needs to be increased a bit in order to ensure reliable behavior. It is recommended that you use one 
-- of the functions in ClockCrossingPkg.vhd to define clock_ratio, as they handle the details for you. This entity does 
-- not care what units the frequencies are in, so long as they have the same units. For instance, make them both Hz or 
-- both MHz; but don't mix & match. The functions that calculate the number of delays 
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.VhdSynthToolsPkg.all;
use work.ClockCrossingPkg.all;


entity ClockCrossingKernel is
    generic (
        data_width : positive := 8;
        frequency_1 : real := 200.0;
        frequency_2 : real := 100.0
    );
    port (
        clk_f1 : in std_logic;
        clk_f2 : in std_logic;
        rst : in std_logic;
        i_f1_din : in std_logic_vector(data_width-1 downto 0);
        i_f1_we : in std_logic;
        o_f1_ok_to_wr : out std_logic;
        
        -- The following 2 outputs can be helpful for simulation.
        -- They are not synthesizable, so leave them open during synthesis.
        o_raw_clock_ratio : out real;
        
        -- ceiling(o_raw_clock_ratio) + fudge_factor, where fudge_factor >= 0 and typically does not get very large.
        o_num_circuit_copies : out positive 
    );
end ClockCrossingKernel;


architecture behavioral_ClockCrossingKernel of ClockCrossingKernel is

constant NUM_DELAYS_F1_TO_F2 : positive := calculateNumDelays(frequency_1, frequency_2);
type t_f1_to_f2_data_delay_line is array(0 to NUM_DELAYS_F1_TO_F2-1) of std_logic_vector(data_width-1 downto 0);
signal s_f1_to_f2_data_delay_line : t_f1_to_f2_data_delay_line;
type t_f1_to_f2_we_delay_line is array(0 to NUM_DELAYS_F1_TO_F2-1) of std_logic;
signal s_f1_to_f2_we_delay_line : t_f1_to_f2_we_delay_line;
signal s_f1_ok_to_wr : std_logic;

begin 

------------------------------------------------------------------------------------------------------------------------
--                                Transfer Data From Low Frequency to High Frequency 
------------------------------------------------------------------------------------------------------------------------
F1_TRANSMIT_DRIVER: process(clk_f1)
    variable v_new_data : std_logic_vector(data_width-1 downto 0);
begin
    if rising_edge(clk_f1) then 
        if rst = '1' then 
            s_f1_to_f2_data_delay_line <= (others => (others => '0'));
            s_f1_to_f2_we_delay_line <= (others => '0');
        else
            s_f1_to_f2_we_delay_line <= i_f1_we & s_f1_to_f2_we_delay_line(0 to NUM_DELAYS_F1_TO_F2-2);
            if i_f1_we = '1' then 
                v_new_data := i_f1_din;
            else
                v_new_data := (others => '0');
            end if;
            s_f1_to_f2_data_delay_line <= v_new_data & s_f1_to_f2_data_delay_line(0 to NUM_DELAYS_F1_TO_F2-2);
            
        end if;
    end if;
end process;


F1_OK_TO_WRITE_DRIVER: process(clk_f1)
    variable tmp : std_logic;
begin
    if rising_edge(clk_f1) then 
        if rst_g = '1' then 
            s_f1_ok_to_wr <= '0';
        else
            -- Note: PICK UP HERE
        end if;
    end if;
end process;


F2_RECEIVE_DRIVER: process(clk_f2)
begin
    if rising_edge(clk_f2) then 
        if rst = '1' then 

        else

        end if;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                Transfer Data From High Frequency To Low Frequency 
------------------------------------------------------------------------------------------------------------------------

F1_RECEIVE_DRIVER: process(clk_f1)
begin
    if rising_edge(clk_f1) then 
        if rst = '1' then 

        else

        end if;
    end if;
end process;


F2_TRANSMIT_DRIVER: process(clk_f2)
begin
    if rising_edge(clk_f2) then 
        if rst = '1' then 

        else

        end if;
    end if;
end process;

-- o_num_circuit_copies <= 

end behavioral_ClockCrossingKernel;

