library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;


entity MinFunction_0016 is
    generic (
        -- Only use i_distance_xx when an input is smaller than this:
        max_integer : integer := (2**30)-2
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        i_soft_rst : in std_logic;
        i_distance_0000 : in integer := max_integer;
        i_distance_0001 : in integer := max_integer;
        i_distance_0002 : in integer := max_integer;
        i_distance_0003 : in integer := max_integer;
        i_distance_0004 : in integer := max_integer;
        i_distance_0005 : in integer := max_integer;
        i_distance_0006 : in integer := max_integer;
        i_distance_0007 : in integer := max_integer;
        i_distance_0008 : in integer := max_integer;
        i_distance_0009 : in integer := max_integer;
        i_distance_0010 : in integer := max_integer;
        i_distance_0011 : in integer := max_integer;
        i_distance_0012 : in integer := max_integer;
        i_distance_0013 : in integer := max_integer;
        i_distance_0014 : in integer := max_integer;
        i_distance_0015 : in integer := max_integer;
        o_min : out integer;
        o_latency : out integer
    );
end MinFunction_0016;


architecture behavioral_MinFunction_0016 of MinFunction_0016 is

signal s_smallest_intermediate_0000 : integer;
signal s_smallest_intermediate_0001 : integer;
signal s_smallest_intermediate_0002 : integer;
signal s_smallest_intermediate_0003 : integer;
signal s_smallest_intermediate_0004 : integer;

begin

process(clk)
    variable v_smallest_intermediate_0000 : integer;
    variable v_smallest_intermediate_0001 : integer;
    variable v_smallest_intermediate_0002 : integer;
    variable v_smallest_intermediate_0003 : integer;
    variable v_smallest_intermediate_0004 : integer;
begin
    if rising_edge(clk) then 
        if (i_soft_rst or rst) = '1' then 
            s_smallest_intermediate_0001 <= max_integer;
            s_smallest_intermediate_0002 <= max_integer;
            s_smallest_intermediate_0003 <= max_integer;
            s_smallest_intermediate_0004 <= max_integer;
            
        else
            
            v_smallest_intermediate_0000 := max_integer;
            v_smallest_intermediate_0001 := max_integer;
            v_smallest_intermediate_0002 := max_integer;
            v_smallest_intermediate_0003 := max_integer;
            v_smallest_intermediate_0004 := max_integer;
            if i_distance_0000 < v_smallest_intermediate_0000 then
                v_smallest_intermediate_0000 := i_distance_0000;
            end if;
            
            if i_distance_0001 < v_smallest_intermediate_0000 then
                v_smallest_intermediate_0000 := i_distance_0001;
            end if;
            
            if i_distance_0002 < v_smallest_intermediate_0000 then
                v_smallest_intermediate_0000 := i_distance_0002;
            end if;
            
            if i_distance_0003 < v_smallest_intermediate_0000 then
                v_smallest_intermediate_0000 := i_distance_0003;
            end if;
            
            if i_distance_0004 < v_smallest_intermediate_0001 then
                v_smallest_intermediate_0001 := i_distance_0004;
            end if;
            
            if i_distance_0005 < v_smallest_intermediate_0001 then
                v_smallest_intermediate_0001 := i_distance_0005;
            end if;
            
            if i_distance_0006 < v_smallest_intermediate_0001 then
                v_smallest_intermediate_0001 := i_distance_0006;
            end if;
            
            if i_distance_0007 < v_smallest_intermediate_0001 then
                v_smallest_intermediate_0001 := i_distance_0007;
            end if;
            
            if i_distance_0008 < v_smallest_intermediate_0002 then
                v_smallest_intermediate_0002 := i_distance_0008;
            end if;
            
            if i_distance_0009 < v_smallest_intermediate_0002 then
                v_smallest_intermediate_0002 := i_distance_0009;
            end if;
            
            if i_distance_0010 < v_smallest_intermediate_0002 then
                v_smallest_intermediate_0002 := i_distance_0010;
            end if;
            
            if i_distance_0011 < v_smallest_intermediate_0002 then
                v_smallest_intermediate_0002 := i_distance_0011;
            end if;
            
            if i_distance_0012 < v_smallest_intermediate_0003 then
                v_smallest_intermediate_0003 := i_distance_0012;
            end if;
            
            if i_distance_0013 < v_smallest_intermediate_0003 then
                v_smallest_intermediate_0003 := i_distance_0013;
            end if;
            
            if i_distance_0014 < v_smallest_intermediate_0003 then
                v_smallest_intermediate_0003 := i_distance_0014;
            end if;
            
            if i_distance_0015 < v_smallest_intermediate_0003 then
                v_smallest_intermediate_0003 := i_distance_0015;
            end if;
            
            if s_smallest_intermediate_0000 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := s_smallest_intermediate_0000;
            end if;
            
            if s_smallest_intermediate_0001 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := s_smallest_intermediate_0001;
            end if;
            
            if s_smallest_intermediate_0002 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := s_smallest_intermediate_0002;
            end if;
            
            if s_smallest_intermediate_0003 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := s_smallest_intermediate_0003;
            end if;
            
            s_smallest_intermediate_0000 <= v_smallest_intermediate_0000;
            s_smallest_intermediate_0001 <= v_smallest_intermediate_0001;
            s_smallest_intermediate_0002 <= v_smallest_intermediate_0002;
            s_smallest_intermediate_0003 <= v_smallest_intermediate_0003;
            s_smallest_intermediate_0004 <= v_smallest_intermediate_0004;
            
        end if;
    end if;
end process;

o_min <= s_smallest_intermediate_0004;
o_latency <= 2;

end behavioral_MinFunction_0016;
