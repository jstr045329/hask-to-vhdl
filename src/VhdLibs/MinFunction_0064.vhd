library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;


entity MinFunction_0064 is
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
        i_distance_0016 : in integer := max_integer;
        i_distance_0017 : in integer := max_integer;
        i_distance_0018 : in integer := max_integer;
        i_distance_0019 : in integer := max_integer;
        i_distance_0020 : in integer := max_integer;
        i_distance_0021 : in integer := max_integer;
        i_distance_0022 : in integer := max_integer;
        i_distance_0023 : in integer := max_integer;
        i_distance_0024 : in integer := max_integer;
        i_distance_0025 : in integer := max_integer;
        i_distance_0026 : in integer := max_integer;
        i_distance_0027 : in integer := max_integer;
        i_distance_0028 : in integer := max_integer;
        i_distance_0029 : in integer := max_integer;
        i_distance_0030 : in integer := max_integer;
        i_distance_0031 : in integer := max_integer;
        i_distance_0032 : in integer := max_integer;
        i_distance_0033 : in integer := max_integer;
        i_distance_0034 : in integer := max_integer;
        i_distance_0035 : in integer := max_integer;
        i_distance_0036 : in integer := max_integer;
        i_distance_0037 : in integer := max_integer;
        i_distance_0038 : in integer := max_integer;
        i_distance_0039 : in integer := max_integer;
        i_distance_0040 : in integer := max_integer;
        i_distance_0041 : in integer := max_integer;
        i_distance_0042 : in integer := max_integer;
        i_distance_0043 : in integer := max_integer;
        i_distance_0044 : in integer := max_integer;
        i_distance_0045 : in integer := max_integer;
        i_distance_0046 : in integer := max_integer;
        i_distance_0047 : in integer := max_integer;
        i_distance_0048 : in integer := max_integer;
        i_distance_0049 : in integer := max_integer;
        i_distance_0050 : in integer := max_integer;
        i_distance_0051 : in integer := max_integer;
        i_distance_0052 : in integer := max_integer;
        i_distance_0053 : in integer := max_integer;
        i_distance_0054 : in integer := max_integer;
        i_distance_0055 : in integer := max_integer;
        i_distance_0056 : in integer := max_integer;
        i_distance_0057 : in integer := max_integer;
        i_distance_0058 : in integer := max_integer;
        i_distance_0059 : in integer := max_integer;
        i_distance_0060 : in integer := max_integer;
        i_distance_0061 : in integer := max_integer;
        i_distance_0062 : in integer := max_integer;
        i_distance_0063 : in integer := max_integer;
        o_min : out integer;
        o_latency : out integer
    );
end MinFunction_0064;


architecture behavioral_MinFunction_0064 of MinFunction_0064 is

signal s_smallest_intermediate_0000 : integer;
signal s_smallest_intermediate_0001 : integer;
signal s_smallest_intermediate_0002 : integer;
signal s_smallest_intermediate_0003 : integer;
signal s_smallest_intermediate_0004 : integer;
signal s_smallest_intermediate_0005 : integer;
signal s_smallest_intermediate_0006 : integer;
signal s_smallest_intermediate_0007 : integer;
signal s_smallest_intermediate_0008 : integer;
signal s_smallest_intermediate_0009 : integer;
signal s_smallest_intermediate_0010 : integer;
signal s_smallest_intermediate_0011 : integer;
signal s_smallest_intermediate_0012 : integer;
signal s_smallest_intermediate_0013 : integer;
signal s_smallest_intermediate_0014 : integer;
signal s_smallest_intermediate_0015 : integer;
signal s_smallest_intermediate_0016 : integer;
signal s_smallest_intermediate_0017 : integer;
signal s_smallest_intermediate_0018 : integer;
signal s_smallest_intermediate_0019 : integer;
signal s_smallest_intermediate_0020 : integer;

begin

process(clk)
    variable v_smallest_intermediate_0000 : integer;
    variable v_smallest_intermediate_0001 : integer;
    variable v_smallest_intermediate_0002 : integer;
    variable v_smallest_intermediate_0003 : integer;
    variable v_smallest_intermediate_0004 : integer;
    variable v_smallest_intermediate_0005 : integer;
    variable v_smallest_intermediate_0006 : integer;
    variable v_smallest_intermediate_0007 : integer;
    variable v_smallest_intermediate_0008 : integer;
    variable v_smallest_intermediate_0009 : integer;
    variable v_smallest_intermediate_0010 : integer;
    variable v_smallest_intermediate_0011 : integer;
    variable v_smallest_intermediate_0012 : integer;
    variable v_smallest_intermediate_0013 : integer;
    variable v_smallest_intermediate_0014 : integer;
    variable v_smallest_intermediate_0015 : integer;
    variable v_smallest_intermediate_0016 : integer;
    variable v_smallest_intermediate_0017 : integer;
    variable v_smallest_intermediate_0018 : integer;
    variable v_smallest_intermediate_0019 : integer;
    variable v_smallest_intermediate_0020 : integer;
begin
    if rising_edge(clk) then 
        if (i_soft_rst or rst) = '1' then 
            s_smallest_intermediate_0001 <= max_integer;
            s_smallest_intermediate_0002 <= max_integer;
            s_smallest_intermediate_0003 <= max_integer;
            s_smallest_intermediate_0004 <= max_integer;
            s_smallest_intermediate_0005 <= max_integer;
            s_smallest_intermediate_0006 <= max_integer;
            s_smallest_intermediate_0007 <= max_integer;
            s_smallest_intermediate_0008 <= max_integer;
            s_smallest_intermediate_0009 <= max_integer;
            s_smallest_intermediate_0010 <= max_integer;
            s_smallest_intermediate_0011 <= max_integer;
            s_smallest_intermediate_0012 <= max_integer;
            s_smallest_intermediate_0013 <= max_integer;
            s_smallest_intermediate_0014 <= max_integer;
            s_smallest_intermediate_0015 <= max_integer;
            s_smallest_intermediate_0016 <= max_integer;
            s_smallest_intermediate_0017 <= max_integer;
            s_smallest_intermediate_0018 <= max_integer;
            s_smallest_intermediate_0019 <= max_integer;
            s_smallest_intermediate_0020 <= max_integer;
            
        else
            
            v_smallest_intermediate_0000 := max_integer;
            v_smallest_intermediate_0001 := max_integer;
            v_smallest_intermediate_0002 := max_integer;
            v_smallest_intermediate_0003 := max_integer;
            v_smallest_intermediate_0004 := max_integer;
            v_smallest_intermediate_0005 := max_integer;
            v_smallest_intermediate_0006 := max_integer;
            v_smallest_intermediate_0007 := max_integer;
            v_smallest_intermediate_0008 := max_integer;
            v_smallest_intermediate_0009 := max_integer;
            v_smallest_intermediate_0010 := max_integer;
            v_smallest_intermediate_0011 := max_integer;
            v_smallest_intermediate_0012 := max_integer;
            v_smallest_intermediate_0013 := max_integer;
            v_smallest_intermediate_0014 := max_integer;
            v_smallest_intermediate_0015 := max_integer;
            v_smallest_intermediate_0016 := max_integer;
            v_smallest_intermediate_0017 := max_integer;
            v_smallest_intermediate_0018 := max_integer;
            v_smallest_intermediate_0019 := max_integer;
            v_smallest_intermediate_0020 := max_integer;
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
            
            if i_distance_0016 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := i_distance_0016;
            end if;
            
            if i_distance_0017 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := i_distance_0017;
            end if;
            
            if i_distance_0018 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := i_distance_0018;
            end if;
            
            if i_distance_0019 < v_smallest_intermediate_0004 then
                v_smallest_intermediate_0004 := i_distance_0019;
            end if;
            
            if i_distance_0020 < v_smallest_intermediate_0005 then
                v_smallest_intermediate_0005 := i_distance_0020;
            end if;
            
            if i_distance_0021 < v_smallest_intermediate_0005 then
                v_smallest_intermediate_0005 := i_distance_0021;
            end if;
            
            if i_distance_0022 < v_smallest_intermediate_0005 then
                v_smallest_intermediate_0005 := i_distance_0022;
            end if;
            
            if i_distance_0023 < v_smallest_intermediate_0005 then
                v_smallest_intermediate_0005 := i_distance_0023;
            end if;
            
            if i_distance_0024 < v_smallest_intermediate_0006 then
                v_smallest_intermediate_0006 := i_distance_0024;
            end if;
            
            if i_distance_0025 < v_smallest_intermediate_0006 then
                v_smallest_intermediate_0006 := i_distance_0025;
            end if;
            
            if i_distance_0026 < v_smallest_intermediate_0006 then
                v_smallest_intermediate_0006 := i_distance_0026;
            end if;
            
            if i_distance_0027 < v_smallest_intermediate_0006 then
                v_smallest_intermediate_0006 := i_distance_0027;
            end if;
            
            if i_distance_0028 < v_smallest_intermediate_0007 then
                v_smallest_intermediate_0007 := i_distance_0028;
            end if;
            
            if i_distance_0029 < v_smallest_intermediate_0007 then
                v_smallest_intermediate_0007 := i_distance_0029;
            end if;
            
            if i_distance_0030 < v_smallest_intermediate_0007 then
                v_smallest_intermediate_0007 := i_distance_0030;
            end if;
            
            if i_distance_0031 < v_smallest_intermediate_0007 then
                v_smallest_intermediate_0007 := i_distance_0031;
            end if;
            
            if i_distance_0032 < v_smallest_intermediate_0008 then
                v_smallest_intermediate_0008 := i_distance_0032;
            end if;
            
            if i_distance_0033 < v_smallest_intermediate_0008 then
                v_smallest_intermediate_0008 := i_distance_0033;
            end if;
            
            if i_distance_0034 < v_smallest_intermediate_0008 then
                v_smallest_intermediate_0008 := i_distance_0034;
            end if;
            
            if i_distance_0035 < v_smallest_intermediate_0008 then
                v_smallest_intermediate_0008 := i_distance_0035;
            end if;
            
            if i_distance_0036 < v_smallest_intermediate_0009 then
                v_smallest_intermediate_0009 := i_distance_0036;
            end if;
            
            if i_distance_0037 < v_smallest_intermediate_0009 then
                v_smallest_intermediate_0009 := i_distance_0037;
            end if;
            
            if i_distance_0038 < v_smallest_intermediate_0009 then
                v_smallest_intermediate_0009 := i_distance_0038;
            end if;
            
            if i_distance_0039 < v_smallest_intermediate_0009 then
                v_smallest_intermediate_0009 := i_distance_0039;
            end if;
            
            if i_distance_0040 < v_smallest_intermediate_0010 then
                v_smallest_intermediate_0010 := i_distance_0040;
            end if;
            
            if i_distance_0041 < v_smallest_intermediate_0010 then
                v_smallest_intermediate_0010 := i_distance_0041;
            end if;
            
            if i_distance_0042 < v_smallest_intermediate_0010 then
                v_smallest_intermediate_0010 := i_distance_0042;
            end if;
            
            if i_distance_0043 < v_smallest_intermediate_0010 then
                v_smallest_intermediate_0010 := i_distance_0043;
            end if;
            
            if i_distance_0044 < v_smallest_intermediate_0011 then
                v_smallest_intermediate_0011 := i_distance_0044;
            end if;
            
            if i_distance_0045 < v_smallest_intermediate_0011 then
                v_smallest_intermediate_0011 := i_distance_0045;
            end if;
            
            if i_distance_0046 < v_smallest_intermediate_0011 then
                v_smallest_intermediate_0011 := i_distance_0046;
            end if;
            
            if i_distance_0047 < v_smallest_intermediate_0011 then
                v_smallest_intermediate_0011 := i_distance_0047;
            end if;
            
            if i_distance_0048 < v_smallest_intermediate_0012 then
                v_smallest_intermediate_0012 := i_distance_0048;
            end if;
            
            if i_distance_0049 < v_smallest_intermediate_0012 then
                v_smallest_intermediate_0012 := i_distance_0049;
            end if;
            
            if i_distance_0050 < v_smallest_intermediate_0012 then
                v_smallest_intermediate_0012 := i_distance_0050;
            end if;
            
            if i_distance_0051 < v_smallest_intermediate_0012 then
                v_smallest_intermediate_0012 := i_distance_0051;
            end if;
            
            if i_distance_0052 < v_smallest_intermediate_0013 then
                v_smallest_intermediate_0013 := i_distance_0052;
            end if;
            
            if i_distance_0053 < v_smallest_intermediate_0013 then
                v_smallest_intermediate_0013 := i_distance_0053;
            end if;
            
            if i_distance_0054 < v_smallest_intermediate_0013 then
                v_smallest_intermediate_0013 := i_distance_0054;
            end if;
            
            if i_distance_0055 < v_smallest_intermediate_0013 then
                v_smallest_intermediate_0013 := i_distance_0055;
            end if;
            
            if i_distance_0056 < v_smallest_intermediate_0014 then
                v_smallest_intermediate_0014 := i_distance_0056;
            end if;
            
            if i_distance_0057 < v_smallest_intermediate_0014 then
                v_smallest_intermediate_0014 := i_distance_0057;
            end if;
            
            if i_distance_0058 < v_smallest_intermediate_0014 then
                v_smallest_intermediate_0014 := i_distance_0058;
            end if;
            
            if i_distance_0059 < v_smallest_intermediate_0014 then
                v_smallest_intermediate_0014 := i_distance_0059;
            end if;
            
            if i_distance_0060 < v_smallest_intermediate_0015 then
                v_smallest_intermediate_0015 := i_distance_0060;
            end if;
            
            if i_distance_0061 < v_smallest_intermediate_0015 then
                v_smallest_intermediate_0015 := i_distance_0061;
            end if;
            
            if i_distance_0062 < v_smallest_intermediate_0015 then
                v_smallest_intermediate_0015 := i_distance_0062;
            end if;
            
            if i_distance_0063 < v_smallest_intermediate_0015 then
                v_smallest_intermediate_0015 := i_distance_0063;
            end if;
            
            if s_smallest_intermediate_0000 < v_smallest_intermediate_0016 then
                v_smallest_intermediate_0016 := s_smallest_intermediate_0000;
            end if;
            
            if s_smallest_intermediate_0001 < v_smallest_intermediate_0016 then
                v_smallest_intermediate_0016 := s_smallest_intermediate_0001;
            end if;
            
            if s_smallest_intermediate_0002 < v_smallest_intermediate_0016 then
                v_smallest_intermediate_0016 := s_smallest_intermediate_0002;
            end if;
            
            if s_smallest_intermediate_0003 < v_smallest_intermediate_0016 then
                v_smallest_intermediate_0016 := s_smallest_intermediate_0003;
            end if;
            
            if s_smallest_intermediate_0004 < v_smallest_intermediate_0017 then
                v_smallest_intermediate_0017 := s_smallest_intermediate_0004;
            end if;
            
            if s_smallest_intermediate_0005 < v_smallest_intermediate_0017 then
                v_smallest_intermediate_0017 := s_smallest_intermediate_0005;
            end if;
            
            if s_smallest_intermediate_0006 < v_smallest_intermediate_0017 then
                v_smallest_intermediate_0017 := s_smallest_intermediate_0006;
            end if;
            
            if s_smallest_intermediate_0007 < v_smallest_intermediate_0017 then
                v_smallest_intermediate_0017 := s_smallest_intermediate_0007;
            end if;
            
            if s_smallest_intermediate_0008 < v_smallest_intermediate_0018 then
                v_smallest_intermediate_0018 := s_smallest_intermediate_0008;
            end if;
            
            if s_smallest_intermediate_0009 < v_smallest_intermediate_0018 then
                v_smallest_intermediate_0018 := s_smallest_intermediate_0009;
            end if;
            
            if s_smallest_intermediate_0010 < v_smallest_intermediate_0018 then
                v_smallest_intermediate_0018 := s_smallest_intermediate_0010;
            end if;
            
            if s_smallest_intermediate_0011 < v_smallest_intermediate_0018 then
                v_smallest_intermediate_0018 := s_smallest_intermediate_0011;
            end if;
            
            if s_smallest_intermediate_0012 < v_smallest_intermediate_0019 then
                v_smallest_intermediate_0019 := s_smallest_intermediate_0012;
            end if;
            
            if s_smallest_intermediate_0013 < v_smallest_intermediate_0019 then
                v_smallest_intermediate_0019 := s_smallest_intermediate_0013;
            end if;
            
            if s_smallest_intermediate_0014 < v_smallest_intermediate_0019 then
                v_smallest_intermediate_0019 := s_smallest_intermediate_0014;
            end if;
            
            if s_smallest_intermediate_0015 < v_smallest_intermediate_0019 then
                v_smallest_intermediate_0019 := s_smallest_intermediate_0015;
            end if;
            
            if s_smallest_intermediate_0016 < v_smallest_intermediate_0020 then
                v_smallest_intermediate_0020 := s_smallest_intermediate_0016;
            end if;
            
            if s_smallest_intermediate_0017 < v_smallest_intermediate_0020 then
                v_smallest_intermediate_0020 := s_smallest_intermediate_0017;
            end if;
            
            if s_smallest_intermediate_0018 < v_smallest_intermediate_0020 then
                v_smallest_intermediate_0020 := s_smallest_intermediate_0018;
            end if;
            
            if s_smallest_intermediate_0019 < v_smallest_intermediate_0020 then
                v_smallest_intermediate_0020 := s_smallest_intermediate_0019;
            end if;
            
            s_smallest_intermediate_0000 <= v_smallest_intermediate_0000;
            s_smallest_intermediate_0001 <= v_smallest_intermediate_0001;
            s_smallest_intermediate_0002 <= v_smallest_intermediate_0002;
            s_smallest_intermediate_0003 <= v_smallest_intermediate_0003;
            s_smallest_intermediate_0004 <= v_smallest_intermediate_0004;
            s_smallest_intermediate_0005 <= v_smallest_intermediate_0005;
            s_smallest_intermediate_0006 <= v_smallest_intermediate_0006;
            s_smallest_intermediate_0007 <= v_smallest_intermediate_0007;
            s_smallest_intermediate_0008 <= v_smallest_intermediate_0008;
            s_smallest_intermediate_0009 <= v_smallest_intermediate_0009;
            s_smallest_intermediate_0010 <= v_smallest_intermediate_0010;
            s_smallest_intermediate_0011 <= v_smallest_intermediate_0011;
            s_smallest_intermediate_0012 <= v_smallest_intermediate_0012;
            s_smallest_intermediate_0013 <= v_smallest_intermediate_0013;
            s_smallest_intermediate_0014 <= v_smallest_intermediate_0014;
            s_smallest_intermediate_0015 <= v_smallest_intermediate_0015;
            s_smallest_intermediate_0016 <= v_smallest_intermediate_0016;
            s_smallest_intermediate_0017 <= v_smallest_intermediate_0017;
            s_smallest_intermediate_0018 <= v_smallest_intermediate_0018;
            s_smallest_intermediate_0019 <= v_smallest_intermediate_0019;
            s_smallest_intermediate_0020 <= v_smallest_intermediate_0020;
            
        end if;
    end if;
end process;

o_min <= s_smallest_intermediate_0020;
o_latency <= 3;

end behavioral_MinFunction_0064;
