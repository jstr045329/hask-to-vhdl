------------------------------------------------------------------------------------------------------------------------
--                                          Testbench for PowerSet_0016_0016
------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;


entity PowerSet_0016_0016_tb is
end PowerSet_0016_0016_tb;


architecture behavioral_PowerSet_0016_0016_tb of PowerSet_0016_0016_tb is


component PowerSet_0016_0016 is generic ( rst_level : std_logic := '1' ; 
rst_value : std_logic := '0' ; 
clk_edge : std_logic := '1' ; 
allow_fast_reset : std_logic := '1' port ( clk : in std_logic ; 
rst : in std_logic ; 
i_bit_num_1 : in integer ; 
i_set_num_1 : in integer ; 
i_add : in std_logic ; 
i_rem : in std_logic ; 
i_set_num_2 : in integer ; 
i_dest_num : in integer ; 
i_union : in std_logic ; 
i_intersection : in std_logic ; 
i_complement : in std_logic ; 
i_duplicate : in std_logic ; 
i_read_set_num_0000 : in integer ; 
i_read_bit_num_0000 : in integer ; 
i_read_enable_0000 : in integer ; 
o_member_0000 : out std_logic ; 
o_out_valid_0000 : out std_logic ; 
i_read_set_num_0001 : in integer ; 
i_read_bit_num_0001 : in integer ; 
i_read_enable_0001 : in integer ; 
o_member_0001 : out std_logic ; 
o_out_valid_0001 : out std_logic ; 
i_read_set_num_0002 : in integer ; 
i_read_bit_num_0002 : in integer ; 
i_read_enable_0002 : in integer ; 
o_member_0002 : out std_logic ; 
o_out_valid_0002 : out std_logic ; 
i_read_set_num_0003 : in integer ; 
i_read_bit_num_0003 : in integer ; 
i_read_enable_0003 : in integer ; 
o_member_0003 : out std_logic ; 
o_out_valid_0003 : out std_logic ; 
o_collision_warning : out std_logic ) ; 
end component ; 


constant rst_level : std_logic := '1';
constant rst_value : std_logic := '0';
constant clk_edge : std_logic := '1';
constant allow_fast_reset : std_logic := '1';
constant rst : in;
constant i_bit_num_1 : in;
constant i_set_num_1 : in;
constant i_add : in;
constant i_rem : in;
constant i_set_num_2 : in;
constant i_dest_num : in;
constant i_union : in;
constant i_intersection : in;
constant i_complement : in;
constant i_duplicate : in;
constant i_read_set_num_0000 : in;
constant i_read_bit_num_0000 : in;
constant i_read_enable_0000 : in;
constant o_member_0000 : out;
constant o_out_valid_0000 : out;
constant i_read_set_num_0001 : in;
constant i_read_bit_num_0001 : in;
constant i_read_enable_0001 : in;
constant o_member_0001 : out;
constant o_out_valid_0001 : out;
constant i_read_set_num_0002 : in;
constant i_read_bit_num_0002 : in;
constant i_read_enable_0002 : in;
constant o_member_0002 : out;
constant o_out_valid_0002 : out;
constant i_read_set_num_0003 : in;
constant i_read_bit_num_0003 : in;
constant i_read_enable_0003 : in;
constant o_member_0003 : out;
constant o_out_valid_0003 : out;
constant o_collision_warning : out;
constant end : ;;
constant architecture : of := is;
constant constant : : := :=;
constant constant : : := :=;
constant constant : : := :=;
constant type : is;
constant type : is;
constant type : is;
constant type : is;
constant write_enable_row : t_one_row;
constant end : ;;
constant constant : : := :=;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant signal : :;
constant function : (;
constant which_col : integer;
constant n_rows : integer;
constant begin : i := 0;
constant end : ;;
constant return : ;;
constant end : ;;
constant function : (;
constant v_operand_2 : t_one_col;
constant v_union : std_logic;
constant v_intersection : std_logic;
constant v_complement : std_logic;
constant v_duplicate : std_logic;
constant n_rows : integer;
constant begin : := := ;;
constant if : = := then;
constant end : ;;
constant y.write_enable_row : ( := =>;
constant y.write_enable_row : i_dest_num := :=;
constant elsif : = := then;
constant end : ;;
constant y.write_enable_row : ( := =>;
constant y.write_enable_row : i_dest_num := :=;
constant elsif : = := then;
constant end : ;;
constant y.write_enable_row : ( := =>;
constant y.write_enable_row : i_dest_num := :=;
constant elsif : = := then;
constant end : ;;
constant y.write_enable_row : ( := =>;
constant y.write_enable_row : i_dest_num := :=;
constant else : := := others;
constant y.write_enable_row : ( := =>;
constant end : ;;
constant return : ;;
constant end : ;;
constant begin : <=;
constant OPERAND_DRIVER : process;
constant s_operand_2 : (;
constant else : <=;
constant s_operand_2 : extractOneColumn;
constant end : ;;
constant end : ;;
constant end : ;;
constant RISING_EDGE_VERSION : if;
constant begin : rising_edge;
constant s_global_write_enable : (;
constant else : := := (;
constant s_result : v_brain_pack.one_set;
constant s_global_write_enable : v_brain_pack.write_enable_row;
constant end : ;;
constant end : ;;
constant end : ;;
constant end : ;;
constant FALLING_EDGE_VERSION : if;
constant s_global_write_enable : (;
constant else : := := (;
constant s_result : v_brain_pack.one_set;
constant s_global_write_enable : v_brain_pack.write_enable_row;
constant end : ;;
constant end : ;;
constant end : ;;
constant end : ;;
constant COLLISION_WARNING_DRIVER : process;
constant else : b2SL_And;
constant end : ;;
constant end : ;;
constant end : ;;
constant end : ;;
constant ARRAY_DRIVER : process;
constant else : <=;
constant end : ;;
constant end : ;;
constant end : ;;
constant BIT_INSTANCE_0000_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0000 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0001 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0002 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0003 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0004 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0005 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0006 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0007 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0008 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0009 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0010 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0011 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0012 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0013 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0014 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0000_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0001_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0002_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0003_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0004_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0005_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0006_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0007_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0008_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0009_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0010_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0011_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0012_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0013_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0014_0015 : OneSetBit;
constant port : (;
constant BIT_INSTANCE_0015_0015 : OneSetBit;
constant port : (;
constant READ_PORT_DRIVER : process;
constant o_out_valid_0000 : '0';
constant o_member_0001 : '0';
constant o_out_valid_0001 : '0';
constant o_member_0002 : '0';
constant o_out_valid_0002 : '0';
constant o_member_0003 : '0';
constant o_out_valid_0003 : '0';
constant else : i_read_enable_0000;
constant o_out_valid_0000 : '1';
constant else : <=;
constant o_out_valid_0000 : '0';
constant end : ;;
constant if : =;
constant o_out_valid_0001 : '1';
constant else : <=;
constant o_out_valid_0001 : '0';
constant end : ;;
constant if : =;
constant o_out_valid_0002 : '1';
constant else : <=;
constant o_out_valid_0002 : '0';
constant end : ;;
constant if : =;
constant o_out_valid_0003 : '1';
constant else : <=;
constant o_out_valid_0003 : '0';
constant end : ;;
constant end : ;;
constant end : ;;
constant end : ;;
constant end : ;;
constant library : ;;
constant use : ;;
constant use : ;;
constant use : ;;
constant package : is := PowerSet_0016_0016;
constant rst_value : std_logic := '0';
constant clk_edge : std_logic := '1';
constant allow_fast_reset : std_logic := '1';
constant port : clk;
constant rst : in;
constant i_bit_num_1 : in;
constant i_set_num_1 : in;
constant i_add : in;
constant i_rem : in;
constant i_set_num_2 : in;
constant i_dest_num : in;
constant i_union : in;
constant i_intersection : in;
constant i_complement : in;
constant i_duplicate : in;
constant i_read_set_num_0000 : in;
constant i_read_bit_num_0000 : in;
constant i_read_enable_0000 : in;
constant o_member_0000 : out;
constant o_out_valid_0000 : out;
constant i_read_set_num_0001 : in;
constant i_read_bit_num_0001 : in;
constant i_read_enable_0001 : in;
constant o_member_0001 : out;
constant o_out_valid_0001 : out;
constant i_read_set_num_0002 : in;
constant i_read_bit_num_0002 : in;
constant i_read_enable_0002 : in;
constant o_member_0002 : out;
constant o_out_valid_0002 : out;
constant i_read_set_num_0003 : in;
constant i_read_bit_num_0003 : in;
constant i_read_enable_0003 : in;
constant o_member_0003 : out;
constant o_out_valid_0003 : out;
constant o_collision_warning : out;
constant end : ;;
constant end : PowerSetPkg;
constant package : PowerSetPkg;
signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal s_bit_num_1_000000 : integer;
signal s_set_num_1_000000 : integer;
signal s_add_000000 : std_logic;
signal s_rem_000000 : std_logic;
signal s_set_num_2_000000 : integer;
signal s_dest_num_000000 : integer;
signal s_union_000000 : std_logic;
signal s_intersection_000000 : std_logic;
signal s_complement_000000 : std_logic;
signal s_duplicate_000000 : std_logic;
signal s_read_set_num_0000_000000 : integer;
signal s_read_bit_num_0000_000000 : integer;
signal s_read_enable_0000_000000 : integer;
signal s_member_0000_000000 : std_logic;
signal s_out_valid_0000_000000 : std_logic;
signal s_read_set_num_0001_000000 : integer;
signal s_read_bit_num_0001_000000 : integer;
signal s_read_enable_0001_000000 : integer;
signal s_member_0001_000000 : std_logic;
signal s_out_valid_0001_000000 : std_logic;
signal s_read_set_num_0002_000000 : integer;
signal s_read_bit_num_0002_000000 : integer;
signal s_read_enable_0002_000000 : integer;
signal s_member_0002_000000 : std_logic;
signal s_out_valid_0002_000000 : std_logic;
signal s_read_set_num_0003_000000 : integer;
signal s_read_bit_num_0003_000000 : integer;
signal s_read_enable_0003_000000 : integer;
signal s_member_0003_000000 : std_logic;
signal s_out_valid_0003_000000 : std_logic;
signal s_collision_warning_000000 : std_logic;

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';
signal test_stage : integer := 0;
signal s_clock_cycle_count : integer := 0;


begin


------------------------------------------------------------------------------------------------------------------------
--                                                    Boiler Plate
------------------------------------------------------------------------------------------------------------------------
CLOCK_PROCESS: process
begin
    if sim_done = '1' then
        wait;
    else
        wait for clk_per/2;
        clk <= not clk;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                 Count Clock Cycles
------------------------------------------------------------------------------------------------------------------------
CLOCK_CYCLE_COUNTER: process(clk)
begin
    if rising_edge(clk) then 
        s_clock_cycle_count <= s_clock_cycle_count + 1;
    end if;
end process;


------------------------------------------------------------------------------------------------------------------------
--                                                    Stim Process
------------------------------------------------------------------------------------------------------------------------
STIM_PROCESS: process
begin
    s_bit_num_1_000000 <= 0;
    s_set_num_1_000000 <= 0;
    s_add_000000 <= '0';
    s_rem_000000 <= '0';
    s_set_num_2_000000 <= 0;
    s_dest_num_000000 <= 0;
    s_union_000000 <= '0';
    s_intersection_000000 <= '0';
    s_complement_000000 <= '0';
    s_duplicate_000000 <= '0';
    s_read_set_num_0000_000000 <= 0;
    s_read_bit_num_0000_000000 <= 0;
    s_read_enable_0000_000000 <= 0;
    s_read_set_num_0001_000000 <= 0;
    s_read_bit_num_0001_000000 <= 0;
    s_read_enable_0001_000000 <= 0;
    s_read_set_num_0002_000000 <= 0;
    s_read_bit_num_0002_000000 <= 0;
    s_read_enable_0002_000000 <= 0;
    s_read_set_num_0003_000000 <= 0;
    s_read_bit_num_0003_000000 <= 0;
    s_read_enable_0003_000000 <= 0;
    s_bit_num_1_000000 <= 0;
    s_set_num_1_000000 <= 0;
    s_add_000000 <= '0';
    s_rem_000000 <= '0';
    s_set_num_2_000000 <= 0;
    s_dest_num_000000 <= 0;
    s_union_000000 <= '0';
    s_intersection_000000 <= '0';
    s_complement_000000 <= '0';
    s_duplicate_000000 <= '0';
    s_read_set_num_0000_000000 <= 0;
    s_read_bit_num_0000_000000 <= 0;
    s_read_enable_0000_000000 <= 0;
    s_read_set_num_0001_000000 <= 0;
    s_read_bit_num_0001_000000 <= 0;
    s_read_enable_0001_000000 <= 0;
    s_read_set_num_0002_000000 <= 0;
    s_read_bit_num_0002_000000 <= 0;
    s_read_enable_0002_000000 <= 0;
    s_read_set_num_0003_000000 <= 0;
    s_read_bit_num_0003_000000 <= 0;
    s_read_enable_0003_000000 <= 0;
    test_stage <= 0;
    sync_wait_rising(clk, 10);
    rst_value <= not rst_value;
    rst <= not rst;
    rst_value <= not rst_value;
    rst <= not rst;
    sync_wait_rising(clk, 10);


    -- Test ends here:
    test_stage <= test_stage + 1;
    sync_wait_rising(clk, 100);
    sim_done <= '1';
    wait;
end process;




------------------------------------------------------------------------------------------------------------------------
--                                                  Unit Under Test
------------------------------------------------------------------------------------------------------------------------
UUT
: 
PowerSet_0016_0016
generic map (
    PowerSet_0016_0016 => s_PowerSet_0016_0016_000000,
    rst_value => s_rst_value_000000,
    clk_edge => s_clk_edge_000000,
    allow_fast_reset => s_allow_fast_reset_000000
)
port map (
    clk => clk,
    rst => rst,
    i_bit_num_1 => s_bit_num_1_000000,
    i_set_num_1 => s_set_num_1_000000,
    i_add => s_add_000000,
    i_rem => s_rem_000000,
    i_set_num_2 => s_set_num_2_000000,
    i_dest_num => s_dest_num_000000,
    i_union => s_union_000000,
    i_intersection => s_intersection_000000,
    i_complement => s_complement_000000,
    i_duplicate => s_duplicate_000000,
    i_read_set_num_0000 => s_read_set_num_0000_000000,
    i_read_bit_num_0000 => s_read_bit_num_0000_000000,
    i_read_enable_0000 => s_read_enable_0000_000000,
    o_member_0000 => s_member_0000_000000,
    o_out_valid_0000 => s_out_valid_0000_000000,
    i_read_set_num_0001 => s_read_set_num_0001_000000,
    i_read_bit_num_0001 => s_read_bit_num_0001_000000,
    i_read_enable_0001 => s_read_enable_0001_000000,
    o_member_0001 => s_member_0001_000000,
    o_out_valid_0001 => s_out_valid_0001_000000,
    i_read_set_num_0002 => s_read_set_num_0002_000000,
    i_read_bit_num_0002 => s_read_bit_num_0002_000000,
    i_read_enable_0002 => s_read_enable_0002_000000,
    o_member_0002 => s_member_0002_000000,
    o_out_valid_0002 => s_out_valid_0002_000000,
    i_read_set_num_0003 => s_read_set_num_0003_000000,
    i_read_bit_num_0003 => s_read_bit_num_0003_000000,
    i_read_enable_0003 => s_read_enable_0003_000000,
    o_member_0003 => s_member_0003_000000,
    o_out_valid_0003 => s_out_valid_0003_000000,
    o_collision_warning => s_collision_warning_000000
);


end architecture behavioral_PowerSet_0016_0016_tb;


