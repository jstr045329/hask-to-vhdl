----------------------------------------------------------------------------------------------------
-- WARNING: This is a machine generated file. Do not modify.
-- Run PopulatePowerSetTemplate.py if you need another one.
----------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------
--                     Power Set With 0004 Sets And 0004 Elements In Each Set 
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PowerSetGears.all;
use work.PrefixGatesPkg.all;


entity PowerSet_0004_0004 is
    generic (
        rst_level : std_logic := '1'; -- Level of global reset that resets the device.
        rst_value : std_logic := '0'; -- Value that s_member is assigned during reset.
        clk_edge : std_logic := '1';
        allow_fast_reset : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        
        -- Enable operations on individual bits:
        i_bit_num_1 : in integer; -- Bit number = row number
        i_set_num_1 : in integer; -- Set number = column number
        i_add : in std_logic;
        i_rem : in std_logic;
        
        -- Enable operations between sets:
        i_set_num_2 : in integer;
        i_dest_num : in integer;
        i_union : in std_logic;
        i_intersection : in std_logic;
        
        -- The following operations only use i_set_num_1 and i_dest_num
        -- (i_set_num_2 is ignored):
        i_complement : in std_logic; 
        i_duplicate : in std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0000
        --------------------------------------------------------------------------------
        i_read_set_num_0000 : in integer;
        i_read_bit_num_0000 : in integer;
        i_read_enable_0000 : in std_logic;
        o_member_0000 : out std_logic;
        o_out_valid_0000 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0001
        --------------------------------------------------------------------------------
        i_read_set_num_0001 : in integer;
        i_read_bit_num_0001 : in integer;
        i_read_enable_0001 : in std_logic;
        o_member_0001 : out std_logic;
        o_out_valid_0001 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0002
        --------------------------------------------------------------------------------
        i_read_set_num_0002 : in integer;
        i_read_bit_num_0002 : in integer;
        i_read_enable_0002 : in std_logic;
        o_member_0002 : out std_logic;
        o_out_valid_0002 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0003
        --------------------------------------------------------------------------------
        i_read_set_num_0003 : in integer;
        i_read_bit_num_0003 : in integer;
        i_read_enable_0003 : in std_logic;
        o_member_0003 : out std_logic;
        o_out_valid_0003 : out std_logic;
        
        -- If both ports try to write to the same column at the same time, this warning goes hot. 
        -- Note that the potential for collisions is between i_set_num_1 and i_dest_num, not 
        -- i_set_num_1 and i_set_num_2.
        o_collision_warning : out std_logic 
    );
end PowerSet_0004_0004;


architecture behavioral_PowerSet_0004_0004 of PowerSet_0004_0004 is

constant ROW_BITS : integer := 0002;
constant COL_BITS : integer := 0002;

constant NUM_ROWS : integer := 2 ** ROW_BITS;
constant NUM_COLS : integer := 2 ** COL_BITS;

type t_one_row is array (0 to NUM_COLS-1) of std_logic;
type t_one_col is array (0 to NUM_ROWS-1) of std_logic;
type t_one_array is array (0 to NUM_ROWS-1, 0 to NUM_COLS-1) of std_logic;
type t_new_set_result is record 
    one_set : t_one_col;
    write_enable_row : t_one_row;
end record;

constant BLANK_SET_RESULT : t_new_set_result := (
    one_set => (others => '0'),
    write_enable_row => (others => '0')
    );

signal s_global_fast_reset : std_logic;
signal s_collision_warning : std_logic;
signal s_operand_1 : t_one_col;
signal s_operand_2 : t_one_col;
signal s_result : t_one_col;
signal s_write_column : std_logic;

-- Each column in this row vector drives the write enable pin on 1 set:
signal s_global_write_enable : t_one_row; 
signal s_one_array_d : t_one_array;
signal s_one_array_q : t_one_array;

function extractOneColumn(
    x : t_one_array;
    which_col : integer;
    n_rows : integer
    ) return t_one_col is 
variable y : t_one_col;
begin 
    for i in 0 to n_rows-1 loop 
        y(i) := x(i, which_col);
    end loop;
    return y;
end function;

function newSetBrain(
    v_operand_1 : t_one_col;
    v_operand_2 : t_one_col;
    v_union : std_logic;
    v_intersection : std_logic;
    v_complement : std_logic;
    v_duplicate : std_logic;
    n_rows : integer;
    v_some_set_num : integer
    ) return t_new_set_result is 
variable y : t_new_set_result;
begin 
    y := BLANK_SET_RESULT;
    if v_union = '1' then 
        for i in 0 to n_rows-1 loop 
            y.one_set(i) := v_operand_1(i) or v_operand_2(i);
        end loop;
        y.write_enable_row := (others => '0');
        y.write_enable_row(v_some_set_num) := '1';
        
    elsif v_intersection = '1' then 
        for i in 0 to n_rows-1 loop 
            y.one_set(i) := v_operand_1(i) and v_operand_2(i);
        end loop;
        y.write_enable_row := (others => '0');
        y.write_enable_row(v_some_set_num) := '1';

    elsif v_complement = '1' then 
        for i in 0 to n_rows-1 loop 
            y.one_set(i) := not v_operand_1(i);
        end loop;
        y.write_enable_row := (others => '0');
        y.write_enable_row(v_some_set_num) := '1';

    elsif v_duplicate = '1' then 
        for i in 0 to n_rows-1 loop 
            y.one_set(i) := v_operand_1(i);
        end loop;
        y.write_enable_row := (others => '0');
        y.write_enable_row(v_some_set_num) := '1';
        
    else 
        y.one_set := (others => '0');
        y.write_enable_row := (others => '0');
    
    end if;    
    return y;
end function;


begin


s_global_fast_reset <= rst and allow_fast_reset;

--       Be sure to include OneSetBit. 

OPERAND_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            s_operand_1 <= (others => '0');
            s_operand_2 <= (others => '0');
        else
            s_operand_1 <= extractOneColumn(s_one_array_q, i_set_num_1, NUM_ROWS);
            s_operand_2 <= extractOneColumn(s_one_array_q, i_set_num_2, NUM_ROWS);
        end if;
    end if;
end process;


RISING_EDGE_VERSION: if clk_edge = '1' generate
    RESULTS_MAKER: process(clk)
    variable v_brain_pack : t_new_set_result;
    begin 
        if rising_edge(clk) then 
            if s_global_fast_reset = rst_level then 
                s_result <= (others => '0');
                s_global_write_enable <= (others => '0');
            else
                v_brain_pack := newSetBrain(s_operand_1, s_operand_2, i_union, i_intersection, i_complement, i_duplicate, NUM_ROWS, i_dest_num);
                s_result <= v_brain_pack.one_set;
                s_global_write_enable <= v_brain_pack.write_enable_row;
            end if;
        end if;
    end process;
end generate;


FALLING_EDGE_VERSION: if clk_edge = '0' generate
    RESULTS_MAKER_FALLING: process(clk)
    variable v_brain_pack : t_new_set_result;
    begin 
        if falling_edge(clk) then 
            if s_global_fast_reset = rst_level then 
                s_result <= (others => '0');
                s_global_write_enable <= (others => '0');
            else
                v_brain_pack := newSetBrain(s_operand_1, s_operand_2, i_union, i_intersection, i_complement, i_duplicate, NUM_ROWS, i_dest_num);
                s_result <= v_brain_pack.one_set;
                s_global_write_enable <= v_brain_pack.write_enable_row;
            end if;
        end if;
    end process;
end generate;


-- Likewise for every other process in here.
COLLISION_WARNING_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            s_collision_warning <= '0';
        else
            -- If the two set numbers are equal, that alone does not set the sticky warning. 
            -- But if the two set numbers are equal and both write ports attempt to do something, 
            -- that sets the sticky warning. 
            if b2SL_And(
                i_set_num_1 = i_dest_num, 
                b2SL_Or(i_union = '1', i_intersection = '1'),
                b2SL_Or(i_add = '1', i_rem = '1')) then 
                s_collision_warning <= '1';
            end if;
        end if;
    end if;
end process;


ARRAY_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            s_one_array_q <= (others => (others => '0'));
        else
            s_one_array_q <= s_one_array_d;
        end if;
    end if;
end process;



BIT_INSTANCE_0000_0000: OneSetBit 
    generic map (
        my_col => 0000, 
        my_row => 0000,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0000),
        i_we => s_global_write_enable(0000),
        o_member => s_one_array_d(0000, 0000)
    );

BIT_INSTANCE_0001_0000: OneSetBit 
    generic map (
        my_col => 0000, 
        my_row => 0001,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0001),
        i_we => s_global_write_enable(0000),
        o_member => s_one_array_d(0001, 0000)
    );

BIT_INSTANCE_0002_0000: OneSetBit 
    generic map (
        my_col => 0000, 
        my_row => 0002,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0002),
        i_we => s_global_write_enable(0000),
        o_member => s_one_array_d(0002, 0000)
    );

BIT_INSTANCE_0003_0000: OneSetBit 
    generic map (
        my_col => 0000, 
        my_row => 0003,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0003),
        i_we => s_global_write_enable(0000),
        o_member => s_one_array_d(0003, 0000)
    );

BIT_INSTANCE_0000_0001: OneSetBit 
    generic map (
        my_col => 0001, 
        my_row => 0000,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0000),
        i_we => s_global_write_enable(0001),
        o_member => s_one_array_d(0000, 0001)
    );

BIT_INSTANCE_0001_0001: OneSetBit 
    generic map (
        my_col => 0001, 
        my_row => 0001,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0001),
        i_we => s_global_write_enable(0001),
        o_member => s_one_array_d(0001, 0001)
    );

BIT_INSTANCE_0002_0001: OneSetBit 
    generic map (
        my_col => 0001, 
        my_row => 0002,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0002),
        i_we => s_global_write_enable(0001),
        o_member => s_one_array_d(0002, 0001)
    );

BIT_INSTANCE_0003_0001: OneSetBit 
    generic map (
        my_col => 0001, 
        my_row => 0003,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0003),
        i_we => s_global_write_enable(0001),
        o_member => s_one_array_d(0003, 0001)
    );

BIT_INSTANCE_0000_0002: OneSetBit 
    generic map (
        my_col => 0002, 
        my_row => 0000,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0000),
        i_we => s_global_write_enable(0002),
        o_member => s_one_array_d(0000, 0002)
    );

BIT_INSTANCE_0001_0002: OneSetBit 
    generic map (
        my_col => 0002, 
        my_row => 0001,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0001),
        i_we => s_global_write_enable(0002),
        o_member => s_one_array_d(0001, 0002)
    );

BIT_INSTANCE_0002_0002: OneSetBit 
    generic map (
        my_col => 0002, 
        my_row => 0002,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0002),
        i_we => s_global_write_enable(0002),
        o_member => s_one_array_d(0002, 0002)
    );

BIT_INSTANCE_0003_0002: OneSetBit 
    generic map (
        my_col => 0002, 
        my_row => 0003,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0003),
        i_we => s_global_write_enable(0002),
        o_member => s_one_array_d(0003, 0002)
    );

BIT_INSTANCE_0000_0003: OneSetBit 
    generic map (
        my_col => 0003, 
        my_row => 0000,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0000),
        i_we => s_global_write_enable(0003),
        o_member => s_one_array_d(0000, 0003)
    );

BIT_INSTANCE_0001_0003: OneSetBit 
    generic map (
        my_col => 0003, 
        my_row => 0001,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0001),
        i_we => s_global_write_enable(0003),
        o_member => s_one_array_d(0001, 0003)
    );

BIT_INSTANCE_0002_0003: OneSetBit 
    generic map (
        my_col => 0003, 
        my_row => 0002,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0002),
        i_we => s_global_write_enable(0003),
        o_member => s_one_array_d(0002, 0003)
    );

BIT_INSTANCE_0003_0003: OneSetBit 
    generic map (
        my_col => 0003, 
        my_row => 0003,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => 0004,
        num_cols => 0004
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(0003),
        i_we => s_global_write_enable(0003),
        o_member => s_one_array_d(0003, 0003)
    );



READ_PORT_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 

            o_member_0000 <= '0';
            o_out_valid_0000 <= '0';

            o_member_0001 <= '0';
            o_out_valid_0001 <= '0';

            o_member_0002 <= '0';
            o_out_valid_0002 <= '0';

            o_member_0003 <= '0';
            o_out_valid_0003 <= '0';
        else

            if i_read_enable_0000 = '1' then 
                o_member_0000 <= s_one_array_q(i_set_num_1, i_bit_num_1);
                o_out_valid_0000 <= '1';
            else 
                o_member_0000 <= '0';
                o_out_valid_0000 <= '0';
            end if;

            if i_read_enable_0001 = '1' then 
                o_member_0001 <= s_one_array_q(i_set_num_1, i_bit_num_1);
                o_out_valid_0001 <= '1';
            else 
                o_member_0001 <= '0';
                o_out_valid_0001 <= '0';
            end if;

            if i_read_enable_0002 = '1' then 
                o_member_0002 <= s_one_array_q(i_set_num_1, i_bit_num_1);
                o_out_valid_0002 <= '1';
            else 
                o_member_0002 <= '0';
                o_out_valid_0002 <= '0';
            end if;

            if i_read_enable_0003 = '1' then 
                o_member_0003 <= s_one_array_q(i_set_num_1, i_bit_num_1);
                o_out_valid_0003 <= '1';
            else 
                o_member_0003 <= '0';
                o_out_valid_0003 <= '0';
            end if;
        end if;
    end if;
end process;

------------------------------------------------------------------------------------------------------------------------
--                                                   Drive Outputs 
------------------------------------------------------------------------------------------------------------------------
o_collision_warning <= s_collision_warning;

end behavioral_PowerSet_0004_0004;




library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PowerSetGears.all;


------------------------------------------------------------------------------------------------------------------------
--                                        At Long Last: The Power Set Package 
------------------------------------------------------------------------------------------------------------------------
package PowerSetPkg is

component PowerSet_0004_0004 is
    generic (
        rst_level : std_logic := '1';
        rst_value : std_logic := '0';
        clk_edge : std_logic := '1';
        allow_fast_reset : std_logic := '1'
    );
    port (
        clk : in std_logic;
        rst : in std_logic;
        i_bit_num_1 : in integer;
        i_set_num_1 : in integer;
        i_add : in std_logic;
        i_rem : in std_logic;
        i_set_num_2 : in integer;
        i_dest_num : in integer;
        i_union : in std_logic;
        i_intersection : in std_logic;        
        i_complement : in std_logic; 
        i_duplicate : in std_logic;
        

        --------------------------------------------------------------------------------
        -- Read Port 0000
        --------------------------------------------------------------------------------
        i_read_set_num_0000 : in integer;
        i_read_bit_num_0000 : in integer;
        i_read_enable_0000 : in std_logic;
        o_member_0000 : out std_logic;
        o_out_valid_0000 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0001
        --------------------------------------------------------------------------------
        i_read_set_num_0001 : in integer;
        i_read_bit_num_0001 : in integer;
        i_read_enable_0001 : in std_logic;
        o_member_0001 : out std_logic;
        o_out_valid_0001 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0002
        --------------------------------------------------------------------------------
        i_read_set_num_0002 : in integer;
        i_read_bit_num_0002 : in integer;
        i_read_enable_0002 : in std_logic;
        o_member_0002 : out std_logic;
        o_out_valid_0002 : out std_logic;

        --------------------------------------------------------------------------------
        -- Read Port 0003
        --------------------------------------------------------------------------------
        i_read_set_num_0003 : in integer;
        i_read_bit_num_0003 : in integer;
        i_read_enable_0003 : in std_logic;
        o_member_0003 : out std_logic;
        o_out_valid_0003 : out std_logic;
        
        o_collision_warning : out std_logic 
    );
end component;


end package PowerSetPkg;


package body PowerSetPkg is
end package body PowerSetPkg;
