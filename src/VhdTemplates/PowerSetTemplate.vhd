------------------------------------------------------------------------------------------------------------------------
--                     Power Set With <num_cols_here> Sets And <num_rows_here> Elements In Each Set 
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PowerSetGears.all;
use work.PrefixGatesPkg.all;


entity PowerSet_<num_rows_here>_<num_cols_here> is
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
        -- <read_ports_here>
        
        -- If both ports try to write to the same column at the same time, this warning goes hot. 
        -- Note that the potential for collisions is between i_set_num_1 and i_dest_num, not 
        -- i_set_num_1 and i_set_num_2.
        o_collision_warning : out std_logic 
    );
end PowerSet_<num_rows_here>_<num_cols_here>;


architecture behavioral_PowerSet_<num_rows_here>_<num_cols_here> of PowerSet_<num_rows_here>_<num_cols_here> is

constant ROW_BITS : integer := <row_bits_here>;
constant COL_BITS : integer := <col_bits_here>;

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

-- TODO: Test results in ModelSim and see what happens
-- TODO: Delete unnecessary files
-- TODO: Pipeline inputs/signals so that delays line up. 
-- TODO: Make async reset version.
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



-- <instances_here>


READ_PORT_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            -- <reset_read_outputs_here>
        else
            -- <drive_read_outputs_here>
        end if;
    end if;
end process;


end behavioral_PowerSet_<num_rows_here>_<num_cols_here>;

