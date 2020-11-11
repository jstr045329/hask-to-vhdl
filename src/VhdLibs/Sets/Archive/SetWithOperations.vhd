----------------------------------------------------------------------------------------------------
-- This file uses a 1-bit wide RAM to implement a set.
-- A set is an unordered data structure with 3 operations:
--      1) Add element
--      2) Remove element
--      3) Check membership of an element
-- 
-- This implementation also supports unions and intersections between sets that cannot 
-- be implemented (efficiently) in BigSet.vhd.
--
--      column_3 <= column_1 or column_2; -- union between 2 sets
--      column_3 <= column_1 and column_2; -- intersection between 2 sets
-- 
-- in all 65536 rows of a 16 address bit RAM in 1 clock cycle.
-- 
-- In BigSet.vhd, I note that you can make a smaller set that supports unions and intersections.
-- This is an example of such a circuit. 
-- 
-- Notes:
--
--      1)  The synthesizer will not be able to use BRAMs to implement this circuit. 
--          This circuit will almost certainly be made out of fabric. 
--
--      2)  This circuit can get quite greedy when it comes to fabric. That is the price
--          you pay for fast set operations. How many CPUs give you the intersection of 
--          two sets in a single cycle?
--
--      3)  The compiler and/or synthesizer will probably warn you that 2- and 3-dimensional 
--          arrays are not supported in RAM. That's the synthesizer's way of saying points 1
--          and 2, above.
--
-- Here is how to think about this circuit:
--
--      1)  Each column in s_my_set is a separate set. You can add, remove or check membership
--          on each of these sets independently. 
--
--      2)  When you use i_din_1, i_din_2, i_check_membership_1, or i_check_membership_2, 
--          you are operating on one column. 
--
--      3)  When you use i_union and i_intersection, you are performing operations on columns, 
--          and storing the result in i_result_set.
--
-- In all cases, you must use i_operand_1 and i_operand_2 to tell the set which columns you are 
-- interested in. 
-- 
-- This circuit is a bit unorthodox. Extensive testing in hardware is recommended before deploying
-- this to anything.
--
----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.VhdSynthToolsPkg.all;


entity SetWithOperations is 
    generic(
        addr_bits : integer := 6;
        column_bits : integer := 6;
        allow_fast_reset : std_logic := '1'
    );
    port(
        clk : in std_logic;
        rst : in std_logic := '0'; 
        i_din_1 : in unsigned(addr_bits-1 downto 0);
        i_din_2 : in unsigned(addr_bits-1 downto 0);
        i_operand_1 : in unsigned(column_bits-1 downto 0);
        i_operand_2 : in unsigned(column_bits-1 downto 0);
        i_result_set : in unsigned(column_bits-1 downto 0);
        i_union : in std_logic;
        i_intersection : in std_logic;
        i_add_1 : in std_logic;
        i_remove_1 : in std_logic;
        i_check_membership_1 : in std_logic;
        i_check_membership_2 : in std_logic;
        o_operand_1 : out 
        o_is_member_1 : out std_logic;
        o_out_valid_1 : out std_logic;
        o_is_member_2 : out std_logic;
        o_out_valid_2 : out std_logic
    );
end SetWithOperations;

architecture behavioral_SetWithOperations of SetWithOperations is 

constant num_rows : integer := 2 ** addr_bits;
constant num_cols : integer := 2 ** column_bits;
type t_my_set is array (0 to num_rows-1) of std_logic_vector;
signal s_my_set : t_my_set;
signal s_address_int_1 : integer range 0 to num_rows-1;
signal s_address_int_2 : integer range 0 to num_rows-1;
signal s_operand_int_1 : integer range 0 to column_bits-1;
signal s_operand_int_2 : integer range 0 to column_bits-1;
signal s_result_int : integer range 0 to column_bits-1;

-- Grabs 1 column from s_my_set, and returns it as a std_logic_vector:
function grab_column(
    someSig : t_my_set;
    whichCol : integer
    ) return std_logic_vector is 
variable oneRow : std_logic_vector;
variable y : std_logic_vector(num_rows-1 downto 0);
begin 
    for i in 0 to num_rows-1 loop 
        oneRow := someSig(i);
        y(i) := oneRow(whichCol);
    end loop;
    return y;
end function;





-- TODO: PICK UP HERE: Replace this with a generate statement. 
-- Then instantiate in generate statement. 






-- Takes a new column, and assigns it to t_my_set:
function write_column(
    someSet : t_my_set;
    someSig : std_logic_vector(num_rows-1 downto 0); -- Represents 1 column
    whichCol : integer
    ) return t_my_set is 
variable oneRow : std_logic_vector;
variable y : t_my_set;
begin
    y := someSet;
    for i in 0 to num_rows-1 loop 
        oneRow := someSet(i);
        oneRow(whichCol) := someSig(i);
        y(i) := oneRow;
    end loop;
    return y;
end function;


begin 


s_address_int_1 <= slv2IntRstP(i_din_1, rst);
s_address_int_2 <= slv2IntRstP(i_din_2, rst);
s_operand_int_1 <= slv2IntRstP(i_operand_1, rst);
s_operand_int_2 <= slv2IntRstP(i_operand_2, rst);
s_result_int <= slv2IntRstP(i_result_set, rst);

process(clk)
    -- For intersections & unions:
    variable one_row : std_logic_vector(num_cols-1 downto 0);
    
    -- For operations on a single set:
    variable column_1 : std_logic_vector(num_rows-1 downto 0);
    variable column_2 : std_logic_vector(num_rows-1 downto 0);
begin
    if rising_edge(clk) then 
        if rst = '1' then 
            if allow_fast_reset = '1' then 
                s_my_set <= (others => (others => '0'));
            end if;
            o_is_member_1 <= '0';
            o_is_member_2 <= '0';
            o_out_valid_1 <= '0';
            o_out_valid_2 <= '0';
        else
            if ((not i_add_1) and (not i_remove_1) and (not i_check_membership_1) and (not i_check_membership_2)) = '1' then 
            
                -- If add, remove, and check membership lines are all 0's, allow unions & intersections:
                if i_union = '1' then 
                    for i in 0 to num_cols-1 loop 
                        one_row := s_my_set(i);
                        one_row(s_result_int) := one_row(s_operand_int_1) or one_row(s_operand_int_2);
                        s_my_set(i) := one_row;
                    end loop;
                    
                elsif i_intersection = '1' then 
                    for i in 0 to num_cols-1 loop 
                        one_row := s_my_set(i);
                        one_row(s_result_int) := one_row(s_operand_int_1) and one_row(s_operand_int_2);
                        s_my_set(i) := one_row;
                    end loop;
                end if;
                
            else 
                -- Implment stuff from BigRam.vhd here:
                if i_add_1 = '1' then 
                    s_my_set(s_address_int_1) <= '1';
                    o_is_member_1 <= '0';
                    out_valid <= '0';
                elsif i_remove_1 = '1' then 
                    s_my_set(s_address_int_1) <= '0';
                    o_is_member_1 <= '0';
                    out_valid <= '0';
                elsif i_check_membership_1 <= '1' then 
                    o_is_member_1 <= s_my_set(s_address_int_1);
                    out_valid <= '1';
                end if;
                
                -- Port 2 is read only:
                if i_check_membership_2 = '1' then 
                    o_is_member_2 <= s_my_set(s_address_int_2);
                    o_out_valid_2 <= '1';
                else
                    o_is_member_2 <= '0';
                    o_out_valid_2 <= '0';
                end if;
            end if;
        end if;
    end if;
end process;

end behavioral_SetWithOperations;
