library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.DijkstraParametersPkg.ALL;


package IntegerSetPkg is

constant INTEGER_SET_ROWS : integer := 2**INTEGER_SET_EXPONENT;
type t_integer_set is array(0 to INTEGER_SET_ROWS-1) of std_logic;

end package IntegerSetPkg;


package body IntegerSetPkg is
end package body IntegerSetPkg;
