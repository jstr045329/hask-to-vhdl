library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package BitPaddingTools is 

----------------------------------------------------------------------------------------------------
--                                Return A Vector of Zeros or Ones
----------------------------------------------------------------------------------------------------
function zeros(
    n : integer 
    ) return std_logic_vector;


function zeros(
    n : integer 
    ) return unsigned;


function zeros(
    n : integer 
    ) return signed;


function ones(
    n : integer 
    ) return std_logic_vector;


function ones(
    n : integer 
    ) return unsigned;


function ones(
    n : integer 
    ) return signed;


----------------------------------------------------------------------------------------------------
--                                  Left Justify a Vector
----------------------------------------------------------------------------------------------------
function leftJustifyZeros(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector;


function leftJustifyZeros(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned;


function leftJustifyZeros(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed;
    

function leftJustifyOnes(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector;


function leftJustifyOnes(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned;


function leftJustifyOnes(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed;


----------------------------------------------------------------------------------------------------
--                                  Right Justify a Vector
----------------------------------------------------------------------------------------------------
function rightJustifyZeros(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector;


function rightJustifyZeros(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned;


function rightJustifyZeros(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed;
    

function rightJustifyOnes(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector;


function rightJustifyOnes(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned;


function rightJustifyOnes(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed;


end package BitPaddingTools;


package body BitPaddingTools is 

----------------------------------------------------------------------------------------------------
--                                Return A Vector of Zeros or Ones
----------------------------------------------------------------------------------------------------
function zeros(
    n : integer 
    ) return std_logic_vector is 
variable tmp : std_logic_vector(n-1 downto 0) := (others => '0');
begin 
    return tmp;
end function;


function zeros(
    n : integer 
    ) return unsigned is 
variable tmp : unsigned(n-1 downto 0) := (others => '0');
begin 
    return tmp;
end function;


function zeros(
    n : integer 
    ) return signed is 
variable tmp : signed(n-1 downto 0) := (others => '0');
begin 
    return tmp;
end function;


function ones(
    n : integer 
    ) return std_logic_vector is 
variable tmp : std_logic_vector(n-1 downto 0) := (others => '1');
begin 
    return tmp;
end function;


function ones(
    n : integer 
    ) return unsigned is 
variable tmp : unsigned(n-1 downto 0) := (others => '1');
begin 
    return tmp;
end function;


function ones(
    n : integer 
    ) return signed is 
variable tmp : signed(n-1 downto 0) := (others => '1');
begin 
    return tmp;
end function;

----------------------------------------------------------------------------------------------------
--                                  Left Justify a Vector
----------------------------------------------------------------------------------------------------
function leftJustifyZeros(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & zeros(y_width - x_width);
end function;


function leftJustifyZeros(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & zeros(y_width - x_width);
end function;


function leftJustifyZeros(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & zeros(y_width - x_width);
end function;
    

function leftJustifyOnes(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & ones(y_width - x_width);
end function;


function leftJustifyOnes(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & ones(y_width - x_width);
end function;


function leftJustifyOnes(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return x & ones(y_width - x_width);
end function;



----------------------------------------------------------------------------------------------------
--                                  Right Justify a Vector
----------------------------------------------------------------------------------------------------
function rightJustifyZeros(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return zeros(y_width - x_width) & x;
end function;


function rightJustifyZeros(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return zeros(y_width - x_width) & x;
end function;


function rightJustifyZeros(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return zeros(y_width - x_width) & x;
end function;
    

function rightJustifyOnes(
    x : std_logic_vector;
    x_width : integer;
    y_width : integer
    ) return std_logic_vector is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return ones(y_width - x_width) & x;
end function;


function rightJustifyOnes(
    x : unsigned;
    x_width : integer;
    y_width : integer
    ) return unsigned is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return ones(y_width - x_width) & x;
end function;


function rightJustifyOnes(
    x : signed;
    x_width : integer;
    y_width : integer
    ) return signed is 
begin
    if x_width >= y_width then 
        report "x_width should always be <= y_width. Strange behavioral may result." severity error;
    end if;
    return ones(y_width - x_width) & x;
end function;


end package body BitPaddingTools;

