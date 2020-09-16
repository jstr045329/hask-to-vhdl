library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;


package VhdSynthToolsPkg is

----------------------------------------------------------------------------------------------------
--                                  One Delay for Std Logic
----------------------------------------------------------------------------------------------------
component delayStdLogic is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );  
end component;


----------------------------------------------------------------------------------------------------
--                                One Delay for Std Logic Vector
----------------------------------------------------------------------------------------------------
component delayStdLogicVector is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic_vector(w-1 downto 0);
        q : out std_logic_vector(w-1 downto 0)
    );
end component;

----------------------------------------------------------------------------------------------------
--                                   One Delay for Signed
----------------------------------------------------------------------------------------------------
component delaySigned is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in signed(w-1 downto 0);
        q : out signed(w-1 downto 0)
    );
end component;

----------------------------------------------------------------------------------------------------
--                                   One Delay for Unsigned
----------------------------------------------------------------------------------------------------
component delayUnsigned is
    generic(w : integer);
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in unsigned(w-1 downto 0);
        q : out unsigned(w-1 downto 0)
    );
end component;

----------------------------------------------------------------------------------------------------
--                                One Delay for Integer
----------------------------------------------------------------------------------------------------
component delayInteger is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in integer;
        q : out integer
    );
end component;


----------------------------------------------------------------------------------------------------
--                                  Delay Chain for Std Logic
----------------------------------------------------------------------------------------------------
component delayChainStdLogic is
    generic(
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );
end component;

----------------------------------------------------------------------------------------------------
--                               Delay Chain for Std Logic Vector
----------------------------------------------------------------------------------------------------
component delayChainStdLogicVector is  
    generic(
        w : integer;
        n : integer
    );  
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic_vector(w-1 downto 0); 
        q : out std_logic_vector(w-1 downto 0)
    );  
end component;


----------------------------------------------------------------------------------------------------
--                                Delay Chain for Signed
----------------------------------------------------------------------------------------------------
component delayChainSigned is
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in signed(w-1 downto 0);
        q : out signed(w-1 downto 0)
    );
end component;

----------------------------------------------------------------------------------------------------
--                                Delay Chain for Unigned
----------------------------------------------------------------------------------------------------
component delayChainUnsigned is
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in unsigned(w-1 downto 0);
        q : out unsigned(w-1 downto 0)
    );
end component;


----------------------------------------------------------------------------------------------------
--                                  Delay Chain for Integer
----------------------------------------------------------------------------------------------------
component delayChainInteger is
    generic(
        w : integer;
        n : integer
    );
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in integer;
        q : out integer
    );
end component;


----------------------------------------------------------------------------------------------------
--                                    Rising Edge Detector
----------------------------------------------------------------------------------------------------
component detectRisingEdge is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );  
end component detectRisingEdge;


----------------------------------------------------------------------------------------------------
--                                    Falling Edge Detector
----------------------------------------------------------------------------------------------------
component detectFallingEdge is
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic := '1';
        d : in std_logic;
        q : out std_logic
    );  
end component detectFallingEdge ;

----------------------------------------------------------------------------------------------------
--                                       Sticky Bits
----------------------------------------------------------------------------------------------------
component StickyWarning is 
    generic( 
        -- counter_width and clks_high specify how long soft_sticky stays asserted.
        -- hard_sticky stays asserted until hardware reset. 
        counter_width : integer := 8; 
        clks_high : integer := 128
    ); 
    port(
        clk : in std_logic;
        rst : in std_logic;
        en : in std_logic;
        trigger : in std_logic;
        soft_sticky : out std_logic;
        hard_sticky : out std_logic
    ); 
end component;

----------------------------------------------------------------------------------------------------
--                                  Convert Boolean to std_logic
----------------------------------------------------------------------------------------------------
function bool2SL(
    x : boolean
    ) return std_logic;


----------------------------------------------------------------------------------------------------
--                             Perform Arithmetic on std_logic_vector's
----------------------------------------------------------------------------------------------------
-- Treat std_logic_vector as unsigned, add 1, and cast back to std_logic_vector.
function incrementSlv (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector;


-- Treat std_logic_vector as unsigned, subtract 1, and cast back to std_logic_vector.
function decrementSlv (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector;


-- Treat std_logic_vector as unsigned, add 1, and cast back to std_logic_vector.
-- Do not allow overflow.
function incrementSlvSaturating (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector;


-- Treat std_logic_vector as unsigned, subtract 1, and cast back to std_logic_vector.
-- Do not allow underflow.
function decrementSlvSaturating (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector;


-- Calculate x - y
-- 1) Regards x as unsigned, 
-- 2) Converts y to unsigned, 
-- 3) Calculates x - y,
-- 4) Casts result back to std_logic_vector.
function subtractFromSlv(
    x : std_logic_vector;
    w : integer;
    y : integer) return std_logic_vector;


function subtractFromSlv(
    x : unsigned;
    w : integer;
    y : integer) return std_logic_vector;

-- Regard std_logic_vector as unsigned, and set LSB to 1. 
function setSlvToOne(
    w : integer
    ) return std_logic_vector;


end package VhdSynthToolsPkg ;


----------------------------------------------------------------------------------------------------
--                                        Package Body
----------------------------------------------------------------------------------------------------
package body VhdSynthToolsPkg is

----------------------------------------------------------------------------------------------------
--                                  Convert Boolean to std_logic
----------------------------------------------------------------------------------------------------
function bool2SL(
    x : boolean
    ) return std_logic is 
begin 
    if x then 
        return '1';
    else
        return '0';
    end if;
end function;


----------------------------------------------------------------------------------------------------
--                             Perform Arithmetic on std_logic_vector's
----------------------------------------------------------------------------------------------------

-- This function treats a std_logic_vector as unsigned, adds 1, then casts it back to std_logic_vector.
-- w is the width of x.
function incrementSlv (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector is
begin
    return std_logic_vector(to_unsigned(to_integer(unsigned(x)) + 1, w));
end function;


-- This function treats a std_logic_vector as unsigned, subtracts 1, then casts it back to std_logic_vector.
-- w is the width of x.
function decrementSlv (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector is
begin
    return std_logic_vector(to_unsigned(to_integer(unsigned(x)) - 1, w));
end function;

-- This function treats a std_logic_vector as unsigned, adds 1, then casts it back to std_logic_vector.
-- w is the width of x.
-- This version does not allow x to overflow. 
function incrementSlvSaturating (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector is
begin
    if to_integer(unsigned(x)) = (2**w)-1 then 
        return x;
    else
        return std_logic_vector(to_unsigned(to_integer(unsigned(x)) + 1, w));
    end if;
end function;


-- This function treats a std_logic_vector as unsigned, subtracts 1, then casts it back to std_logic_vector.
-- w is the width of x.
-- This version does not allow x to underflow. 
function decrementSlvSaturating (
    x : std_logic_vector;
    w : integer
    ) return std_logic_vector is
begin
    if to_integer(unsigned(x)) = 0 then 
        return x;
    else
        return std_logic_vector(to_unsigned(to_integer(unsigned(x)) - 1, w));
    end if;
end function;


-- TODO: Create versions of x-y for addition, and then saturating versions. 

-- Calculate x - y
-- 1) Regards x as unsigned, 
-- 2) Converts y to unsigned, 
-- 3) Calculates x - y,
-- 4) Casts result back to std_logic_vector.
function subtractFromSlv(
    x : std_logic_vector;
    w : integer;
    y : integer) return std_logic_vector is
variable tmp0 : integer;
variable tmp1 : unsigned(w-1 downto 0);
begin
    tmp0 := to_integer(unsigned(x));
    tmp1 := tmp0 - to_unsigned(y, w);
    return std_logic_vector(tmp1);
end function;


function subtractFromSlv(
    x : unsigned;
    w : integer;
    y : integer) return std_logic_vector is
begin
    return subtractFromSlv(std_logic_vector(x), w, y);
end function;

-- Regard std_logic_vector as unsigned, and set LSB to 1. 
function setSlvToOne(
    w : integer
    ) return std_logic_vector is 
begin
    return std_logic_vector(to_unsigned(1, w));
end function;

end package body VhdSynthToolsPkg ;

