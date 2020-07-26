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


end package VhdSynthToolsPkg ;


package body VhdSynthToolsPkg is
end package body VhdSynthToolsPkg ;

