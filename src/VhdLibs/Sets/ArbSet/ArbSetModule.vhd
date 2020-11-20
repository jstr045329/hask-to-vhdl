------------------------------------------------------------------------------------------------------------------------
--                                               Arbitrary Set Module 
--
-- This set stores arbitrary 64-bit integers. This module stitches together many instances of ArbSetLowLayerNode. 
--
------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.ALL;
use work.PrefixGatesPkg.all;
use work.VhdSynthToolsPkg.all;
use work.ArbSetPkg.all;
use work.RegisteredGatesPkg.all;


entity ArbSetModule is
    port(
        clk : in std_logic;
        rst : in std_logic;
        i_din : in std_logic_vector(NUMBER_WIDTH-1 downto 0);
        i_add_enable : in std_logic;
        i_remove_enable : in std_logic;
        i_test_enable : in std_logic;
        o_occupied : out std_logic;
        o_membership : out std_logic 
   );
end ArbSetModule;


architecture behavioral_ArbSetModule of ArbSetModule is

constant num_nodes : integer := 1024;
signal s_predecessor_occupied : std_logic_vector(num_nodes-1 downto 0);
signal s_membership : std_logic_vector(num_nodes-1 downto 0);
signal s_overall_membership : std_logic;

------------------------------------------------------------------------------------------------------------------------
--                                                      Begin 
------------------------------------------------------------------------------------------------------------------------
begin

------------------------------------------------------------------------------------------------------------------------
--                                                    Node Array 
------------------------------------------------------------------------------------------------------------------------
NODE_0: ArbSetLowLayerNode
    port map (
        clk => clk,
        rst => rst,
        i_din => i_din,
        i_add_enable => i_add_enable,
        i_remove_enable => i_remove_enable,
        i_test_enable => i_test_enable,
        i_predecessor_occupied => '1',
        o_number_stored => open,
        o_occupied => s_predecessor_occupied(0),
        o_membership => s_membership(0)
    );
    
NODE_ARRAY: for i in 1 to num_nodes-1 generate 
    NODE_INST: ArbSetLowLayerNode
        port map (
            clk => clk,
            rst => rst,
            i_din => i_din,
            i_add_enable => i_add_enable,
            i_remove_enable => i_remove_enable,
            i_test_enable => i_test_enable,
            i_predecessor_occupied => s_predecessor_occupied(i-1),
            o_number_stored => open,
            o_occupied => s_predecessor_occupied(i),
            o_membership => s_membership(i)
        );
end generate;

------------------------------------------------------------------------------------------------------------------------
--                                      Logically OR Membership Tests Together 
------------------------------------------------------------------------------------------------------------------------
BIG_OR_GATE: ArbSetOrGate 
    port map (
        clk => clk, 
        rst => rst, 
        intermediate => s_membership,
        dout => s_overall_membership
    );


------------------------------------------------------------------------------------------------------------------------
--                                                   Drive Outputs 
------------------------------------------------------------------------------------------------------------------------
o_occupied <= s_predecessor_occupied(num_nodes-1);
o_membership <= s_overall_membership;

end behavioral_ArbSetModule;

