component PowerSet_<num_rows_here>_<num_cols_here> is
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
        
        -- <read_ports_here>
        
        o_collision_warning : out std_logic 
    );
end component;