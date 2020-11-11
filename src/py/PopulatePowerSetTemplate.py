"""This script populates PowerSetTemplate.vhd"""
INPUT_FILE_ENTITY = "../VhdTemplates/PowerSetTemplate.vhd"
INPUT_FILE_COMPONENT = "../VhdTemplates/PowerSetComponentTemplate.vhd"
INPUT_FILE_PACKAGE = "../VhdTemplates/PowerSetPkgTemplate.vhd"
OUTPUT_FILE_PACKAGE = "../VhdLibs/Sets/PowerSetPkg.vhd"

SQUARE_ARRAY = 4

# If you want all 4 parameters the same, you can set them all to SQUARE_ARRAY:
MIN_ROW_BITS = SQUARE_ARRAY
MAX_ROW_BITS = SQUARE_ARRAY
MIN_COL_BITS = SQUARE_ARRAY
MAX_COL_BITS = SQUARE_ARRAY
NUM_READ_PORTS = 4

assert(MAX_ROW_BITS >= MIN_ROW_BITS)
assert(MAX_COL_BITS >= MIN_COL_BITS)

FORMAT_STR = "%04d"
from vhdPyTools import *


one_port_map = """BIT_INSTANCE_<row_num_here>_<col_num_here>: OneSetBit 
    generic map (
        my_col => <col_num_here>, 
        my_row => <row_num_here>,
        rst_level => rst_level,
        rst_value => rst_value,
        clk_edge => clk_edge,
        num_rows => <num_rows_here>,
        num_cols => <num_cols_here>
    )
    port map (
        clk => clk, 
        rst => s_global_fast_reset,
        i_row => i_bit_num_1,
        i_col => i_set_num_1,
        i_add => i_add, 
        i_rem => i_rem, 
        i_din => s_result(<row_num_here>),
        i_we => s_global_write_enable(<col_num_here>),
        o_member => s_one_array_d(<row_num_here>, <col_num_here>)
    );
"""


def writeOnePortMap(rowNum, colNum, totalRows, totalColumns):
    s = one_port_map
    s = s.replace("<row_num_here>", FORMAT_STR % rowNum)
    s = s.replace("<col_num_here>", FORMAT_STR % colNum)
    s = s.replace("<num_rows_here>", FORMAT_STR % totalRows)
    s = s.replace("<num_cols_here>", FORMAT_STR % totalColumns)
    return s


one_read_port = """
        --------------------------------------------------------------------------------
        -- Read Port <port_num_here>
        --------------------------------------------------------------------------------
        i_read_set_num_<port_num_here> : in integer;
        i_read_bit_num_<port_num_here> : in integer;
        i_read_enable_<port_num_here> : in std_logic;
        o_member_<port_num_here> : out std_logic;
        o_out_valid_<port_num_here> : out std_logic;
"""


def declareOneReadPort(portNum):
    s = one_read_port
    s = s.replace("<port_num_here>", FORMAT_STR % portNum)
    return s


one_read_reset_value = """
            o_member_<port_num_here> <= '0';
            o_out_valid_<port_num_here> <= '0';
"""


def resetOneReadPort(portNum):
    s = one_read_reset_value
    s = s.replace("<port_num_here>", FORMAT_STR % portNum)
    return s


one_read_driver = """
            if i_read_enable_<port_num_here> = '1' then 
                o_member_<port_num_here> <= s_one_array_q(i_set_num_1, i_bit_num_1);
                o_out_valid_<port_num_here> <= '1';
            else 
                o_member_<port_num_here> <= '0';
                o_out_valid_<port_num_here> <= '0';
            end if;"""


def driveOneReadPort(portNum):
    s = one_read_driver
    s = s.replace("<port_num_here>", FORMAT_STR % portNum)
    return s 


def populateTemplateOnce(num_row_bits, num_col_bits, num_read_ports):
    num_rows = 2 ** num_row_bits
    num_cols = 2 ** num_col_bits
    y = []
    with open(INPUT_FILE_ENTITY, 'r') as f:
        input_lines = f.readlines()
    for one_line in input_lines:
        s = one_line
        s = s.replace("<num_rows_here>", FORMAT_STR % num_rows)
        s = s.replace("<num_cols_here>", FORMAT_STR % num_cols)
        s = s.replace("<row_bits_here>", FORMAT_STR % num_row_bits)
        s = s.replace("<col_bits_here>", FORMAT_STR % num_col_bits)
        if "<read_ports_here>" in s:
            for readPortNum in range(num_read_ports):
                y.append(declareOneReadPort(readPortNum))
        
        elif "<instances_here>" in s:
            for colNum in range(num_cols):
                for rowNum in range(num_rows):
                    y.append(writeOnePortMap(rowNum, colNum, num_rows, num_cols))
                    y.append("\n")
        
        elif "<reset_read_outputs_here>" in s:
            for readPortNum in range(num_read_ports):
                y.append(resetOneReadPort(readPortNum))
                
        elif "<drive_read_outputs_here>" in s:
            for readPortNum in range(num_read_ports):
                y.append(driveOneReadPort(readPortNum))
                
        elif "TODO:" in s:
            pass
                            
        else:
            y.append(s)
    return y


def populateComponentOnce(num_rows, num_cols, num_read_ports):
    y = []
    with open(INPUT_FILE_COMPONENT, 'r') as f:
        lines = f.readlines()
    for one_line in lines:
        s = one_line
        s = s.replace("<num_rows_here>", FORMAT_STR % num_rows)
        s = s.replace("<num_cols_here>", FORMAT_STR % num_cols)
        if "<read_ports_here>" in s:
            for readPortNum in range(num_read_ports):
                y.append(declareOneReadPort(readPortNum))
        else:
            y.append(s)    
    return y
    

def main():
    entity_definitions = []
    entity_definitions.extend(machineGeneratedFileWarning("PopulatePowerSetTemplate.py", []))
    entity_definitions.append("")
    for row_bits in range(MIN_ROW_BITS, MAX_ROW_BITS+1):
        for col_bits in range(MIN_COL_BITS, MAX_COL_BITS+1):
            entity_definitions.extend(populateTemplateOnce(row_bits, col_bits, NUM_READ_PORTS))
            entity_definitions.append("")
            entity_definitions.append("")

    component_decs = []
    for row_bits in range(MIN_ROW_BITS, MAX_ROW_BITS+1):
        for col_bits in range(MIN_COL_BITS, MAX_COL_BITS+1):
            component_decs.extend(populateComponentOnce(2**row_bits, 2**col_bits, NUM_READ_PORTS))
            component_decs.append("")

    output_lines = []
    with open(INPUT_FILE_PACKAGE, 'r') as f:
        pkgTemplateLines = f.readlines()
    
    for line in pkgTemplateLines:
        if "<entities_here>" in line:
            output_lines.extend(entity_definitions)
        elif "<component_declarations_here>" in line:
            output_lines.extend(component_decs)
        else:
            output_lines.append(line)
    
    with open(OUTPUT_FILE_PACKAGE, 'w') as f:
        for line in output_lines:
            f.write(enforceOneLineEnding(line))

if __name__ == "__main__":
    main()

