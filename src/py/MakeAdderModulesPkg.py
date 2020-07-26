# Takes the template for counting bits and populates reset name & reset level.
from os import linesep as eol
INPUT_FILENAME = "../VhdTemplates/OneAdderTemplate.vhd"
OUTPUT_FILENAME = "../VhdLibs/AdderModulesPkg.vhd"
ENTITY_STUB = "AddTwoNumbers_%04d_%04d"
BITS_IN_START = 2
BITS_IN_STOP = 16

USE_POSITIVE_RESET = True


def rst_name():
    if USE_POSITIVE_RESET:
        return "reset"
    return "reset_n"


def rst_level():
    if USE_POSITIVE_RESET:
        return "'1'"
    return "'0'"


def one_entity_name(bitsIn, bitsOut):
    return ENTITY_STUB % (bitsIn, bitsOut,)


def test_rst_statement():
    return "if " + rst_name() + " = " + rst_level() + " then"


def convert_entity_to_component(one_entity_declaration):
    y = []
    for line in one_entity_declaration:
        s = line
        s = s.replace("entity", "component")
        y.append(s)
    y.append(""+eol)
    return y


def generate_pkg(lolos):
    "lolos is a list of list of strings"
    y = []
    y.append("----------------------------------------------------------------------------------------------------"+eol)
    y.append("--                                    Adder Module Package"+eol)
    y.append("----------------------------------------------------------------------------------------------------"+eol)
    y.append("library ieee;"+eol)
    y.append("use ieee.std_logic_1164.all;"+eol)
    y.append("use ieee.numeric_std.all;"+eol)
    y.append(""+eol)
    y.append(""+eol)
    y.append("package AdderModulePkg is"+eol)
    for los in lolos:
        y.extend(convert_entity_to_component(los))
    y.append(""+eol)
    y.append("end package AdderModulePkg;"+eol)
    y.append(""+eol)
    y.append(""+eol)
    y.append("package body AdderModulePkg is"+eol)
    y.append("end package body AdderModulePkg;"+eol)
    y.append(""+eol)
    return y

def main():
    entity_declarations = [] # list of list of strings
    y = [] # list of strings
    with open(INPUT_FILENAME, 'r') as f:
        los = f.readlines()
    for bits_in in range(BITS_IN_START, BITS_IN_STOP+1):
        bits_out = bits_in + 1
        one_entity_declaration = []
        record_entity = False
        for line in los:
            include_line = True
            if "-- <entity_declaration_start>" in line:
                include_line = False
            if "-- <entity_declaration_stop>" in line:
                include_line = False
            s = line 
            s = s.replace("-- <entity_declaration_start>", "")
            s = s.replace("-- <entity_declaration_stop>", "")
            s = s.replace("<entity_name_here>", one_entity_name(bits_in, bits_in+1))
            s = s.replace("<reset_name_here>", rst_name())
            s = s.replace("<test_reset_here>", test_rst_statement())
            s = s.replace("<reset_level_here>", rst_level())
            s = s.replace("<bits_in_here>", str(bits_in))
            s = s.replace("<bits_out_here>", str(bits_out))
            s = s.replace("<bits_in_msb_here>", str(bits_in-1))
            s = s.replace("<bits_out_msb_here>", str(bits_out-1))
            s = s.replace(" - 1", "-1")
            if include_line:
                y.append(s)

            if include_line and record_entity:
                one_entity_declaration.append(s)

            if "-- <entity_declaration_start>" in line:
                one_entity_declaration = []
                record_entity = True

            if "-- <entity_declaration_stop>" in line:
                entity_declarations.append(one_entity_declaration)
                one_entity_declaration = []
                record_entity = False
    y.extend(generate_pkg(entity_declarations))
    with open(OUTPUT_FILENAME, 'w') as f:
        for line in y:
            f.write(line)

if __name__ == "__main__":
    main()

