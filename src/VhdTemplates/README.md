This directory contains VHDL templates. 

You do not want to use the .vhd files in this directory as-is, as 
they tend to contain tags like this:
    
    <entity_name_here>
    <input_0_here>

The templates in this directory have been used by Python to 
generate the VHDL libraries in the VhdLibs directory. Most users
should be able to include the files in VhdLibs in your FPGA project
and not need to worry about running Python. 

If you need a variant of something in the VhdLibs directory, 
adjust the parameters in the appropriate Python script (which
may include the output filename as well) and run that script to 
generate the desired library.

