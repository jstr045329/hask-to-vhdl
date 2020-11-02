vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSynthToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/PrefixGatesPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/PowerSetGears.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/OneSetBit.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/PowerSetPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibTbs/PowerSet_0016_0016_tb.vhd

vsim work.PowerSet_0016_0016_tb -t ns

add wave -position insertpoint  \
    sim:/PowerSet_0016_0016_tb/clk \
    sim:/PowerSet_0016_0016_tb/rst \
    sim:/PowerSet_0016_0016_tb/UUT/s_operand_1 \
    sim:/PowerSet_0016_0016_tb/UUT/s_operand_2 \
    sim:/PowerSet_0016_0016_tb/UUT/s_result \
    sim:/PowerSet_0016_0016_tb/UUT/s_global_write_enable \
    sim:/PowerSet_0016_0016_tb/UUT/s_one_array_d \
    sim:/PowerSet_0016_0016_tb/UUT/s_one_array_q \
    sim:/PowerSet_0016_0016_tb/s_add_000000 \
    sim:/PowerSet_0016_0016_tb/s_rem_000000 \
    sim:/PowerSet_0016_0016_tb/s_union_000000 \
    sim:/PowerSet_0016_0016_tb/s_intersection_000000 \
    sim:/PowerSet_0016_0016_tb/UUT/s_result \
    sim:/PowerSet_0016_0016_tb/UUT/s_global_write_enable \
    sim:/PowerSet_0016_0016_tb/test_stage

run -all
