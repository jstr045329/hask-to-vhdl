vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSynthToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/RegisteredGatesPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetOrGate.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetLowLayerNode.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetModule.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibTbs/ArbSetModule_tb.vhd

vsim work.ArbSetModule_tb -t ns

add wave -position insertpoint -radix hexadecimal \
    sim:/ArbSetModule_tb/clk \
    sim:/ArbSetModule_tb/rst\
    sim:/ArbSetModule_tb/s_number_to_add \
    sim:/ArbSetModule_tb/s_add_enable \
    sim:/ArbSetModule_tb/s_number_to_remove \
    sim:/ArbSetModule_tb/s_remove_enable \
    sim:/ArbSetModule_tb/s_number_to_test \
    sim:/ArbSetModule_tb/s_test_enable \
    sim:/ArbSetModule_tb/s_occupied \
    sim:/ArbSetModule_tb/s_membership \
    sim:/arbsetmodule_tb/UUT/s_membership \
    sim:/arbsetmodule_tb/UUT/o_membership 

run -all
