vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSynthToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/Sets/ArbSet/ArbSetLowLayerNode.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibTbs/ArbSetLowLayerNode_tb.vhd

vsim work.ArbSetLowLayerNode_tb -t ns

add wave -position insertpoint -radix hexadecimal \
    sim:/ArbSetLowLayerNode_tb/clk \
    sim:/ArbSetLowLayerNode_tb/rst\
    sim:/ArbSetLowLayerNode_tb/s_number_to_add \
    sim:/ArbSetLowLayerNode_tb/s_add_enable \
    sim:/ArbSetLowLayerNode_tb/s_number_to_remove \
    sim:/ArbSetLowLayerNode_tb/s_remove_enable \
    sim:/ArbSetLowLayerNode_tb/s_number_to_test \
    sim:/ArbSetLowLayerNode_tb/s_test_enable \
    sim:/ArbSetLowLayerNode_tb/s_predecessor_occupied \
    sim:/ArbSetLowLayerNode_tb/s_number_stored \
    sim:/ArbSetLowLayerNode_tb/s_occupied \
    sim:/ArbSetLowLayerNode_tb/s_membership 

run -all
