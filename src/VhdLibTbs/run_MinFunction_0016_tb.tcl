vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
# vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSynthToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/MinFunction_0016.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibTbs/MinFunction_0016_tb.vhd

vsim work.MinFunction_0016_tb -t ns

add wave -position insertpoint  \
    sim:/MinFunction_0016_tb/clk \
    sim:/MinFunction_0016_tb/rst \
    sim:/MinFunction_0016_tb/i_distance_0000 \
    sim:/MinFunction_0016_tb/i_distance_0001 \
    sim:/MinFunction_0016_tb/i_distance_0002 \
    sim:/MinFunction_0016_tb/i_distance_0003 \
    sim:/MinFunction_0016_tb/i_distance_0004 \
    sim:/MinFunction_0016_tb/i_distance_0005 \
    sim:/MinFunction_0016_tb/i_distance_0006 \
    sim:/MinFunction_0016_tb/i_distance_0007 \
    sim:/MinFunction_0016_tb/i_distance_0008 \
    sim:/MinFunction_0016_tb/i_distance_0009 \
    sim:/MinFunction_0016_tb/i_distance_0010 \
    sim:/MinFunction_0016_tb/i_distance_0011 \
    sim:/MinFunction_0016_tb/i_distance_0012 \
    sim:/MinFunction_0016_tb/i_distance_0013 \
    sim:/MinFunction_0016_tb/i_distance_0014 \
    sim:/MinFunction_0016_tb/i_distance_0015 \
    sim:/MinFunction_0016_tb/o_min \
    sim:/MinFunction_0016_tb/correct_answer \

run -all
