vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/MinFunction_0064.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibTbs/MinFunction_0064_tb.vhd

vsim work.MinFunction_0064_tb -t ns

add wave -position insertpoint  \
    sim:/MinFunction_0064_tb/clk \
    sim:/MinFunction_0064_tb/rst \
    sim:/MinFunction_0064_tb/i_distance_0000 \
    sim:/MinFunction_0064_tb/i_distance_0001 \
    sim:/MinFunction_0064_tb/i_distance_0002 \
    sim:/MinFunction_0064_tb/i_distance_0003 \
    sim:/MinFunction_0064_tb/i_distance_0004 \
    sim:/MinFunction_0064_tb/i_distance_0005 \
    sim:/MinFunction_0064_tb/i_distance_0006 \
    sim:/MinFunction_0064_tb/i_distance_0007 \
    sim:/MinFunction_0064_tb/i_distance_0008 \
    sim:/MinFunction_0064_tb/i_distance_0009 \
    sim:/MinFunction_0064_tb/i_distance_0010 \
    sim:/MinFunction_0064_tb/i_distance_0011 \
    sim:/MinFunction_0064_tb/i_distance_0012 \
    sim:/MinFunction_0064_tb/i_distance_0013 \
    sim:/MinFunction_0064_tb/i_distance_0014 \
    sim:/MinFunction_0064_tb/i_distance_0015 \
    sim:/MinFunction_0064_tb/i_distance_0016 \
    sim:/MinFunction_0064_tb/i_distance_0017 \
    sim:/MinFunction_0064_tb/i_distance_0018 \
    sim:/MinFunction_0064_tb/i_distance_0019 \
    sim:/MinFunction_0064_tb/i_distance_0020 \
    sim:/MinFunction_0064_tb/i_distance_0021 \
    sim:/MinFunction_0064_tb/i_distance_0022 \
    sim:/MinFunction_0064_tb/i_distance_0023 \
    sim:/MinFunction_0064_tb/i_distance_0024 \
    sim:/MinFunction_0064_tb/i_distance_0025 \
    sim:/MinFunction_0064_tb/i_distance_0026 \
    sim:/MinFunction_0064_tb/i_distance_0027 \
    sim:/MinFunction_0064_tb/i_distance_0028 \
    sim:/MinFunction_0064_tb/i_distance_0029 \
    sim:/MinFunction_0064_tb/i_distance_0030 \
    sim:/MinFunction_0064_tb/i_distance_0031 \
    sim:/MinFunction_0064_tb/i_distance_0032 \
    sim:/MinFunction_0064_tb/i_distance_0033 \
    sim:/MinFunction_0064_tb/i_distance_0034 \
    sim:/MinFunction_0064_tb/i_distance_0035 \
    sim:/MinFunction_0064_tb/i_distance_0036 \
    sim:/MinFunction_0064_tb/i_distance_0037 \
    sim:/MinFunction_0064_tb/i_distance_0038 \
    sim:/MinFunction_0064_tb/i_distance_0039 \
    sim:/MinFunction_0064_tb/i_distance_0040 \
    sim:/MinFunction_0064_tb/i_distance_0041 \
    sim:/MinFunction_0064_tb/i_distance_0042 \
    sim:/MinFunction_0064_tb/i_distance_0043 \
    sim:/MinFunction_0064_tb/i_distance_0044 \
    sim:/MinFunction_0064_tb/i_distance_0045 \
    sim:/MinFunction_0064_tb/i_distance_0046 \
    sim:/MinFunction_0064_tb/i_distance_0047 \
    sim:/MinFunction_0064_tb/i_distance_0048 \
    sim:/MinFunction_0064_tb/i_distance_0049 \
    sim:/MinFunction_0064_tb/i_distance_0050 \
    sim:/MinFunction_0064_tb/i_distance_0051 \
    sim:/MinFunction_0064_tb/i_distance_0052 \
    sim:/MinFunction_0064_tb/i_distance_0053 \
    sim:/MinFunction_0064_tb/i_distance_0054 \
    sim:/MinFunction_0064_tb/i_distance_0055 \
    sim:/MinFunction_0064_tb/i_distance_0056 \
    sim:/MinFunction_0064_tb/i_distance_0057 \
    sim:/MinFunction_0064_tb/i_distance_0058 \
    sim:/MinFunction_0064_tb/i_distance_0059 \
    sim:/MinFunction_0064_tb/i_distance_0060 \
    sim:/MinFunction_0064_tb/i_distance_0061 \
    sim:/MinFunction_0064_tb/i_distance_0062 \
    sim:/MinFunction_0064_tb/i_distance_0063 \
    sim:/MinFunction_0064_tb/o_min \
    sim:/MinFunction_0064_tb/correct_answer

run -all
