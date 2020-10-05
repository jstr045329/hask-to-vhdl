vlib work

vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSimToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/hask-to-vhdl/src/VhdLibs/VhdSynthToolsPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/dijkstra_vhd/DijkstraPkg.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/dijkstra_vhd/MinHeap.vhd
vcom -reportprogress 300 -work work C:/Users/jstrieter/dijkstra_vhd/MinHeap_tb.vhd

# TODO: Rename DijkstraPkg_tb to DijkstraPkg_tb_functions_only or something like that.
vcom -reportprogress 300 -work work C:/Users/jstrieter/dijkstra_vhd/DijkstraPkg_tb.vhd

vsim work.MinHeap_tb -t ns


add wave -position insertpoint  \
sim:/minheap_tb/clk \
sim:/minheap_tb/rst \
sim:/minheap_tb/UUT/s_soft_enable_0 \
sim:/minheap_tb/UUT/s_soft_enable_1 \
sim:/minheap_tb/UUT/s_soft_enable_2 \
sim:/minheap_tb/UUT/s_soft_enable_3 \
sim:/minheap_tb/UUT/s_soft_enable_4 \
sim:/minheap_tb/UUT/s_soft_enable_5 \
sim:/minheap_tb/UUT/s_soft_enable_6 \
sim:/minheap_tb/UUT/s_soft_enable_7 \
sim:/minheap_tb/i_din \
sim:/minheap_tb/i_start \
sim:/minheap_tb/UUT/s_go_0 \
sim:/minheap_tb/UUT/s_go_1 \
sim:/minheap_tb/i_soft_rst \
sim:/minheap_tb/i_wr_en \
sim:/minheap_tb/INPUT_NUMBERS \
sim:/minheap_tb/o_minimum \
sim:/minheap_tb/o_node_array \
sim:/minheap_tb/UUT/s_tiny_graph_array \
sim:/minheap_tb/UUT/s_action_array \
sim:/minheap_tb/o_ok_to_write \
sim:/minheap_tb/o_sorted \
sim:/minheap_tb/sim_done \
sim:/minheap_tb/test_stage \
sim:/minheap_tb/s_tb_thinks_nodes_are_sorted

run -all
