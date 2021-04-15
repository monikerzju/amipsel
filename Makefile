DIR := $(CURDIR)

tile:
	sbt "runMain tile.GenT"
	cat $(DIR)/build/verilog/tile/single_port_ram.v >> build/verilog/tile/Tile.v
	cat $(DIR)/build/verilog/tile/dual_port_ram.v >> build/verilog/tile/Tile.v
	rm $(DIR)/build/verilog/tile/firrtl*
	rm $(DIR)/build/verilog/tile/single_port_ram.v
	rm $(DIR)/build/verilog/tile/dual_port_ram.v

core:
	sbt "runMain tile.GenC"