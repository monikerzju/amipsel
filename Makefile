DIR := $(CURDIR)

tilediff:
	sbt "runMain tile.GenT -diff"
	cat $(DIR)/build/verilog/tile/single_port_ram.v >> build/verilog/tile/Tile.v
	cat $(DIR)/build/verilog/tile/dual_port_ram.v >> build/verilog/tile/Tile.v
	rm $(DIR)/build/verilog/tile/firrtl*
	rm $(DIR)/build/verilog/tile/single_port_ram.v
	rm $(DIR)/build/verilog/tile/dual_port_ram.v

tile:
	sbt "runMain tile.GenT"
	cat $(DIR)/build/verilog/tile/single_port_ram.v >> build/verilog/tile/Tile.v
	cat $(DIR)/build/verilog/tile/dual_port_ram.v >> build/verilog/tile/Tile.v
	rm $(DIR)/build/verilog/tile/firrtl*
	rm $(DIR)/build/verilog/tile/single_port_ram.v
	rm $(DIR)/build/verilog/tile/dual_port_ram.v

core:
	sbt "runMain tile.GenC"

tilevery:
	sbt "runMain tile.GenTV"
	cat $(DIR)/build/verilog/tile/single_port_ram.v >> build/verilog/tile/TileForVerilator.v
	cat $(DIR)/build/verilog/tile/dual_port_ram.v >> build/verilog/tile/TileForVerilator.v
	rm $(DIR)/build/verilog/tile/firrtl*
	rm $(DIR)/build/verilog/tile/single_port_ram.v
	rm $(DIR)/build/verilog/tile/dual_port_ram.v
	rm $(DIR)/build/verilog/tile/TileForVerilator.fir
	rm $(DIR)/build/verilog/tile/TileForVerilator.anno.json

clean:
	rm -rf build/