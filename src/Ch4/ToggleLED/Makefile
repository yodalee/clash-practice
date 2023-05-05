TARGET = toggleLED

All: toggleLED.bin

.PHONY: clean prog

DIR := $(shell basename $(shell pwd))
SRCS := $(wildcard *.hs)
VERILOG := $(addprefix verilog/${DIR}.${TARGET}/,$(patsubst %.hs,%.v,$(SRCS)))

$(TARGET).json: ${VERILOG}
	yosys -p "synth_ecp5 -json $@" $^

$(VERILOG) : $(SRCS)
	stack run clash -- --verilog $^

%_out.config: %.json %.lpf
	nextpnr-ecp5 --25k --package CABGA256 --speed 6 --top $* --json $< --textcfg $@ --lpf $*.lpf

%.bin: %_out.config
	ecppack $< $@

prog:
	iceprog -d i:0x1d50:0x602b $(TARGET).bin

clean:
	rm -rf verilog                  # clash generated verilog
	rm -f *.dyn_hi *.dyn_o *.hi *.o # clash compiled code
	rm -f *_out.config *.json *.bin # synthesizer
