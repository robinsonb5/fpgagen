DEMISTIFYPATH=DeMiSTify
PROJECT=FPGAGen
BOARD=

all: $(DEMISTIFYPATH)/site.mk submodules vbcc firmware init compile tns

$(DEMISTIFYPATH)/site.mk:
	$(info ******************************************************************)
	$(info Copy the example DeMiSTify.site.template file to DeMiSTify/site.mk)
	$(info and edit the paths for the version(s) of Quartus)
	$(info you have installed.)
	$(info ******************************************************************)
	$(error site.mk not found.)

include $(DEMISTIFYPATH)/site.mk

submodules: $(DEMISTIFYPATH)/Makefile

.PHONY: vbcc
vbcc:
	make -C $(DEMISTIFYPATH)/EightThirtyTwo/ vbcc

$(DEMISTIFYPATH)/DeMiSTify/Makefile:
	git submodule update --init --recursive

.PHONY: firmware
firmware: submodules
	make -C firmware -f ../$(DEMISTIFYPATH)/Scripts/firmware.mk DEMISTIFYPATH=../$(DEMISTIFYPATH)

.PHONY: init
init:
	make -f $(DEMISTIFYPATH)/Makefile DEMISTIFYPATH=$(DEMISTIFYPATH) PROJECTS=$(PROJECT) BOARD=$(BOARD) init 

.PHONY: buildid
buildid:
	$(Q13)/quartus_sh -t src/build_id.tcl

.PHONY: compile
compile: buildid
	make -f $(DEMISTIFYPATH)/Makefile DEMISTIFYPATH=$(DEMISTIFYPATH) PROJECTS=$(PROJECT) BOARD=$(BOARD) compile

.PHONY: clean
clean:
	make -f $(DEMISTIFYPATH)/Makefile DEMISTIFYPATH=$(DEMISTIFYPATH) PROJECTS=$(PROJECT) BOARD=$(BOARD) clean

.PHONY: tns
tns:
	grep -r Design-wide\ TNS fpga/*

