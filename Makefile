DEMISTIFYPATH=DeMiSTify
PROJECT=FPGAGen
BOARD=

all: $(DEMISTIFYPATH)/site.mk firmware init compile tns

$(DEMISTIFYPATH)/site.mk:
	$(info ******************************************************)
	$(info Please checkout submodules using "git submodule init" )
	$(info followed by "git submodule update --recursive")
	$(info )
	$(info Then Copy the example DeMiSTify/site.template file to)
	$(info DeMiSTify/site.mk and edit the paths for the version(s))
	$(info  of Quartus you have installed.)
	$(info *******************************************************)
	$(error site.mk not found.)

include $(DEMISTIFYPATH)/site.mk

.PHONY: submodules
submodules:
	git submodule update --init --recursive
	make -C $(DEMISTIFYPATH) -f bootstrap.mk

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
