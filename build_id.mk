DEMISTIFYPATH=DeMiSTify
include $(DEMISTIFYPATH)/site.mk

.PHONY: build_id.vhd
build_id.vhd: build_id.tcl
	$(Q13)/quartus_sh -t build_id.tcl

