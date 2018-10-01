#
# Makefile
#

SRC=../src
SRCS = $(SRC)/vdp.vhd vdp_objinfo.vhd vdp_colinfo.vhd $(SRC)/vdp_common.vhd vdp_tb.vhd vram.vhd
OBJS = $(notdir $(SRCS:.vhd=.o)) vram_c.o video_c.o

FLAGS=--ieee=synopsys -fexplicit 

all: vdp_tb

vdp_tb.o: vdp.o vram.o video.o
vdp.o: vdp_colinfo.o vdp_objinfo.o vdp_common.o
vram.o: vram_c.o
video.o: video_c.o

%.o: %.vhd # work-obj93.cf
	ghdl -a $(FLAGS) $<

%.o: $(SRC)/%.vhd # work-obj93.cf
	ghdl -a $(FLAGS) $<

work-obj93.cf: $(SRCS)
	ghdl -i $(SRCS)

run: vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable --stop-time=100ms 

view: video.png
	display video.png

video.png: vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable
	rm -f video.png
	avconv -vcodec rawvideo -f rawvideo -pix_fmt rgb565 -s 855x262 -i video.rgb -f image2 -vcodec png video.png
	convert video.png -resize 427x262! video.png

vdp_tb: $(OBJS)
	ghdl -e $(FLAGS) -Wl,vram_c.o -Wl,video_c.o $@

vdp.ghw:  vdp_tb Makefile
	ghdl -r $< --ieee-asserts=disable --stop-time=300us --wave=$@

wave: vdp.ghw
	gtkwave vdp.sav

clean::
	rm -f work-obj93.cf *.o *~ *.lst *.ghw $(ROOT) *.out
