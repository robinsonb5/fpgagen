## Generated SDC file "hello_led.out.sdc"

## Copyright (C) 1991-2011 Altera Corporation
## Your use of Altera Corporation's design tools, logic functions 
## and other software and tools, and its AMPP partner logic 
## functions, and any output files from any of the foregoing 
## (including device programming or simulation files), and any 
## associated documentation or information are expressly subject 
## to the terms and conditions of the Altera Program License 
## Subscription Agreement, Altera MegaCore Function License 
## Agreement, or other applicable license agreement, including, 
## without limitation, that your use is for the sole purpose of 
## programming logic devices manufactured by Altera and sold by 
## Altera or its authorized distributors.  Please refer to the 
## applicable agreement for further details.


## VENDOR  "Altera"
## PROGRAM "Quartus II"
## VERSION "Version 11.1 Build 216 11/23/2011 Service Pack 1 SJ Web Edition"

## DATE    "Fri Jul 06 23:05:47 2012"

##
## DEVICE  "EP3C25Q240C8"
##


#**************************************************************
# Time Information
#**************************************************************

set_time_format -unit ns -decimal_places 3



#**************************************************************
# Create Clock
#**************************************************************

create_clock -name clk_27 -period 37.037 [get_ports {CLOCK_27[0]}]
create_clock -name {SPI_SCK}  -period 41.666 -waveform { 20.8 41.666 } [get_ports {SPI_SCK}]

create_clock -name fm_clk3 -period 388.9 -waveform { 0 129.64 } [get_nets {virtualtoplevel|fm|u_clksync|u_clkgen|cnt3[0]}]

#create_clock -name fm_clk6 -period 777.8 [get_nets {virtualtoplevel|fm|u_clksync|u_clkgen|clk_n6}]
#create_clock -name VCLK -period 129.6 [get_nets {virtualtoplevel|VCLK}]

create_generated_clock -name VCLK -source [get_nets {U00|altpll_component|auto_generated|wire_pll1_clk[0]}] -divide_by 7 -duty_cycle 57.1 [get_nets {virtualtoplevel|VCLK}]
# romrd_req is identified as a clock, it should not run faster than MCLK/2
create_generated_clock -name romrd_req -source [get_nets {U00|altpll_component|auto_generated|wire_pll1_clk[0]}] -divide_by 7  [get_nets {virtualtoplevel|romrd_req}]
create_generated_clock -name fm_clk6 -source [get_nets {virtualtoplevel|VCLK}] -divide_by 6 -duty_cycle 50 -phase 0 [get_nets {virtualtoplevel|fm|u_clksync|u_clkgen|clk_n6}]
create_generated_clock -name ZCLK -source [get_nets {virtualtoplevel|VCLK}] -divide_by 14 -duty_cycle 50 [get_nets {virtualtoplevel|ZCLK}]
create_generated_clock -name psg_clk -source [get_nets {virtualtoplevel|ZCLK}] -divide_by 32 [get_nets {virtualtoplevel|u_psg|clk_divide[4]}]
#create_generated_clock -name psg_noise -source [get_nets {virtualtoplevel|u_psg|clk_divide[4]}] -divide_by 2 [get_nets {virtualtoplevel|u_psg|t3|v}]

#**************************************************************
# Create Generated Clock
#**************************************************************

derive_pll_clocks 
create_generated_clock -name sd1clk_pin -source [get_pins {U00|altpll_component|auto_generated|pll1|clk[3]}] [get_ports {SDRAM_CLK}]
create_generated_clock -name memclk -source [get_pins {U00|altpll_component|auto_generated|pll1|clk[2]}]
# MCLK
create_generated_clock -name sysclk -source [get_pins {U00|altpll_component|auto_generated|pll1|clk[0]}]

#**************************************************************
# Set Clock Latency
#**************************************************************


#**************************************************************
# Set Clock Uncertainty
#**************************************************************

derive_clock_uncertainty;

#**************************************************************
# Set Input Delay
#**************************************************************

# SDRAM is clocked from sd1clk_pin, but the SDRAM controller uses memclk
set_input_delay -clock memclk -max 6.4 [get_ports SDRAM_DQ[*]]
set_input_delay -clock memclk -min 3.2 [get_ports SDRAM_DQ[*]]

# Delays for async signals - not necessary, but might as well avoid
# having unconstrained ports in the design
set_input_delay -clock sysclk -min 0.0 [get_ports {UART_RX}]
set_input_delay -clock sysclk -max 0.0 [get_ports {UART_RX}]

#**************************************************************
# Set Output Delay
#**************************************************************

set_output_delay -clock memclk -max 1.5 [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]
set_output_delay -clock memclk -min -0.8 [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]
set_output_delay -clock sd1clk_pin -max 0.5 [get_ports SDRAM_CLK]
set_output_delay -clock sd1clk_pin -min 0.5 [get_ports SDRAM_CLK]
set_output_delay -clock [get_clocks {sysclk}] -max 0 [get_ports {VGA_*}]
set_output_delay -clock [get_clocks {sysclk}] -min -5 [get_ports {VGA_*}]

#set_output_delay -clock clk21m -min 0.0 [get_ports AUDIO*]
#set_output_delay -clock clk21m -max 0.0 [get_ports AUDIO*]
#set_output_delay -clock SPICLK -min 0.0 [get_ports LED*]
#set_output_delay -clock SPICLK -max 0.0 [get_ports LED*]
#set_output_delay -clock SPICLK -min 0.5 [get_ports SPI_DO*]
#set_output_delay -clock SPICLK -max 2.0 [get_ports SPI_DO*]

#**************************************************************
# Set Clock Groups
#**************************************************************

set_clock_groups -asynchronous -group [get_clocks {SPI_SCK}] -group [get_clocks {U00|altpll_component|auto_generated|pll1|clk[*]}]

#**************************************************************
# Set False Path
#**************************************************************

# Asynchronous signal, so not important timing-wise
# JT12 internal clock uses synchronizers:
set_false_path  -from  [get_clocks {VCLK}]  -to  [get_clocks {fm_clk6}]

#JT12 output is not synchronous to the DAC:
set_false_path  -from  [get_clocks {fm_clk6}]  -to  [get_clocks {U00|altpll_component|auto_generated|pll1|clk[2]}]

# set_false_path -from [get_registers {Virtual_Toplevel:virtualtoplevel|jt12:fm|jt12_clksync:u_clksync|write_copy}] -to [get_nets {virtualtoplevel|fm|u_clksync|write}]

set_false_path -to [get_ports {UART_TX}]
set_false_path -to [get_ports {AUDIO_L}]
set_false_path -to [get_ports {AUDIO_R}]
set_false_path -to [get_ports {LED}]

#**************************************************************
# Set Multicycle Path
#**************************************************************

set_multicycle_path -from [get_clocks {memclk}] -to [get_clocks {sysclk}] -setup 2
set_multicycle_path -from [get_clocks {memclk}] -to [get_clocks {sysclk}] -hold 2
set_multicycle_path -from [get_clocks {sysclk}] -to [get_clocks {memclk}] -setup 2
set_multicycle_path -from [get_clocks {sysclk}] -to [get_clocks {memclk}] -hold 2

set_multicycle_path -from {Virtual_Toplevel:virtualtoplevel|TG68KdotC_Kernel:tg68|*} -setup 2
set_multicycle_path -from {Virtual_Toplevel:virtualtoplevel|TG68KdotC_Kernel:tg68|*} -hold 2

set_multicycle_path -to {VGA_*[*]} -setup 2
set_multicycle_path -to {VGA_*[*]} -hold 2

set_multicycle_path -through [get_nets {*zpu|Mult0*}] -setup -end 2
set_multicycle_path -through [get_nets {*zpu|Mult0*}] -hold -end 2

#**************************************************************
# Set Maximum Delay
#**************************************************************



#**************************************************************
# Set Minimum Delay
#**************************************************************



#**************************************************************
# Set Input Transition
#**************************************************************
