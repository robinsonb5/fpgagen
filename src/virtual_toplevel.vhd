-- Copyright (c) 2010 Gregory Estrade (greg@torlus.com)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.

library STD;
use STD.TEXTIO.ALL;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_TEXTIO.all;
use work.jt12.all;
use work.jt89.all;

entity Virtual_Toplevel is
	generic (
		colAddrBits : integer := 9;
		rowAddrBits : integer := 13;
		rasCasTiming : integer := 2;
		prechargeTiming: integer := 2
	);
	port(
		reset : in std_logic;
		MCLK : in std_logic;		-- 54MHz
		SDR_CLK : in std_logic;		-- 108MHz

		DRAM_ADDR	: out std_logic_vector(rowAddrBits-1 downto 0);
		DRAM_BA_0	: out std_logic;
		DRAM_BA_1	: out std_logic;
		DRAM_CAS_N	: out std_logic;
		DRAM_CKE	: out std_logic;
		DRAM_CS_N	: out std_logic;
		DRAM_DQ		: inout std_logic_vector(15 downto 0);
		DRAM_LDQM	: out std_logic;
		DRAM_RAS_N	: out std_logic;
		DRAM_UDQM	: out std_logic;
		DRAM_WE_N	: out std_logic;
		
		DAC_LDATA : out std_logic_vector(15 downto 0);
		DAC_RDATA : out std_logic_vector(15 downto 0);
		
		RED			: out std_logic_vector(3 downto 0);
		GREEN		: out std_logic_vector(3 downto 0);
		BLUE		: out std_logic_vector(3 downto 0);
		VS			: out std_logic;
		HS			: out std_logic;
		
		LED : out std_logic;

		joya : in std_logic_vector(11 downto 0) := (others =>'1');
		joyb : in std_logic_vector(11 downto 0) := (others =>'1');

		mouse_x: in std_logic_vector(7 downto 0);
		mouse_y: in std_logic_vector(7 downto 0);
		mouse_flags: in std_logic_vector(7 downto 0);
		mouse_strobe: in std_logic;

		saveram_addr : in std_logic_vector(14 downto 0);
		saveram_we   : in std_logic;
		saveram_din  : in std_logic_vector(7 downto 0);
		saveram_rd   : in std_logic;
		saveram_dout : out std_logic_vector(7 downto 0);

        -- ROM Loader / Host boot data
		ext_reset_n    : in std_logic := '1';
		ext_bootdone   : in std_logic := '0';
		ext_data       : in std_logic_vector(15 downto 0) := (others => '0');
		ext_data_req   : out std_logic;
		ext_data_ack   : in std_logic := '0';

        -- DIP switches
		ext_sw         : in std_logic_vector(15 downto 0) -- 1 - SVP
														  -- 2 - joy swap
														  -- 3 - PSG EN
														  -- 4 - FM EN
														  -- 5 - Export
														  -- 6 - PAL
														  -- 7 - Swap y
														  -- 8 - 3 Buttons only
														  -- 9 - VRAM speed emu
														  -- 10 - EEPROM emu (fake)
														  -- 12-11 - Mouse
														  -- 13 - HiFi PCM
														  -- 14 - CPU Turbo
														  -- 15 - Border
	);
end entity;

architecture rtl of Virtual_Toplevel is

constant addrwidth : integer := rowAddrBits+colAddrBits+2;

-- "FLASH"
signal romwr_req : std_logic := '0';
signal romwr_ack : std_logic;
signal romwr_we  : std_logic := '1';
signal romwr_a : unsigned(23 downto 1);
signal romwr_d : std_logic_vector(15 downto 0);
signal romwr_q : std_logic_vector(15 downto 0);

signal romrd_req : std_logic := '0';
signal romrd_ack : std_logic;
signal romrd_a : std_logic_vector(23 downto 1);
signal romrd_q : std_logic_vector(15 downto 0);

type fc_t is ( FC_IDLE, 
	FC_FX68_RD,
	FC_DMA_RD,
	FC_T80_BR,
	FC_T80_RD
);
signal FC : fc_t;
signal FC_DELAY : std_logic_vector(1 downto 0);

-- 68000 RAM
signal ram68k_req : std_logic;
signal ram68k_ack : std_logic;
signal ram68k_we : std_logic;
signal ram68k_a : std_logic_vector(15 downto 1);
signal ram68k_d : std_logic_vector(15 downto 0);
signal ram68k_q : std_logic_vector(15 downto 0);
signal ram68k_l_n : std_logic;
signal ram68k_u_n : std_logic;

type sdrc_t is ( SDRC_IDLE,
	SDRC_FX68,
	SDRC_DMA,
	SDRC_T80_BR,
	SDRC_T80);
signal SDRC : sdrc_t;
signal SDRC_DELAY : std_logic_vector(1 downto 0);

-- SRAM
signal sram_req : std_logic := '0';
signal sram_ack : std_logic;
signal sram_we : std_logic := '0';
signal sram_a : std_logic_vector(15 downto 1);
signal sram_d : std_logic_vector(15 downto 0);
signal sram_q : std_logic_vector(15 downto 0);
signal sram_l_n : std_logic;
signal sram_u_n : std_logic;

type sramrc_t is ( SRAMRC_IDLE,	SRAMRC_EXT, SRAMRC_FX68);
signal SRAMRC : sramrc_t;
signal SRAM_EN : std_logic;
signal SRAM_EN_AUTO : std_logic;
signal SRAM_EN_PAGEIN : std_logic;
signal EEPROM_EN : std_logic;

-- VRAM
signal vram_req : std_logic;
signal vram_ack : std_logic;
signal vram_we : std_logic;
signal vram_a : std_logic_vector(15 downto 1);
signal vram_d : std_logic_vector(15 downto 0);
signal vram_q : std_logic_vector(15 downto 0);
signal vram_l_n : std_logic;
signal vram_u_n : std_logic;

signal vram32_req : std_logic;
signal vram32_ack : std_logic;
signal vram32_a   : std_logic_vector(15 downto 1);
signal vram32_q   : std_logic_vector(31 downto 0);

-- Z80 RAM
signal zram_a : std_logic_vector(12 downto 0);
signal zram_d : std_logic_vector(7 downto 0);
signal zram_q : std_logic_vector(7 downto 0);
signal zram_we : std_logic;

signal FX68_ZRAM_SEL		: std_logic;
signal FX68_ZRAM_D			: std_logic_vector(15 downto 0);
signal FX68_ZRAM_DTACK_N	: std_logic;

signal T80_ZRAM_SEL		: std_logic;
signal T80_ZRAM_D			: std_logic_vector(7 downto 0);
signal T80_ZRAM_DTACK_N	: std_logic;

type zrc_t is ( ZRC_IDLE, ZRC_ACC1, ZRC_ACC2 );
signal ZRC : zrc_t;

-- SVP
signal svp_ram1_req : std_logic;
signal svp_ram1_ack : std_logic;
signal svp_ram1_we  : std_logic;
signal svp_ram1_a   : std_logic_vector(16 downto 1);
signal svp_ram1_d   : std_logic_vector(15 downto 0);
signal svp_ram1_q   : std_logic_vector(15 downto 0);

signal svp_ram2_req : std_logic;
signal svp_ram2_ack : std_logic;
signal svp_ram2_we  : std_logic;
signal svp_ram2_a   : std_logic_vector(16 downto 1);
signal svp_ram2_d   : std_logic_vector(15 downto 0);
signal svp_ram2_q   : std_logic_vector(15 downto 0);
signal svp_ram2_u_n : std_logic;
signal svp_ram2_l_n : std_logic;

signal svp_rom_req  : std_logic;
signal svp_rom_ack  : std_logic;
signal svp_rom_a    : std_logic_vector(20 downto 1);
signal svp_rom_q    : std_logic_vector(15 downto 0);

type svprc_t is ( SVPRC_IDLE, SVPRC_FX68, SVPRC_DMA);
signal SVPRC : svprc_t;

signal SVP_CLKEN    : std_logic;
signal SVP_ENABLE   : std_logic;

signal SVP_DI       : std_logic_vector(15 downto 0);
signal SVP_DO       : std_logic_vector(15 downto 0);
signal SVP_SEL      : std_logic;
signal SVP_A        : std_logic_vector(3 downto 1);
signal SVP_RNW      : std_logic;
signal SVP_DTACK_N  : std_logic;
signal FX68_SVP_SEL : std_logic;
signal T80_SVP_SEL  : std_logic;

-- Genesis core
signal NO_DATA		: std_logic_vector(15 downto 0);	-- SYNTHESIS gp/m68k.c line 12

signal MRST_N		: std_logic;

-- 68K
signal FX68_RES_N	: std_logic;
signal FX68_DI		: std_logic_vector(15 downto 0);
signal FX68_IPL_N	: std_logic_vector(2 downto 0);
signal FX68_DTACK_N	: std_logic;
signal FX68_A		: std_logic_vector(23 downto 1);
signal FX68_DO		: std_logic_vector(15 downto 0);
signal FX68_SEL		: std_logic;
signal FX68_UDS_N	: std_logic;
signal FX68_LDS_N	: std_logic;
signal FX68_RNW		: std_logic;
signal FX68_RNW_D	: std_logic;
signal FX68_INTACK	: std_logic;

signal FX68_PHI1	: std_logic;
signal FX68_PHI2	: std_logic;
signal FX68_FC		: std_logic_vector(2 downto 0);
signal FX68_AS_N	: std_logic;
signal FX68_AS_N_D	: std_logic;
signal FX68_VPA_N	: std_logic;
signal FX68_IO_READY : std_logic;
signal FX68_BG_N	: std_logic;
signal FX68_BR_N	: std_logic;
signal FX68_BGACK_N	: std_logic;

-- Z80
signal T80_RESET_N	: std_logic;
signal T80_WAIT_N	: std_logic;
signal T80_INT_N           : std_logic;
signal T80_NMI_N           : std_logic;
signal T80_BUSRQ_N         : std_logic;
signal T80_M1_N            : std_logic;
signal T80_MREQ_N          : std_logic;
signal T80_IORQ_N          : std_logic;
signal T80_RD_N            : std_logic;
signal T80_WR_N            : std_logic;
signal T80_RFSH_N          : std_logic;
signal T80_HALT_N          : std_logic;
signal T80_BUSAK_N         : std_logic;
signal T80_A               : std_logic_vector(15 downto 0);
signal T80_DI              : std_logic_vector(7 downto 0);
signal T80_DO              : std_logic_vector(7 downto 0);

signal FCLK_EN		: std_logic;

-- CLOCK GENERATION
signal VCLKCNT     : std_logic_vector(2 downto 0);
signal ZCLK_ENA    : std_logic;
signal ZCLK_nENA   : std_logic;
signal ZCLKCNT     : std_logic_vector(3 downto 0);
signal RFRSH_CNT   : std_logic_vector(7 downto 0);
signal RFRSH_DELAY : std_logic;
signal MCLKCNT     : std_logic_vector(11 downto 0);

-- FLASH CONTROL
signal FX68_FLASH_SEL		: std_logic;
signal FX68_FLASH_D			: std_logic_vector(15 downto 0);
signal FX68_FLASH_DTACK_N	: std_logic;

signal T80_FLASH_SEL		: std_logic;
signal T80_FLASH_D			: std_logic_vector(7 downto 0);
signal T80_FLASH_DTACK_N	: std_logic;
signal T80_FLASH_BR_N       : std_logic := '1';
signal T80_FLASH_BGACK_N    : std_logic := '1';

signal DMA_FLASH_SEL		: std_logic;
signal DMA_FLASH_D			: std_logic_vector(15 downto 0);
signal DMA_FLASH_D_REG		: std_logic_vector(15 downto 0);
signal DMA_FLASH_DTACK_N	: std_logic;
signal DMA_FLASH_DTACK_N_REG: std_logic;

-- SDRAM CONTROL
signal FX68_SDRAM_SEL		: std_logic;
signal FX68_SDRAM_D			: std_logic_vector(15 downto 0);
signal FX68_SDRAM_DTACK_N	: std_logic;

signal T80_SDRAM_SEL		: std_logic;
signal T80_SDRAM_D			: std_logic_vector(7 downto 0);
signal T80_SDRAM_DTACK_N	: std_logic;
signal T80_SDRAM_BR_N       : std_logic := '1';
signal T80_SDRAM_BGACK_N    : std_logic := '1';

signal DMA_SDRAM_SEL		: std_logic;
signal DMA_SDRAM_D			: std_logic_vector(15 downto 0);
signal DMA_SDRAM_D_REG		: std_logic_vector(15 downto 0);
signal DMA_SDRAM_DTACK_N	: std_logic;
signal DMA_SDRAM_DTACK_N_REG: std_logic;

-- SRAM CONTROL
signal FX68_SRAM_SEL		: std_logic;
signal FX68_SRAM_D			: std_logic_vector(15 downto 0);
signal FX68_SRAM_DTACK_N	: std_logic;

signal FX68_EEPROM_SEL		: std_logic;
signal FX68_EEPROM_DATA		: std_logic_vector(15 downto 0);
signal FX68_EEPROM_DTACK_N	: std_logic;

signal BIG_CART             : std_logic;
signal SSF2_MAP             : std_logic_vector(8*6-1 downto 0);
signal SSF2_USE_MAP         : std_logic;
signal ROM_PAGE             : std_logic_vector(2 downto 0);
signal ROM_PAGE_A           : std_logic_vector(4 downto 0); -- 16 MB only (original mapper: max. 32)

-- SVP CONTROL
signal FX68_SVP_RAM_SEL     : std_logic;
signal FX68_SVP_RAM_D       : std_logic_vector(15 downto 0);
signal FX68_SVP_RAM_DTACK_N : std_logic;

signal DMA_SVP_RAM_SEL      : std_logic;
signal DMA_SVP_RAM_D        : std_logic_vector(15 downto 0);
signal DMA_SVP_RAM_DTACK_N  : std_logic;

-- OPERATING SYSTEM ROM
signal FX68_OS_SEL			: std_logic;
signal FX68_OS_D			: std_logic_vector(15 downto 0);
signal FX68_OS_DTACK_N		: std_logic;
signal OS_OEn				: std_logic;

-- CONTROL AREA
signal ZBUSREQ				: std_logic;
signal ZRESET_N				: std_logic;
signal ZBUSACK_N				: std_logic;
signal CART_EN				: std_logic;

signal FX68_CTRL_SEL		: std_logic;
signal FX68_CTRL_D			: std_logic_vector(15 downto 0);
signal FX68_CTRL_DTACK_N		: std_logic;

signal T80_CTRL_SEL		: std_logic;
signal T80_CTRL_D			: std_logic_vector(7 downto 0);
signal T80_CTRL_DTACK_N		: std_logic;

-- I/O AREA
signal IO_SEL				: std_logic;
signal IO_A 				: std_logic_vector(4 downto 0);
signal IO_RNW				: std_logic;
signal IO_UDS_N				: std_logic;
signal IO_LDS_N				: std_logic;
signal IO_DI				: std_logic_vector(15 downto 0);
signal IO_DO				: std_logic_vector(15 downto 0);
signal IO_DTACK_N			: std_logic;

signal FX68_IO_SEL		: std_logic;
signal FX68_IO_D			: std_logic_vector(15 downto 0);
signal FX68_IO_DTACK_N		: std_logic;

signal T80_IO_SEL		: std_logic;
signal T80_IO_D			: std_logic_vector(7 downto 0);
signal T80_IO_DTACK_N		: std_logic;

type ioc_t is ( IOC_IDLE, IOC_FX68_ACC, IOC_T80_ACC, IOC_DESEL );
signal IOC : ioc_t;

-- VDP AREA
signal VDP_SEL				: std_logic;
signal VDP_A 				: std_logic_vector(4 downto 0);
signal VDP_RNW				: std_logic;
signal VDP_DI				: std_logic_vector(15 downto 0);
signal VDP_DO				: std_logic_vector(15 downto 0);
signal VDP_DTACK_N			: std_logic;
signal VDP_RST_N			: std_logic;
signal VDP_VRAM_SPEED		: std_logic;
signal VDP_BR_N             : std_logic;
signal VDP_BGACK_N          : std_logic;

signal FX68_VDP_SEL		: std_logic;
signal FX68_VDP_D			: std_logic_vector(15 downto 0);
signal FX68_VDP_DTACK_N		: std_logic;

signal T80_VDP_SEL          : std_logic;
signal T80_VDP_D            : std_logic_vector(7 downto 0);
signal T80_VDP_DTACK_N      : std_logic;

type vdpc_t is ( VDPC_IDLE, VDPC_FX68_ACC, VDPC_T80_ACC, VDPC_DESEL );
signal VDPC : vdpc_t;

-- FM AREA
signal FM_SEL			: std_logic;
signal FM_A 			: std_logic_vector(1 downto 0);
signal FM_RNW			: std_logic;
signal FM_RNW_D			: std_logic;
signal FM_UDS_N			: std_logic;
signal FM_LDS_N			: std_logic;
signal FM_DI			: std_logic_vector(7 downto 0);
signal FM_DO			: std_logic_vector(7 downto 0);
signal FM_CLKOUT		: std_logic;
signal FM_SAMPLE		: std_logic;
signal FM_LEFT			: std_logic_vector(15 downto 0);
signal FM_RIGHT		: std_logic_vector(15 downto 0);

signal FM_ENABLE		: std_logic;
signal FM_HIFI			: std_logic;

-- PSG
signal PSG_SEL			: std_logic;
signal T80_PSG_SEL		: std_logic;
signal FX68_PSG_SEL		: std_logic;
signal PSG_DI			: std_logic_vector(7 downto 0);
signal PSG_SND			: std_logic_vector(10 downto 0);
signal PSG_ENABLE		: std_logic;

signal FX68_FM_SEL		: std_logic;
signal FX68_FM_D			: std_logic_vector(15 downto 0);

signal T80_FM_SEL		: std_logic;
signal T80_FM_D			: std_logic_vector(7 downto 0);

-- BANK ADDRESS REGISTER
signal BAR 					: std_logic_vector(23 downto 15);
signal FX68_BAR_SEL			: std_logic;
signal FX68_BAR_D			: std_logic_vector(15 downto 0);
signal FX68_BAR_DTACK_N		: std_logic;
signal T80_BAR_SEL			: std_logic;
signal T80_BAR_D			: std_logic_vector(7 downto 0);
signal T80_BAR_DTACK_N		: std_logic;

signal FX68_TIME_SEL        : std_logic;

-- INTERRUPTS
signal HINT		: std_logic;
signal VINT_FX68	: std_logic;
signal VINT_T80		: std_logic;

-- VDP VBUS DMA
signal VBUS_ADDR	: std_logic_vector(23 downto 1);
signal VBUS_DATA	: std_logic_vector(15 downto 0);		
signal VBUS_SEL		: std_logic;
signal VBUS_DTACK_N	: std_logic;	

-- VDP Video Output
signal VDP_RED		: std_logic_vector(3 downto 0);
signal VDP_GREEN	: std_logic_vector(3 downto 0);
signal VDP_BLUE	: std_logic_vector(3 downto 0);
signal VDP_VS_N	: std_logic;
signal VDP_HS_N	: std_logic;

-- Joystick signals
signal JOY_SWAP	    : std_logic;
signal JOY_Y_SWAP   : std_logic;
signal JOY_1_UP     : std_logic;
signal JOY_1_DOWN   : std_logic;
signal JOY_2_UP     : std_logic;
signal JOY_2_DOWN   : std_logic;
signal JOY_1        : std_logic_vector(11 downto 0);
signal JOY_2        : std_logic_vector(11 downto 0);
signal JOY_3BUT		: std_logic;

signal SDR_INIT_DONE	: std_logic;

type bootStates is (BOOT_READ_1, BOOT_WRITE_1, BOOT_WRITE_2, BOOT_DONE);
signal bootState : bootStates := BOOT_DONE;

signal FL_DQ : std_logic_vector(15 downto 0);

signal osd_window : std_logic;
signal osd_pixel : std_logic;

type romStates is (ROM_IDLE, ROM_READ);
signal romState : romStates := ROM_IDLE;

signal SW : std_logic_vector(15 downto 0);
signal KEY : std_logic_vector(3 downto 0);

signal PAL : std_logic;
signal model: std_logic;
signal PAL_IO: std_logic;
signal MSEL : std_logic_vector(1 downto 0);
signal MOUSE_Y_ADJ : std_logic_vector(8 downto 0);
signal CPU_TURBO : std_logic;
signal BORDER : std_logic;

-- DEBUG
signal HEXVALUE			: std_logic_vector(15 downto 0);

COMPONENT fx68k
	PORT
	(
		clk			: in std_logic;
		extReset	: in std_logic;	-- External sync reset on emulated system
		pwrUp		: in std_logic; -- Asserted together with reset on emulated system coldstart
		enPhi1		: in std_logic;
		enPhi2		: in std_logic;	-- Clock enables. Next cycle is PHI1 or PHI2

		eRWn		: out std_logic;
		ASn			: out std_logic;
		LDSn		: out std_logic;
		UDSn		: out std_logic;
		E			: out std_logic;
		VMAn		: out std_logic;
		FC0			: out std_logic;
		FC1			: out std_logic;
		FC2			: out std_logic;
		BGn			: out std_logic;
		oRESETn		: out std_logic;
		oHALTEDn	: out std_logic;
		DTACKn		: in std_logic;
		VPAn		: in std_logic;
		BERRn		: in std_logic;
		BRn			: in std_logic;
		BGACKn		: in std_logic;
		IPL0n		: in std_logic;
		IPL1n		: in std_logic;
		IPL2n		: in std_logic;
		iEdb		: in std_logic_vector(15 downto 0);
		oEdb		: out std_logic_vector(15 downto 0);
		eab			: out std_logic_vector(23 downto 1)
	);
END COMPONENT;

begin

-- -----------------------------------------------------------------------
-- Global assignments
-- -----------------------------------------------------------------------

-- Reset
process(MRST_N,MCLK)
begin
	if rising_edge(MCLK) then
		MRST_N <= SDR_INIT_DONE and reset and ext_bootdone and ext_reset_n;
		if bootState = BOOT_DONE then
			VDP_RST_N <= '1';
		else
			VDP_RST_N <= '0';
		end if;
	end if;
end process;

-- Joystick swapping
JOY_SWAP <= SW(2);
JOY_Y_SWAP <= SW(7);
JOY_3BUT <= SW(8);

JOY_1_DOWN <= joya(3) when JOY_Y_SWAP = '1' else joya(2);
JOY_1_UP <= joya(2) when JOY_Y_SWAP = '1' else joya(3);
JOY_2_DOWN <= joyb(3) when JOY_Y_SWAP = '1' else joyb(2);
JOY_2_UP <= joyb(2) when JOY_Y_SWAP = '1' else joyb(3);

JOY_1(1 downto 0) <= joyb(1 downto 0) when JOY_SWAP = '1' else joya(1 downto 0);
JOY_1(2) <= JOY_2_DOWN when JOY_SWAP = '1' else JOY_1_DOWN;
JOY_1(3) <= JOY_2_UP when JOY_SWAP = '1' else JOY_1_UP;
JOY_1(11 downto 4) <= joyb(11 downto 4) when JOY_SWAP = '1' else joya(11 downto 4);

JOY_2(1 downto 0) <= joyb(1 downto 0) when JOY_SWAP = '0' else joya(1 downto 0);
JOY_2(2) <= JOY_2_DOWN when JOY_SWAP = '0' else JOY_1_DOWN;
JOY_2(3) <= JOY_2_UP when JOY_SWAP = '0' else JOY_1_UP;
JOY_2(11 downto 4) <= joyb(11 downto 4) when JOY_SWAP = '0' else joya(11 downto 4);

model <= SW(5);
PAL <= SW(6);

VDP_VRAM_SPEED <= SW(9);
EEPROM_EN <= SW(10);
MSEL <= SW(12 downto 11);

MOUSE_Y_ADJ <= mouse_flags(5) & mouse_y when JOY_Y_SWAP = '0' else (not mouse_flags(5) & not mouse_y) + 1;
JOY_Y_SWAP <= SW(7);

CPU_TURBO <= SW(14);
BORDER <= SW(15);

-- DIP Switches
SW <= ext_sw;

-- SDRAM
DRAM_CKE <= '1';
DRAM_CS_N <= '0';

-- LED
LED <= '0';

-- -----------------------------------------------------------------------
-- SDRAM Controller
-- -----------------------------------------------------------------------		
sdc : entity work.chameleon_sdram generic map (
	colAddrBits => colAddrBits,
	rowAddrBits => rowAddrBits,
	prechargeTiming => prechargeTiming,
	rasCasTiming => rasCasTiming
) port map(
	clk         => SDR_CLK,
	reset_n     => ext_reset_n,

	std_logic_vector(sd_data)	=> DRAM_DQ,
	std_logic_vector(sd_addr)	=> DRAM_ADDR,
	sd_we_n						=> DRAM_WE_N,
	sd_ras_n					=> DRAM_RAS_N,
	sd_cas_n					=> DRAM_CAS_N,
	sd_ba_0						=> DRAM_BA_0,
	sd_ba_1						=> DRAM_BA_1,
	sd_ldqm						=> DRAM_LDQM,
	sd_udqm						=> DRAM_UDQM,

	romwr_req	=> romwr_req,
	romwr_ack	=> romwr_ack,
	romwr_we 	=> romwr_we,
	romwr_a		=> conv_std_logic_vector(0, addrwidth - 23) & std_logic_vector(romwr_a),
	romwr_d		=> romwr_d,
	romwr_q		=> romwr_q,

	romrd_req	=> romrd_req,
	romrd_ack	=> romrd_ack,
	romrd_a		=> conv_std_logic_vector(0, addrwidth - 23) & romrd_a,
	romrd_q		=> romrd_q,

	ram68k_req	=> ram68k_req,
	ram68k_ack	=> ram68k_ack,
	ram68k_we	=> ram68k_we,
	ram68k_a	=> conv_std_logic_vector(2#111111000#, addrwidth - 15) & ram68k_a,
	ram68k_d	=> ram68k_d,
	ram68k_q	=> ram68k_q,
	ram68k_u_n	=> ram68k_u_n,
	ram68k_l_n	=> ram68k_l_n,

	sram_req	=> sram_req,
	sram_ack	=> sram_ack,
	sram_we	=> sram_we,
	sram_a		=> conv_std_logic_vector(2#111111010#, addrwidth - 15) & sram_a,
	sram_d		=> sram_d,
	sram_q		=> sram_q,
	sram_u_n	=> sram_u_n,
	sram_l_n	=> sram_l_n,

	vram_req	=> vram_req,
	vram_ack => vram_ack,
	vram_we	=> vram_we,
	vram_a	=> conv_std_logic_vector(2#111111100#, addrwidth - 15) & vram_a,
	vram_d	=> vram_d,
	vram_q	=> vram_q,
	vram_u_n => vram_u_n,
	vram_l_n => vram_l_n,

	vram32_req => vram32_req,
	vram32_ack => vram32_ack,
	vram32_a   => conv_std_logic_vector(2#111111100#, addrwidth - 15) & vram32_a,
	vram32_q   => vram32_q,

	svp_ram1_req => svp_ram1_req,
	svp_ram1_ack => svp_ram1_ack,
	svp_ram1_we  => svp_ram1_we,
	svp_ram1_a   => conv_std_logic_vector(2#11111111#, addrwidth - 16) & svp_ram1_a,
	svp_ram1_d   => svp_ram1_d,
	svp_ram1_q   => svp_ram1_q,

	svp_ram2_req => svp_ram2_req,
	svp_ram2_ack => svp_ram2_ack,
	svp_ram2_we  => svp_ram2_we,
	svp_ram2_a   => conv_std_logic_vector(2#11111111#, addrwidth - 16) & svp_ram2_a,
	svp_ram2_d   => svp_ram2_d,
	svp_ram2_q   => svp_ram2_q,
	svp_ram2_u_n => svp_ram2_u_n,
	svp_ram2_l_n => svp_ram2_l_n,

	svp_rom_req	=> svp_rom_req,
	svp_rom_ack	=> svp_rom_ack,
	svp_rom_a	=> conv_std_logic_vector(0, addrwidth - 20) & svp_rom_a,
	svp_rom_q	=> svp_rom_q,

	initDone 	=> SDR_INIT_DONE
);

-- -----------------------------------------------------------------------
-- Z80 RAM
-- -----------------------------------------------------------------------
zram : entity work.SinglePortRAM
generic map (
	addrbits => 13,
	databits => 8
)
port map(
	address	  => zram_a,
	clock     => MCLK,
	data      => zram_d,
	wren      => zram_we,
	q         => zram_q
);

-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- Genesis Core
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
FX68_BR_N <= VDP_BR_N and T80_FLASH_BR_N and T80_SDRAM_BR_N;
FX68_BGACK_N <= VDP_BGACK_N and T80_FLASH_BGACK_N and T80_SDRAM_BGACK_N;

-- 68K
fx68k_inst: fx68k
	port map (
		clk			=> MCLK,
		extReset	=> not FX68_RES_N,
		pwrUp		=> not FX68_RES_N,
		enPhi1		=> FX68_PHI1,
		enPhi2		=> FX68_PHI2,

		eRWn		=> FX68_RNW,
		ASn			=> FX68_AS_N,
		LDSn		=> FX68_LDS_N,
		UDSn		=> FX68_UDS_N,
		E			=> open,
		VMAn		=> open,
		FC0			=> FX68_FC(0),
		FC1			=> FX68_FC(1),
		FC2			=> FX68_FC(2),
		BGn			=> FX68_BG_N,
		oRESETn		=> open,
		oHALTEDn	=> open,
		DTACKn		=> FX68_DTACK_N,
		VPAn		=> FX68_VPA_N,
		BERRn		=> '1',
		BRn			=> FX68_BR_N,
		BGACKn		=> FX68_BGACK_N,
		IPL0n		=> FX68_IPL_N(0),
		IPL1n		=> FX68_IPL_N(1),
		IPL2n		=> FX68_IPL_N(2),
		iEdb		=> FX68_DI,
		oEdb		=> FX68_DO,
		eab			=> FX68_A(23 downto 1)
	);

-- Z80
t80 : entity work.t80pa
port map(
	RESET_n		=> T80_RESET_N,
	CLK		=> MCLK,
	CEN_p	=> ZCLK_ENA,
	CEN_n	=> ZCLK_nENA,
	WAIT_n	=> T80_WAIT_N,
	INT_n		=> T80_INT_N,
	NMI_n		=> T80_NMI_N,
	BUSRQ_n	=> T80_BUSRQ_N,
	M1_n		=> T80_M1_N,
	MREQ_n	=> T80_MREQ_N,
	IORQ_n	=> T80_IORQ_N,
	RD_n		=> T80_RD_N,
	WR_n		=> T80_WR_N,
	RFSH_n	=> T80_RFSH_N,
	HALT_n	=> T80_HALT_N,
	BUSAK_n	=> T80_BUSAK_N,
	A			=> T80_A,
	DI			=> T80_DI,
	DO			=> T80_DO
);

-- OS ROM
os : entity work.os_rom
port map(
	A			=> FX68_A(8 downto 1),
	OEn		=> OS_OEn,
	D			=> FX68_OS_D
);

-- I/O
io : entity work.gen_io
port map(
	RST_N		=> MRST_N,
	CLK			=> MCLK,

	J3BUT		=> JOY_3BUT,
	P1_UP		=> not JOY_1(3),
	P1_DOWN		=> not JOY_1(2),
	P1_LEFT		=> not JOY_1(1),
	P1_RIGHT	=> not JOY_1(0),
	P1_A		=> not JOY_1(4),
	P1_B		=> not JOY_1(5),
	P1_C		=> not JOY_1(6),
	P1_START	=> not JOY_1(7),
	P1_X		=> not JOY_1(8),
	P1_Y		=> not JOY_1(9),
	P1_Z		=> not JOY_1(10),
	P1_MODE		=> not JOY_1(11),

	P2_UP		=> not JOY_2(3),
	P2_DOWN		=> not JOY_2(2),
	P2_LEFT		=> not JOY_2(1),
	P2_RIGHT	=> not JOY_2(0),
	P2_A		=> not JOY_2(4),
	P2_B		=> not JOY_2(5),
	P2_C		=> not JOY_2(6),
	P2_START	=> not JOY_2(7),
	P2_X		=> not JOY_2(8),
	P2_Y		=> not JOY_2(9),
	P2_Z		=> not JOY_2(10),
	P2_MODE		=> not JOY_2(11),

	MSEL		=> MSEL,
	mouse_x		=> mouse_x,
	mouse_y		=> MOUSE_Y_ADJ(7 downto 0),
	mouse_flags => mouse_flags(7 downto 6) & MOUSE_Y_ADJ(8) & mouse_flags(4 downto 0),
	mouse_strobe => mouse_strobe,

	SEL		=> IO_SEL,
	A			=> IO_A,
	RNW		=> IO_RNW,
	UDS_N		=> IO_UDS_N,
	LDS_N		=> IO_LDS_N,
	DI			=> IO_DI,
	DO			=> IO_DO,
	DTACK_N		=> IO_DTACK_N,

	PAL			=> PAL,
	PAL_OUT		=> PAL_IO,
	MODEL		=> model
);

-- VDP

vdp : entity work.vdp
port map(
	RST_N		=> MRST_N and VDP_RST_N,
	CLK		=> MCLK,

	SEL		=> VDP_SEL,
	A			=> VDP_A,
	RNW		=> VDP_RNW,
	DI			=> VDP_DI,
	DO			=> VDP_DO,
	DTACK_N		=> VDP_DTACK_N,

	vram_req => vram_req,
	vram_ack => vram_ack,
	vram_we	=> vram_we,
	vram_a	=> vram_a,
	vram_d	=> vram_d,
	vram_q	=> vram_q,
	vram_u_n	=> vram_u_n,
	vram_l_n	=> vram_l_n,

	vram32_req => vram32_req,
	vram32_ack => vram32_ack,
	vram32_a   => vram32_a,
	vram32_q   => vram32_q,

	HINT			=> HINT,
	VINT_TG68		=> VINT_FX68,
	VINT_T80			=> VINT_T80,
	INTACK			=> FX68_INTACK,
	BR_N		=> VDP_BR_N,
	BG_N		=> FX68_BG_N,
	BGACK_N		=> VDP_BGACK_N,

	VBUS_ADDR		=> VBUS_ADDR,
	VBUS_DATA		=> VBUS_DATA,
		
	VBUS_SEL			=> VBUS_SEL,
	VBUS_DTACK_N	=> VBUS_DTACK_N,

	PAL					=> PAL_IO,
	R					=> VDP_RED,
	G					=> VDP_GREEN,
	B					=> VDP_BLUE,
	HS					=> VDP_HS_N,
	VS					=> VDP_VS_N,

	VRAM_SPEED			=> VDP_VRAM_SPEED,
	VSCROLL_BUG			=> '0',
	BORDER_EN			=> BORDER
);

-- PSG

u_psg : jt89
port map(
	rst    	=> not MRST_N,
	clk		=> MCLK,
	clk_en	=> ZCLK_ENA,
	wr_n	=> not PSG_SEL,
	din  	=> PSG_DI,
	sound	=> PSG_SND
);

-- FM

fm : jt12
port map(
	rst		=> not T80_RESET_N,
	clk		=> MCLK,
	cen		=> FCLK_EN,
	addr	=> FM_A,
	cs_n	=> '0',
	wr_n	=> FM_RNW,
	din		=> FM_DI,
	dout	=> FM_DO,
	-- Real time configuration
	en_hifi_pcm => FM_HIFI,

	snd_left  => FM_LEFT,
	snd_right => FM_RIGHT
);

-- Audio control
PSG_ENABLE <= not SW(3);
FM_ENABLE  <= not SW(4);
FM_HIFI    <=     SW(13);

genmix : jt12_genmix
port map(
	rst		=> not T80_RESET_N,
	clk		=> MCLK,
    fm_left => FM_LEFT,
    fm_right=> FM_RIGHT,
    psg_snd => PSG_SND,
    fm_en   => FM_ENABLE,
    psg_en  => PSG_ENABLE,
    -- Mixed sound at 54 MHz
    snd_left  => DAC_LDATA,
    snd_right => DAC_RDATA
);

SVP_ENABLE <= SW(1);
-- SVP at A15000-A1500F
FX68_SVP_SEL <= '1' when SVP_ENABLE = '1' and FX68_SEL = '1' and FX68_A(23 downto 4) = x"A1500" else '0';
T80_SVP_SEL <= '1' when BAR(23 downto 15) = x"A1"&'0' and T80_A(15 downto 4) = x"500" and T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';
SVP_SEL <= T80_SVP_SEL or FX68_SVP_SEL;
SVP_RNW <= T80_WR_N when T80_SVP_SEL = '1' else FX68_RNW when FX68_SVP_SEL = '1' else '1';
SVP_A <= T80_A(3 downto 1) when T80_SVP_SEL = '1' else FX68_A(3 downto 1);
SVP_DI <= T80_DO & T80_DO when T80_SVP_SEL = '1' else FX68_DO;

SVP : work.SVP
port map (
	CLK         => MCLK,
	CE          => SVP_CLKEN,
	RST_N       => MRST_N,
	ENABLE      => SVP_ENABLE,

	BUS_A       => SVP_A,
	BUS_DO      => SVP_DO,
	BUS_DI      => SVP_DI,
	BUS_SEL     => SVP_SEL,
	BUS_RNW     => SVP_RNW,
	BUS_DTACK_N => SVP_DTACK_N,

	ROM_A       => svp_rom_a,
	ROM_DI      => svp_rom_q,
	ROM_REQ     => svp_rom_req,
	ROM_ACK     => svp_rom_ack,

	DRAM_A      => svp_ram1_a,
	DRAM_DI     => svp_ram1_q,
	DRAM_DO     => svp_ram1_d,
	DRAM_WE     => svp_ram1_we,
	DRAM_REQ    => svp_ram1_req,
	DRAM_ACK    => svp_ram1_ack
);

-- #############################################################################
-- #############################################################################
-- #############################################################################

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_IO_READY <= '1';
	elsif rising_edge(MCLK) then
		FX68_AS_N_D <= FX68_AS_N;
		FX68_RNW_D <= FX68_RNW;

		if FX68_AS_N_D = '1' and FX68_AS_N = '0' then
			FX68_IO_READY <= '1';
		end if;
		if FX68_RNW_D = '1' and FX68_RNW = '0' and FX68_AS_N_D = '0' then
			FX68_IO_READY <= '0'; -- break read-modify-write cycle (TAS instruction)
		end if;
	end if;
end process;

FX68_SEL <= '1' when FX68_AS_N = '0' and (FX68_UDS_N = '0' or FX68_LDS_N = '0') else '0';

----------------------------------------------------------------
-- INTERRUPTS CONTROL
----------------------------------------------------------------

T80_INT_N <= not VINT_T80;
--FX68_IPL_N <= "001" when VINT_FX68 = '1' else "011" when HINT = '1' else "111";
FX68_VPA_N <= '0' when FX68_INTACK = '1' else '1';
FX68_INTACK <= '1' when FX68_FC = "111" else '0';

process( MCLK )
begin
	if rising_edge(MCLK) then
		-- some delay between the VDP and CPU interrupt lines
		-- makes Fatal Rewind happy
		-- probably it should belong to the CPU
--		if FX68_CYCLE = '1' and FX68_BUS_WAIT = "00" then
		if FX68_PHI1 = '1' and FX68_AS_N = '1' then
			if VINT_FX68 = '1' then
				FX68_IPL_N <= "001";
			elsif HINT = '1' then
				FX68_IPL_N <= "011";
			else
				FX68_IPL_N <= "111";
			end if;
		end if;
	end if;
end process;

----------------------------------------------------------------
-- SWITCHES CONTROL
----------------------------------------------------------------

-- #############################################################################
-- #############################################################################
-- #############################################################################

-- CLOCK GENERATION
process( MRST_N, MCLK, VCLKCNT )
begin
	if MRST_N = '0' then
		VCLKCNT <= "001"; -- important for SDRAM controller (EDIT: not needed anymore)
		ZCLKCNT <= (others => '0');
		RFRSH_CNT <= (others => '0');
		RFRSH_DELAY <= '0';
		SVP_CLKEN <= '0';
		MCLKCNT <= (others => '0');

	elsif rising_edge(MCLK) then
		ZCLKCNT <= ZCLKCNT + 1;
		if ZCLKCNT = "1110" then
			ZCLKCNT <= (others => '0');
		end if;

		if ZCLKCNT = "0000" then
			ZCLK_ENA <= '1';
		else
			ZCLK_ENA <= '0';
		end if;

		if ZCLKCNT = "1000" then
			ZCLK_nENA <= '1';
		else
			ZCLK_nENA <= '0';
		end if;

		-- hack for syncing VDP and CPU, until the original timings can be fully reached
		MCLKCNT <= MCLKCNT + 1;
		if MCLKCNT = 3419 then
			MCLKCNT <= (others => '0');
		end if;

		VCLKCNT <= VCLKCNT + 1;
		if VCLKCNT = "110" or MCLKCNT >= 3416 then
			VCLKCNT <= "000";
		end if;
		if VCLKCNT = "011" then
			FCLK_EN <= '1';
		else
			FCLK_EN <= '0';
		end if;

		-- "The 68k is a bit slower than expected because something on the bus steals 2 out of every 128 cycles.
		-- This is visible when measuring the bus because every time 128 cycles have passed the next bus access is
		-- slowed down by 2 cycles.
		-- Interestingly if this hits when the 68k is writing to the VDP (no matter which port) then the slowdown doesn't happen."
		-- (From Titan 2 tech doc)
		if VCLKCNT = "110" then
			if (RFRSH_CNT(7) = '1' and FX68_DTACK_N = '0') or VDP_BGACK_N = '0' then
				RFRSH_CNT <= (others => '0');
			elsif RFRSH_DELAY = '0' then
				RFRSH_CNT <= RFRSH_CNT + 1;
			end if;
		end if;

		if VCLKCNT = "000" then
			if RFRSH_CNT(7) = '1' and FX68_AS_N = '1' then
				RFRSH_DELAY <= '1';
			else
				RFRSH_DELAY <= '0';
			end if;
		end if;

		if FX68_DTACK_N = '0' then
			RFRSH_DELAY <= '0';
		end if;

		if VCLKCNT = "011" or (CPU_TURBO = '1' and VCLKCNT = "110") then
			FX68_PHI1 <= '1';
		else
			FX68_PHI1 <= '0';
		end if;

		if VCLKCNT = "001" or (CPU_TURBO = '1' and VCLKCNT = "100") then
			FX68_PHI2 <= '1';
		else
			FX68_PHI2 <= '0';
		end if;

		SVP_CLKEN <= not SVP_CLKEN;

     end if;
end process;

-- DMA VBUS
VBUS_DTACK_N <= DMA_FLASH_DTACK_N when DMA_FLASH_SEL = '1'
	else DMA_SDRAM_DTACK_N when DMA_SDRAM_SEL = '1'
	else DMA_SVP_RAM_DTACK_N when DMA_SVP_RAM_SEL = '1'
	else '0';
VBUS_DATA <= DMA_FLASH_D when DMA_FLASH_SEL = '1'
	else DMA_SDRAM_D when DMA_SDRAM_SEL = '1'
	else DMA_SVP_RAM_D when DMA_SVP_RAM_SEL = '1'
	else x"FFFF";

-- 68K INPUTS
FX68_RES_N <= MRST_N;

FX68_DTACK_N <= '1' when bootState /= BOOT_DONE
	else FX68_FLASH_DTACK_N when FX68_FLASH_SEL = '1'
	else FX68_SDRAM_DTACK_N when FX68_SDRAM_SEL = '1'
	else FX68_ZRAM_DTACK_N when FX68_ZRAM_SEL = '1'
	else FX68_SRAM_DTACK_N when FX68_SRAM_SEL = '1'
	else FX68_CTRL_DTACK_N when FX68_CTRL_SEL = '1' 
	else FX68_OS_DTACK_N when FX68_OS_SEL = '1' 
	else FX68_IO_DTACK_N when FX68_IO_SEL = '1' 
	else FX68_BAR_DTACK_N when FX68_BAR_SEL = '1' 
	else FX68_VDP_DTACK_N when FX68_VDP_SEL = '1'
	else FX68_SVP_RAM_DTACK_N when FX68_SVP_RAM_SEL = '1'
	else SVP_DTACK_N when FX68_SVP_SEL = '1'
	else '0' when FX68_FM_SEL = '1'
	else '0' when FX68_EEPROM_SEL = '1'
	else '0' when FX68_TIME_SEL = '1'
	else '1';
FX68_DI(15 downto 8) <= FX68_FLASH_D(15 downto 8) when FX68_FLASH_SEL = '1' and FX68_UDS_N = '0'
	else FX68_SDRAM_D(15 downto 8) when FX68_SDRAM_SEL = '1' and FX68_UDS_N = '0'
	else FX68_SRAM_D(15 downto 8) when FX68_SRAM_SEL = '1' and FX68_UDS_N = '0'
	else FX68_ZRAM_D(15 downto 8) when FX68_ZRAM_SEL = '1' and FX68_UDS_N = '0'
	else FX68_CTRL_D(15 downto 8) when FX68_CTRL_SEL = '1' and FX68_UDS_N = '0'
	else FX68_OS_D(15 downto 8) when FX68_OS_SEL = '1' and FX68_UDS_N = '0'
	else FX68_IO_D(15 downto 8) when FX68_IO_SEL = '1' and FX68_UDS_N = '0'
	else FX68_BAR_D(15 downto 8) when FX68_BAR_SEL = '1' and FX68_UDS_N = '0'
	else FX68_VDP_D(15 downto 8) when FX68_VDP_SEL = '1' and FX68_UDS_N = '0'
	else FX68_FM_D(15 downto 8) when FX68_FM_SEL = '1' and FX68_UDS_N = '0'
	else FX68_EEPROM_DATA(15 downto 8) when FX68_EEPROM_SEL = '1' and FX68_UDS_N = '0'
	else FX68_SVP_RAM_D(15 downto 8) when FX68_SVP_RAM_SEL = '1' and FX68_UDS_N = '0'
	else SVP_DO(15 downto 8) when FX68_SVP_SEL = '1' and FX68_UDS_N = '0'
	else NO_DATA(15 downto 8);
FX68_DI(7 downto 0) <= FX68_FLASH_D(7 downto 0) when FX68_FLASH_SEL = '1' and FX68_LDS_N = '0'
	else FX68_SDRAM_D(7 downto 0) when FX68_SDRAM_SEL = '1' and FX68_LDS_N = '0'
	else FX68_SRAM_D(7 downto 0) when FX68_SRAM_SEL = '1' and FX68_LDS_N = '0'
	else FX68_ZRAM_D(7 downto 0) when FX68_ZRAM_SEL = '1' and FX68_LDS_N = '0'
	else FX68_CTRL_D(7 downto 0) when FX68_CTRL_SEL = '1' and FX68_LDS_N = '0'
	else FX68_OS_D(7 downto 0) when FX68_OS_SEL = '1' and FX68_LDS_N = '0'
	else FX68_IO_D(7 downto 0) when FX68_IO_SEL = '1' and FX68_LDS_N = '0'
	else FX68_BAR_D(7 downto 0) when FX68_BAR_SEL = '1' and FX68_LDS_N = '0'
	else FX68_VDP_D(7 downto 0) when FX68_VDP_SEL = '1' and FX68_LDS_N = '0'
	else FX68_FM_D(7 downto 0) when FX68_FM_SEL = '1' and FX68_LDS_N = '0'
	else FX68_EEPROM_DATA(7 downto 0) when FX68_EEPROM_SEL = '1' and FX68_LDS_N = '0'
	else FX68_SVP_RAM_D(7 downto 0) when FX68_SVP_RAM_SEL = '1' and FX68_LDS_N = '0'
	else SVP_DO(7 downto 0) when FX68_SVP_SEL = '1' and FX68_LDS_N = '0'
	else NO_DATA(7 downto 0);

-- Floating bus
process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		NO_DATA <= x"4E71";
	elsif rising_edge( MCLK ) then
		if FX68_FLASH_SEL = '1' then
			NO_DATA <= FX68_FLASH_D;
		elsif FX68_SDRAM_SEL = '1' then
			NO_DATA <= FX68_SDRAM_D;
		end if;
	end if;
end process;

-- Z80 INPUTS
process(MRST_N, MCLK)
begin
	if MRST_N = '0' then
		T80_RESET_N <= '0';
	elsif rising_edge(MCLK) then
		T80_RESET_N <= ZRESET_N;
		ZBUSACK_N <= T80_BUSAK_N;
		T80_BUSRQ_N <= not ZBUSREQ;
	end if;
end process;

T80_NMI_N <= '1';

T80_WAIT_N <= '0' when bootState /= BOOT_DONE
	else not T80_SDRAM_DTACK_N when T80_SDRAM_SEL = '1'
	else not T80_FLASH_DTACK_N when T80_FLASH_SEL = '1'
	else not T80_CTRL_DTACK_N when T80_CTRL_SEL = '1' 
	else not T80_IO_DTACK_N when T80_IO_SEL = '1' 
	else not T80_BAR_DTACK_N when T80_BAR_SEL = '1'
	else not T80_VDP_DTACK_N when T80_VDP_SEL = '1'
	else '1';
T80_DI <= T80_SDRAM_D when T80_SDRAM_SEL = '1'
	else T80_ZRAM_D when T80_ZRAM_SEL = '1'
	else T80_FLASH_D when T80_FLASH_SEL = '1'
	else T80_CTRL_D when T80_CTRL_SEL = '1'
	else T80_IO_D when T80_IO_SEL = '1'
	else T80_BAR_D when T80_BAR_SEL = '1'
	else T80_VDP_D when T80_VDP_SEL = '1'
	else T80_FM_D when T80_FM_SEL = '1'
	else SVP_DO(7 downto 0) when T80_SVP_SEL = '1'
	else x"FF";

-- OPERATING SYSTEM ROM
FX68_OS_DTACK_N <= '0';
OS_OEn <= '0';
FX68_OS_SEL <= '1' when FX68_A(23 downto 22) = "00" and FX68_AS_N = '0' and FX68_RNW = '1' and CART_EN = '0' else '0';

-- CONTROL AREA
FX68_CTRL_SEL <= '1' when (FX68_A(23 downto 12) = x"A11" or FX68_A(23 downto 12) = x"A14") and
	FX68_SEL = '1' else '0';
T80_CTRL_SEL <= '1' when T80_A(15) = '1' and 
	(BAR(23 downto 15) & T80_A(14 downto 12) = x"A11" or BAR(23 downto 15) & T80_A(14 downto 12) = x"A14") and
	T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_CTRL_DTACK_N <= '1';	
		T80_CTRL_DTACK_N <= '1';	
		
		ZBUSREQ <= '0';
		ZRESET_N <= '0';
		CART_EN <= '0';
		
	elsif rising_edge(MCLK) then
		if FX68_CTRL_SEL = '0' then 
			FX68_CTRL_DTACK_N <= '1';
		end if;
		if T80_CTRL_SEL = '0' then 
			T80_CTRL_DTACK_N <= '1';
		end if;
		
		if FX68_CTRL_SEL = '1' and FX68_CTRL_DTACK_N = '1' then
			FX68_CTRL_DTACK_N <= '0';
			if FX68_RNW = '0' then
				-- Write
				if FX68_A(15 downto 8) = x"11" then
					-- ZBUSREQ
					if FX68_UDS_N = '0' then
						ZBUSREQ <= FX68_DO(8);
					end if;
				elsif FX68_A(15 downto 8) = x"12" then
					-- ZRESET_N
					if FX68_UDS_N = '0' then
						ZRESET_N <= FX68_DO(8);
					end if;			
				elsif FX68_A(15 downto 8) = x"41" then
					-- Cartridge Control Register
					if FX68_LDS_N = '0' then
						CART_EN <= FX68_DO(0);
					end if;								
				end if;
			else
				-- Read
				FX68_CTRL_D <= NO_DATA;
				if FX68_A(15 downto 8) = x"11" then
					-- ZBUSACK_N
					FX68_CTRL_D(8) <= ZBUSACK_N;
				end if;
			end if;		
		elsif T80_CTRL_SEL = '1' and T80_CTRL_DTACK_N = '1' then
			T80_CTRL_DTACK_N <= '0';
			if T80_WR_N = '0' then
				-- Write
				if BAR(15) & T80_A(14 downto 8) = x"11" then
					-- ZBUSREQ
					if T80_A(0) = '0' then
						ZBUSREQ <= T80_DO(0);
					end if;
				elsif BAR(15) & T80_A(14 downto 8) = x"12" then
					-- ZRESET_N
					if T80_A(0) = '0' then
						ZRESET_N <= T80_DO(0);
					end if;			
				elsif BAR(15) & T80_A(14 downto 8) = x"41" then
					-- Cartridge Control Register
					if T80_A(0) = '1' then
						CART_EN <= T80_DO(0);
					end if;								
				end if;
			else
				-- Read
				T80_CTRL_D <= not T80_CTRL_D;
				if BAR(15) & T80_A(14 downto 8) = x"11" and T80_A(0) = '0' then
					-- ZBUSACK_N
					T80_CTRL_D(0) <= ZBUSACK_N;
				end if;
			end if;			
		end if;
		
	end if;
	
end process;

-- I/O AREA
FX68_IO_SEL <= '1' when FX68_A(23 downto 5) = x"A100" & "000" and FX68_SEL = '1' else '0';
T80_IO_SEL <= '1' when T80_A(15) = '1' and BAR & T80_A(14 downto 5) = x"A100" & "000" and
	T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_IO_DTACK_N <= '1';	
		T80_IO_DTACK_N <= '1';	
		
		IO_SEL <= '0';
		IO_RNW <= '1';
		IO_UDS_N <= '1';
		IO_LDS_N <= '1';

		IOC <= IOC_IDLE;
		
	elsif rising_edge(MCLK) then
		if FX68_IO_SEL = '0' then 
			FX68_IO_DTACK_N <= '1';
		end if;
		if T80_IO_SEL = '0' then 
			T80_IO_DTACK_N <= '1';
		end if;

		case IOC is
		when IOC_IDLE =>
			if FX68_IO_SEL = '1' and FX68_IO_DTACK_N = '1' then
				IO_SEL <= '1';
				IO_A <= FX68_A(4 downto 1) & '0';
				IO_RNW <= FX68_RNW;
				IO_UDS_N <= FX68_UDS_N;
				IO_LDS_N <= FX68_LDS_N;
				IO_DI <= FX68_DO;
				IOC <= IOC_FX68_ACC;
			elsif T80_IO_SEL = '1' and T80_IO_DTACK_N = '1' then
				IO_SEL <= '1';
				IO_A <= T80_A(4 downto 0);
				IO_RNW <= T80_WR_N;
				if T80_A(0) = '0' then
					IO_UDS_N <= '0';
					IO_LDS_N <= '1';
				else
					IO_UDS_N <= '1';
					IO_LDS_N <= '0';				
				end if;
				IO_DI <= T80_DO & T80_DO;
				IOC <= IOC_T80_ACC;			
			end if;

		when IOC_FX68_ACC =>
			if IO_DTACK_N = '0' then
				IO_SEL <= '0';
				FX68_IO_D <= IO_DO;
				FX68_IO_DTACK_N <= '0';
				IOC <= IOC_DESEL;
			end if;

		when IOC_T80_ACC =>
			if IO_DTACK_N = '0' then
				IO_SEL <= '0';
				if T80_A(0) = '0' then
					T80_IO_D <= IO_DO(15 downto 8);
				else
					T80_IO_D <= IO_DO(7 downto 0);
				end if;
				T80_IO_DTACK_N <= '0';
				IOC <= IOC_DESEL;
			end if;
		
		when IOC_DESEL =>
			if IO_DTACK_N = '1' then
				IO_RNW <= '1';
				IO_UDS_N <= '1';
				IO_LDS_N <= '1';
				IO_A <= (others => 'Z');

				IOC <= IOC_IDLE;
			end if;
		
		when others => null;
		end case;
	end if;
	
end process;


-- VDP in Z80 address space :
-- Z80:
-- 7F = 01111111 000
-- 68000:
-- 7F = 01111111 000
-- FF = 11111111 000
-- VDP AREA
FX68_VDP_SEL <= '1' when ((FX68_AS_N = '0' and FX68_RNW = '0') or FX68_SEL = '1') and
	((FX68_A(23 downto 21) = "110" and FX68_A(18 downto 16) = "000") or
	 (FX68_A(23 downto 16) = x"A0" and FX68_A(14 downto 5) = "1111111" & "000")) -- Z80 Address space
	else '0';
T80_VDP_SEL <= '1' when T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') and
	(T80_A(15 downto 5) = x"7F" & "000" or
	(T80_A(15) = '1' and BAR(23 downto 21) = "110" and BAR(18 downto 16) = "000"))	-- 68000 Address space
	else '0';

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_VDP_DTACK_N <= '1';	
		T80_VDP_DTACK_N <= '1';	

		VDP_SEL <= '0';
		VDP_RNW <= '1';

		VDPC <= VDPC_IDLE;

	elsif rising_edge(MCLK) then
		if FX68_VDP_SEL = '0' then 
			FX68_VDP_DTACK_N <= '1';
		end if;
		if T80_VDP_SEL = '0' then 
			T80_VDP_DTACK_N <= '1';
		end if;

		case VDPC is
		when VDPC_IDLE =>
			if FX68_VDP_SEL = '1' and FX68_VDP_DTACK_N = '1' then
				VDP_SEL <= '1';
				VDP_A <= FX68_A(4 downto 1) & '0';
				VDP_RNW <= FX68_RNW;
				VDP_DI <= FX68_DO;
				VDPC <= VDPC_FX68_ACC;
			elsif T80_VDP_SEL = '1' and T80_VDP_DTACK_N = '1' then
				VDP_SEL <= '1';
				VDP_A <= T80_A(4 downto 0);
				VDP_RNW <= T80_WR_N;
				VDP_DI <= T80_DO & T80_DO;
				VDPC <= VDPC_T80_ACC;
			end if;

		when VDPC_FX68_ACC =>
			if VDP_DTACK_N = '0' then
				VDP_SEL <= '0';
				if VDP_A(4 downto 2) = "001" then
					-- status register
					FX68_VDP_D <= NO_DATA(15 downto 10) & VDP_DO(9 downto 0);
				elsif VDP_A(4) = '1' then
					-- unused, PSG, debug register
					FX68_VDP_D <= NO_DATA;
				else
					FX68_VDP_D <= VDP_DO;
				end if;
				FX68_VDP_DTACK_N <= '0';
				VDPC <= VDPC_IDLE;
			end if;

		when VDPC_T80_ACC =>
			if VDP_DTACK_N = '0' then
				VDP_SEL <= '0';
				if T80_A(0) = '0' then
					T80_VDP_D <= VDP_DO(15 downto 8);
				else
					T80_VDP_D <= VDP_DO(7 downto 0);
				end if;
				T80_VDP_DTACK_N <= '0';
				VDPC <= VDPC_IDLE;
			end if;

		when others => null;
		end case;
	end if;
	
end process;

-- Z80:
-- 40 = 01000000
-- 5F = 01011111
-- 68000:
-- 40 = 01000000
-- 5F = 01011111
-- C0 = 11000000
-- DF = 11011111
-- FM AREA
FX68_FM_SEL <= '1' when  FX68_A(23 downto 16) = x"A0" and FX68_A(14 downto 13) = "10" and FX68_SEL = '1' else '0';
T80_FM_SEL <= '1' when T80_A(15 downto 13) = "010" and T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';
FM_SEL <= T80_FM_SEL or FX68_FM_SEL;
FM_RNW <= T80_WR_N when T80_FM_SEL = '1' else FX68_RNW when FX68_FM_SEL = '1' else '1';
FM_A <= T80_A(1 downto 0) when T80_FM_SEL = '1' else FX68_A(1) & not FX68_LDS_N;
FM_DI <= T80_DO when T80_FM_SEL = '1' else FX68_DO(15 downto 8) when FX68_UDS_N = '0' else FX68_DO(7 downto 0);
FX68_FM_D <= FM_DO & FM_DO;
T80_FM_D <= FM_DO;

-- PSG AREA
-- Z80: 7F11h
-- 68k: C00011

T80_PSG_SEL <= '1' when T80_VDP_SEL = '1' and T80_A(4 downto 3) = "10" and T80_MREQ_N = '0' and T80_WR_N = '0' else '0';
FX68_PSG_SEL <= '1' when FX68_VDP_SEL = '1' and FX68_A(4 downto 3) = "10" and FX68_RNW='0' else '0';
PSG_SEL <= T80_PSG_SEL or FX68_PSG_SEL;
PSG_DI <= T80_DO when T80_PSG_SEL = '1' else FX68_DO(7 downto 0);

-- Z80:
-- 60 = 01100000
-- 7E = 01111110
-- 68000:
-- 60 = 01100000
-- 7E = 01111110
-- E0 = 11100000
-- FE = 11111110
-- BANK ADDRESS REGISTER AND UNUSED AREA IN Z80 ADDRESS SPACE
FX68_BAR_SEL <= '1' when (FX68_A(23 downto 16) = x"A0" and FX68_A(14 downto 13) = "11" and FX68_A(12 downto 8) /= "11111")
		and FX68_SEL = '1' else '0';
T80_BAR_SEL <= '1' when (T80_A(15 downto 13) = "011" and T80_A(12 downto 8) /= "11111")
		and T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_BAR_DTACK_N <= '1';	
		T80_BAR_DTACK_N <= '1';
		
		BAR <= (others => '0');
		
	elsif rising_edge(MCLK) then
		if FX68_BAR_SEL = '0' then 
			FX68_BAR_DTACK_N <= '1';
		end if;
		if T80_BAR_SEL = '0' then 
			T80_BAR_DTACK_N <= '1';
		end if;

		if FX68_BAR_SEL = '1' and FX68_BAR_DTACK_N = '1' then
			if FX68_RNW = '0' then
				if FX68_A(23 downto 16) = x"A0" and FX68_A(14 downto 8) = "1100000" and FX68_UDS_N = '0' then
					BAR <= FX68_DO(8) & BAR(23 downto 16);
				end if;
			else
				FX68_BAR_D <= x"FFFF";
			end if;
			FX68_BAR_DTACK_N <= '0';
		elsif T80_BAR_SEL = '1' and T80_BAR_DTACK_N = '1' then
			if T80_WR_N = '0' then
				if T80_A(15 downto 8) = x"60" then
					BAR <= T80_DO(0) & BAR(23 downto 16);
				end if;
			else
				T80_BAR_D <= x"FF";
			end if;
			T80_BAR_DTACK_N <= '0';
		end if;
	end if;
end process;

-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- MiST Memory Handling
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- !TIME control (SSF2 mapper / SRAM page in/out)
FX68_TIME_SEL <= '1' when FX68_A(23 downto 8) = x"a130" and FX68_SEL = '1' else '0';

process( MRST_N, MCLK )
begin
    if rising_edge( MCLK ) then
        if ( MRST_N = '0' ) then
            SSF2_USE_MAP <= '0';
            SRAM_EN_PAGEIN <= '0';
            SSF2_MAP( 5 downto  0) <= "00"&x"0";
            SSF2_MAP(11 downto  6) <= "00"&x"1";
            SSF2_MAP(17 downto 12) <= "00"&x"2";
            SSF2_MAP(23 downto 18) <= "00"&x"3";
            SSF2_MAP(29 downto 24) <= "00"&x"4";
            SSF2_MAP(35 downto 30) <= "00"&x"5";
            SSF2_MAP(41 downto 36) <= "00"&x"6";
            SSF2_MAP(47 downto 42) <= "00"&x"7";
        elsif FX68_A(23 downto 4) = x"a130f" and FX68_SEL = '1' and FX68_RNW = '0' then
            if BIG_CART = '1' then
                -- use for ROM bank switch when cart > 4 MB
                SSF2_USE_MAP <= '1';
                case FX68_A(3 downto 1) is
                when "000" =>
                    null; -- always 0;
                when "001" =>
                    SSF2_MAP(11 downto 6) <= FX68_DO(5 downto 0);
                when "010" =>
                    SSF2_MAP(17 downto 12) <= FX68_DO(5 downto 0);
                when "011" =>
                    SSF2_MAP(23 downto 18) <= FX68_DO(5 downto 0);
                when "100" =>
                    SSF2_MAP(29 downto 24) <= FX68_DO(5 downto 0);
                when "101" =>
                    SSF2_MAP(35 downto 30) <= FX68_DO(5 downto 0);
                when "110" =>
                    SSF2_MAP(41 downto 36) <= FX68_DO(5 downto 0);
                when "111" =>
                    SSF2_MAP(47 downto 42) <= FX68_DO(5 downto 0);
                end case;
            else
                -- use for SRAM paging
                SRAM_EN_PAGEIN <= FX68_DO(0);
            end if;
        end if;
    end if;
end process;

ROM_PAGE <= FX68_A(21 downto 19) when FX68_FLASH_SEL = '1' and FX68_DTACK_N = '1'
          else BAR(21 downto 19) when T80_FLASH_SEL = '1' and T80_FLASH_DTACK_N = '1'
          else VBUS_ADDR(21 downto 19);

ROM_PAGE_A <= FX68_A(23 downto 19) when FX68_FLASH_SEL = '1' and FX68_DTACK_N = '1' and SSF2_USE_MAP = '0' else
              BAR(23 downto 19) when T80_FLASH_SEL = '1' and T80_FLASH_DTACK_N = '1' and SSF2_USE_MAP = '0' else
              VBUS_ADDR(23 downto 19) when SSF2_USE_MAP = '0' else
              SSF2_MAP(4 downto 0) when ROM_PAGE = "000" else
              SSF2_MAP(10 downto 6) when ROM_PAGE = "001" else
              SSF2_MAP(16 downto 12) when ROM_PAGE = "010" else
              SSF2_MAP(22 downto 18) when ROM_PAGE = "011" else
              SSF2_MAP(28 downto 24) when ROM_PAGE = "100" else
              SSF2_MAP(34 downto 30) when ROM_PAGE = "101" else
              SSF2_MAP(40 downto 36) when ROM_PAGE = "110" else
              SSF2_MAP(46 downto 42);

-- FLASH (SDRAM) CONTROL
-- 68000: 000000 - 9fffff
-- Z80  : 000000 - 9fffff
-- DMA  : 000000 - 9fffff

FX68_FLASH_SEL <= '1' when (FX68_A(23) = '0' or FX68_A(23 downto 21) = "100") and FX68_SEL = '1' and
	FX68_SRAM_SEL = '0' and FX68_SRAM_SEL = '0' and FX68_EEPROM_SEL = '0' and FX68_SVP_RAM_SEL = '0' and CART_EN = '1' else '0';
T80_FLASH_SEL <= '1' when T80_A(15) = '1' and T80_MREQ_N = '0' and T80_RD_N = '0' and (BAR(23) = '0' or BAR(23 downto 21) = "100")
	else '0';
DMA_FLASH_SEL <= '1' when (VBUS_ADDR(23) = '0' or VBUS_ADDR(23 downto 21) = "100") and VBUS_SEL = '1' and DMA_SVP_RAM_SEL = '0' else '0';

DMA_FLASH_DTACK_N  <= '0' when FC = FC_DMA_RD and romrd_req = romrd_ack else DMA_FLASH_DTACK_N_REG;
--DMA_FLASH_DTACK_N  <= DMA_FLASH_DTACK_N_REG;
DMA_FLASH_D <= romrd_q when FC = FC_DMA_RD and romrd_req = romrd_ack else DMA_FLASH_D_REG;

process( MRST_N, MCLK )
variable dma_a : std_logic_vector(23 downto 1);
begin
	if MRST_N = '0' then
		FC <= FC_IDLE;
		
		FX68_FLASH_DTACK_N <= '1';
		T80_FLASH_DTACK_N <= '1';
		DMA_FLASH_DTACK_N_REG <= '1';
--		T80_FLASH_BR_N <= '1';
--		T80_FLASH_BGACK_N <= '1';

		romrd_req <= '0';
		
	elsif rising_edge( MCLK ) then
		if FX68_FLASH_SEL = '0' then 
			FX68_FLASH_DTACK_N <= '1';
		end if;
		if T80_FLASH_SEL = '0' then 
			T80_FLASH_DTACK_N <= '1';
		end if;
		if DMA_FLASH_SEL = '0' then 
			DMA_FLASH_DTACK_N_REG <= '1';
		end if;

		case FC is
		when FC_IDLE =>			
			--if VCLKCNT = "001" then

				if FX68_FLASH_SEL = '1' and FX68_FLASH_DTACK_N = '1' then
					romrd_req <= not romrd_req;
					romrd_a <= ROM_PAGE_A & FX68_A(18 downto 1);
					FC_DELAY <= "00";
					if RFRSH_DELAY = '1' then FC_DELAY <= "10"; end if;
					FC <= FC_FX68_RD;
				elsif T80_FLASH_SEL = '1' and T80_FLASH_DTACK_N = '1' then
					romrd_a <= ROM_PAGE_A & BAR(18 downto 15) & T80_A(14 downto 1);
--					T80_FLASH_BR_N <= '0';
					FC <= FC_T80_BR;
				elsif DMA_FLASH_SEL = '1' and DMA_FLASH_DTACK_N_REG = '1' then
					if SVP_ENABLE = '1' then
						dma_a := VBUS_ADDR - 1;
					else
						dma_a := VBUS_ADDR;
					end if;
					romrd_req <= not romrd_req;
					romrd_a <= ROM_PAGE_A & dma_a(18 downto 1);
					FC <= FC_DMA_RD;
				end if;
			--end if;

		when FC_FX68_RD =>
			if FX68_PHI2 = '1' and FC_DELAY /= "00" then
				FC_DELAY <= FC_DELAY - 1;
			end if;
			if romrd_req = romrd_ack and FC_DELAY = "00" then
				FX68_FLASH_D <= romrd_q;
				FX68_FLASH_DTACK_N <= '0';
				FC <= FC_IDLE;
			end if;

		when FC_T80_BR =>
--			if FX68_BG_N = '0' then
--				T80_FLASH_BR_N <= '1';
--				T80_FLASH_BGACK_N <= '0';
				romrd_req <= not romrd_req;
				FC <= FC_T80_RD;
--			end if;

		when FC_T80_RD =>
			if romrd_req = romrd_ack then
				if T80_A(0) = '1' then
					T80_FLASH_D <= romrd_q(7 downto 0);
				else
					T80_FLASH_D <= romrd_q(15 downto 8);
				end if;
--				T80_FLASH_BGACK_N <= '1';
				T80_FLASH_DTACK_N <= '0';
				FC <= FC_IDLE;
			end if;

		when FC_DMA_RD =>
			if romrd_req = romrd_ack then
				DMA_FLASH_D_REG <= romrd_q;
				DMA_FLASH_DTACK_N_REG <= '0';
				FC <= FC_IDLE;
			end if;

		when others => null;
		end case;
	
	end if;

end process;

-- SDRAM (68K RAM) CONTROL
FX68_SDRAM_SEL <= '1' when FX68_A(23 downto 21) = "111" and FX68_SEL = '1' else '0';
T80_SDRAM_SEL <= '1' when T80_A(15) = '1' and BAR(23 downto 21) = "111" and
	T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') else '0';
DMA_SDRAM_SEL <= '1' when VBUS_ADDR(23 downto 21) = "111" and VBUS_SEL = '1' else '0';

DMA_SDRAM_DTACK_N  <= '0' when SDRC = SDRC_DMA and ram68k_req = ram68k_ack else DMA_SDRAM_DTACK_N_REG;
DMA_SDRAM_D <= ram68k_q when SDRC = SDRC_DMA and ram68k_req = ram68k_ack else DMA_SDRAM_D_REG;

process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_SDRAM_DTACK_N <= '1';
		T80_SDRAM_DTACK_N <= '1';
		DMA_SDRAM_DTACK_N_REG <= '1';
--		T80_SDRAM_BR_N <= '1';
--		T80_SDRAM_BGACK_N <= '1';
		SDRC_DELAY <= "00";

		ram68k_req <= '0';
		
		SDRC <= SDRC_IDLE;
		
	elsif rising_edge(MCLK) then
		if FX68_SDRAM_SEL = '0' then 
			FX68_SDRAM_DTACK_N <= '1';
		end if;	
		if T80_SDRAM_SEL = '0' then 
			T80_SDRAM_DTACK_N <= '1';
		end if;	
		if DMA_SDRAM_SEL = '0' then 
			DMA_SDRAM_DTACK_N_REG <= '1';
		end if;	

		case SDRC is
		when SDRC_IDLE =>
			--if VCLKCNT = "001" then
				if FX68_SDRAM_SEL = '1' and FX68_SDRAM_DTACK_N = '1' then
					ram68k_req <= not ram68k_req;
					ram68k_a <= FX68_A(15 downto 1);
					ram68k_d <= FX68_DO;
					ram68k_we <= not FX68_RNW and FX68_IO_READY;
					ram68k_u_n <= FX68_UDS_N;
					ram68k_l_n <= FX68_LDS_N;
					SDRC_DELAY <= "00";
					if RFRSH_DELAY = '1' then SDRC_DELAY <= "10"; end if;
					SDRC <= SDRC_FX68;
				elsif T80_SDRAM_SEL = '1' and T80_SDRAM_DTACK_N = '1' then
--					ram68k_req <= not ram68k_req;
					ram68k_a <= BAR(15) & T80_A(14 downto 1);
					ram68k_d <= T80_DO & T80_DO;
					ram68k_we <= not T80_WR_N;
					ram68k_u_n <= T80_A(0);
					ram68k_l_n <= not T80_A(0);
--					T80_SDRAM_BR_N <= '0';
					SDRC <= SDRC_T80_BR;
				elsif DMA_SDRAM_SEL = '1' and DMA_SDRAM_DTACK_N_REG = '1' then
					ram68k_req <= not ram68k_req;
					ram68k_a <= VBUS_ADDR(15 downto 1);
					ram68k_we <= '0';
					ram68k_u_n <= '0';
					ram68k_l_n <= '0';					
					SDRC <= SDRC_DMA;
				end if;
			--end if;

		when SDRC_FX68 =>
			if FX68_PHI2 = '1' and SDRC_DELAY /= "00" then
				SDRC_DELAY <= SDRC_DELAY - 1;
			end if;
			if ram68k_req = ram68k_ack and SDRC_DELAY = "00" then
				FX68_SDRAM_D <= ram68k_q;
				FX68_SDRAM_DTACK_N <= '0';
				SDRC <= SDRC_IDLE;
			end if;

		when SDRC_T80_BR =>
--			if FX68_BG_N = '0' then
--				T80_SDRAM_BGACK_N <= '0';
--				T80_SDRAM_BR_N <= '1';
				ram68k_req <= not ram68k_req;
				SDRC <= SDRC_T80;
--			end if;

		when SDRC_T80 =>
			if ram68k_req = ram68k_ack then
				if T80_A(0) = '0' then
					T80_SDRAM_D <= ram68k_q(15 downto 8);
				else
					T80_SDRAM_D <= ram68k_q(7 downto 0);
				end if;
--				T80_SDRAM_BGACK_N <= '1';
				T80_SDRAM_DTACK_N <= '0';
				SDRC <= SDRC_IDLE;
			end if;

		when SDRC_DMA =>
			if ram68k_req = ram68k_ack then
				DMA_SDRAM_D_REG <= ram68k_q;
				DMA_SDRAM_DTACK_N_REG <= '0';
				SDRC <= SDRC_IDLE;
			end if;
		
		when others => null;
		end case;
		
	end if;

end process;

-- SRAM at 0x200000 - 20FFFF
-- EEPROM at 0x200000
SRAM_EN <= (SRAM_EN_AUTO or SRAM_EN_PAGEIN) and not SVP_ENABLE;

FX68_SRAM_SEL <= '1' when SRAM_EN = '1' and FX68_SEL = '1' and FX68_A(23 downto 16) = x"20" and FX68_EEPROM_SEL = '0' else '0';
FX68_EEPROM_SEL <= '1' when EEPROM_EN = '1' and FX68_SEL = '1' and FX68_A(23 downto 4) = x"20000" and FX68_A(3 downto 1) = "000" else '0';

-- SRAM CONTROL
process( MRST_N, MCLK )
begin

	if MRST_N = '0' then
		FX68_SRAM_DTACK_N <= '1';
		sram_req <= '0';
		SRAMRC <= SRAMRC_IDLE;

	elsif rising_edge(MCLK) then
		if FX68_SRAM_SEL = '0' and saveram_rd ='0' and saveram_we = '0' then
			FX68_SRAM_DTACK_N <= '1';
		end if;

		case SRAMRC is
		when SRAMRC_IDLE =>
			--if VCLKCNT = "001" then
				if (saveram_we = '1' or saveram_rd = '1') and FX68_SRAM_DTACK_N = '1' then
					sram_req <= not sram_req;
					sram_a <= saveram_addr(14 downto 0);
					sram_d <= saveram_din & saveram_din;
					sram_we <= saveram_we;
					sram_u_n <= '0';
					sram_l_n <= '0';
					SRAMRC <= SRAMRC_EXT;
				elsif FX68_SRAM_SEL = '1' and FX68_SRAM_DTACK_N = '1' then
					sram_req <= not sram_req;
					sram_a <= FX68_A(15 downto 1);
					sram_d <= FX68_DO;
					sram_we <= not FX68_RNW;
					sram_u_n <= FX68_UDS_N;
					sram_l_n <= FX68_LDS_N;
					SRAMRC <= SRAMRC_FX68;
				end if;
			--end if;

		when SRAMRC_FX68 =>
			if sram_req = sram_ack then
				FX68_SRAM_D <= sram_q;
				FX68_SRAM_DTACK_N <= '0';
				SRAMRC <= SRAMRC_IDLE;
			end if;

		when SRAMRC_EXT =>
			if sram_req = sram_ack then
				saveram_dout <= sram_q(7 downto 0);
				FX68_SRAM_DTACK_N <= '0';
				SRAMRC <= SRAMRC_IDLE;
			end if;
		when others => null;
		end case;
	end if;
end process;

-- EEPROM CONTROL
process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_EEPROM_DTACK_N <= '1';
	elsif rising_edge(MCLK) then
		if FX68_EEPROM_SEL = '0' then
			FX68_EEPROM_DTACK_N <= '1';
		end if;

		if FX68_EEPROM_SEL = '1' and FX68_EEPROM_DTACK_N = '1' then
			FX68_EEPROM_DATA <= FX68_DO;
			FX68_EEPROM_DTACK_N <= '0';
		end if;
	end if;
end process;

FX68_ZRAM_SEL <= '1' when FX68_A(23 downto 16) = x"A0" and FX68_A(14) = '0' and FX68_SEL = '1' else '0';
T80_ZRAM_SEL <= '1' when T80_A(15 downto 14) = "00" and T80_MREQ_N = '0' and (T80_RD_N = '0' or T80_WR_N = '0') and ZBUSACK_N = '1' else '0';

-- Z80 RAM CONTROL
process( MRST_N, MCLK )
begin
	if MRST_N = '0' then
		FX68_ZRAM_DTACK_N <= '1';
		T80_ZRAM_DTACK_N <= '1';

		zram_we <= '0';
		ZRC <= ZRC_IDLE;

	elsif rising_edge(MCLK) then
		if FX68_ZRAM_SEL = '0' then 
			FX68_ZRAM_DTACK_N <= '1';
		end if;	
		if T80_ZRAM_SEL = '0' then 
			T80_ZRAM_DTACK_N <= '1';
		end if;	

		case ZRC is
		when ZRC_IDLE =>
			if FX68_ZRAM_SEL = '1' and FX68_ZRAM_DTACK_N = '1' then
				zram_a <= T80_A(12 downto 0);
				if FX68_UDS_N = '0' then
					zram_a <= FX68_A(12 downto 1) & "0";
					zram_d <= FX68_DO(15 downto 8);
				else
					zram_a <= FX68_A(12 downto 1) & "1";
					zram_d <= FX68_DO(7 downto 0);
				end if;
				zram_we <= not FX68_RNW and not ZBUSACK_N;
				ZRC <= ZRC_ACC1;
			elsif T80_ZRAM_SEL = '1' and T80_ZRAM_DTACK_N = '1' then
				zram_a <= T80_A(12 downto 0);
				zram_d <= T80_DO;
				zram_we <= not T80_WR_N;
				ZRC <= ZRC_ACC1;
			end if;
		when ZRC_ACC1 =>
			zram_we <= '0';
			ZRC <= ZRC_ACC2;
		when ZRC_ACC2 =>
			if ZBUSACK_N = '0' then
				FX68_ZRAM_D <= zram_q & zram_q;
			else
				FX68_ZRAM_D <= NO_DATA;
			end if;
			T80_ZRAM_D <= zram_q;
			FX68_ZRAM_DTACK_N <= '0';
			T80_ZRAM_DTACK_N <= '0';
			ZRC <= ZRC_IDLE;
		when others => null;
		end case;
	end if;

end process;

-- SVP RAM CONTROL
-- 300000-37FFFF - 128K mirrored x4
-- 390000-39FFFF - cell arrange 1
-- 3A0000-3AFFFF - cell arrange 2
FX68_SVP_RAM_SEL <= '1' when SVP_ENABLE = '1' and (FX68_A(23 downto 19) = x"3"&'0' or FX68_A(23 downto 16) = x"39" or FX68_A(23 downto 16) = x"3A") and FX68_SEL = '1' else '0';
DMA_SVP_RAM_SEL <= '1' when SVP_ENABLE = '1' and (VBUS_ADDR(23 downto 19) = x"3"&'0' or VBUS_ADDR(23 downto 16) = x"39" or VBUS_ADDR(23 downto 16) = x"3A") and VBUS_SEL = '1' else '0';

process( MRST_N, MCLK )
variable svp_dma_a: std_logic_vector(23 downto 1);
begin
	if MRST_N = '0' then
		FX68_SVP_RAM_DTACK_N <= '1';
		DMA_SVP_RAM_DTACK_N <= '1';

		svp_ram2_req <= '0';

		SVPRC <= SVPRC_IDLE;

	elsif rising_edge(MCLK) then
		if FX68_SVP_RAM_SEL = '0' then 
			FX68_SVP_RAM_DTACK_N <= '1';
		end if;	
		if DMA_SVP_RAM_SEL = '0' then 
			DMA_SVP_RAM_DTACK_N <= '1';
		end if;	

		case SVPRC is
		when SVPRC_IDLE =>
			if FX68_SVP_RAM_SEL = '1' and FX68_SVP_RAM_DTACK_N = '1' then
				svp_ram2_req <= not svp_ram2_req;
				if FX68_A(23 downto 16) = x"39" then
					svp_ram2_a <= '0' & FX68_A(15 downto 13) & FX68_A(6 downto 2) & FX68_A(12 downto 7) & FX68_A(1);
				elsif FX68_A(23 downto 16) = x"3A" then
					svp_ram2_a <= '0' & FX68_A(15 downto 12) & FX68_A(5 downto 2) & FX68_A(11 downto 6) & FX68_A(1);
				else
					svp_ram2_a <= FX68_A(16 downto 1);
				end if;
				svp_ram2_d <= FX68_DO;
				svp_ram2_we <= not FX68_RNW;
				svp_ram2_u_n <= FX68_UDS_N;
				svp_ram2_l_n <= FX68_LDS_N;
				SVPRC <= SVPRC_FX68;
			elsif DMA_SVP_RAM_SEL = '1' and DMA_SVP_RAM_DTACK_N = '1' then
				svp_ram2_req <= not svp_ram2_req;
				svp_dma_a := VBUS_ADDR - 1;
				if VBUS_ADDR(23 downto 16) = x"39" then
					svp_ram2_a <= '0' & svp_dma_a(15 downto 13) & svp_dma_a(6 downto 2) & svp_dma_a(12 downto 7) & svp_dma_a(1);
				elsif VBUS_ADDR(23 downto 16) = x"3A" then
					svp_ram2_a <= '0' & svp_dma_a(15 downto 12) & svp_dma_a(5 downto 2) & svp_dma_a(11 downto 6) & svp_dma_a(1);
				else
					svp_ram2_a <= svp_dma_a(16 downto 1);
				end if;
				svp_ram2_we <= '0';
				svp_ram2_u_n <= '0';
				svp_ram2_l_n <= '0';
				SVPRC <= SVPRC_DMA;
			end if;

		when SVPRC_FX68 =>
			if svp_ram2_req = svp_ram2_ack then
				FX68_SVP_RAM_D <= svp_ram2_q;
				FX68_SVP_RAM_DTACK_N <= '0';
				SVPRC <= SVPRC_IDLE;
			end if;

		when SVPRC_DMA =>
			if svp_ram2_req = svp_ram2_ack then
				DMA_SVP_RAM_D <= svp_ram2_q;
				DMA_SVP_RAM_DTACK_N <= '0';
				SVPRC <= SVPRC_IDLE;
			end if;

		when others => null;
		end case;
	end if;

end process;

-- #############################################################################
-- #############################################################################
-- #############################################################################

-- Boot process

FL_DQ <= ext_data;

process( SDR_CLK )
begin
	if rising_edge( SDR_CLK ) then
		if ext_reset_n = '0' then

			ext_data_req <= '0';

			romwr_req <= '0';
			romwr_a <= (others => '0');
			bootState<=BOOT_READ_1;
			SRAM_EN_AUTO <= '0';
			BIG_CART <= '0';
		elsif reset = '0' then
			ext_data_req <= '0';
			romwr_req <= '0';
			bootState <= BOOT_DONE;
		else
			case bootState is 
				when BOOT_READ_1 =>
					ext_data_req <= '1';
					if ext_data_ack ='1' then
						ext_data_req <= '0';
						bootState <= BOOT_WRITE_1;
					end if;
					if ext_bootdone = '1' then
						ext_data_req <= '0';
						-- enable SRAM for carts < 2 MB
						if romwr_a(23 downto 21) = 0 then
							SRAM_EN_AUTO <= '1';
						end if;
						-- enable ROM paging for carts > 4 MB
						if romwr_a(23 downto 22) /= 0 then
							BIG_CART <= '1';
						end if;
						bootState <= BOOT_DONE;
					end if;
				when BOOT_WRITE_1 =>
					romwr_d <= FL_DQ;
					romwr_req <= not romwr_req;
					bootState <= BOOT_WRITE_2;
				when BOOT_WRITE_2 =>
					if romwr_req = romwr_ack then
						romwr_a <= romwr_a + 1;
						bootState <= BOOT_READ_1;
					end if;
				when others => null;
			end case;	
		end if;
	end if;
end process;

-- Route VDP signals to outputs
RED <= VDP_RED;
GREEN <= VDP_GREEN;
BLUE <= VDP_BLUE;
HS <= VDP_HS_N;
VS <= VDP_VS_N;

-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################

-- DEBUG

-- synthesis translate_off
process( MCLK )
	file F		: text open write_mode is "gen.out";
	variable L	: line;
	variable rom_q : std_logic_vector(15 downto 0);
begin
	if rising_edge( MCLK ) then

		-- ROM ACCESS
		if FC = FC_FX68_RD and romrd_req = romrd_ack then
			write(L, string'("68K "));
			write(L, string'("RD"));
			write(L, string'(" ROM     ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			rom_q := x"FFFF";
			case FX68_A(2 downto 1) is
			when "00" =>
				if FX68_UDS_N = '0' then rom_q(15 downto 8) := romrd_q(15 downto 8); end if;
				if FX68_LDS_N = '0' then rom_q(7 downto 0) := romrd_q(7 downto 0); end if;

			when "01" =>
				if FX68_UDS_N = '0' then rom_q(15 downto 8) := romrd_q(31 downto 24); end if;
				if FX68_LDS_N = '0' then rom_q(7 downto 0) := romrd_q(23 downto 16); end if;

			when "10" =>
				if FX68_UDS_N = '0' then rom_q(15 downto 8) := romrd_q(47 downto 40); end if;
				if FX68_LDS_N = '0' then rom_q(7 downto 0) := romrd_q(39 downto 32); end if;

			when "11" =>
				if FX68_UDS_N = '0' then rom_q(15 downto 8) := romrd_q(63 downto 56); end if;
				if FX68_LDS_N = '0' then rom_q(7 downto 0) := romrd_q(55 downto 48); end if;

			when others => null;
			end case;				
			if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
				hwrite(L, rom_q);
			elsif FX68_UDS_N = '0' then
				hwrite(L, rom_q(15 downto 8));
				write(L, string'("  "));
			else
				write(L, string'("  "));
				hwrite(L, rom_q(7 downto 0));
			end if;								
			write(L, string'("]"));
			writeline(F,L);			
		end if;		

	
		-- 68K RAM ACCESS
		if SDRC = SDRC_FX68 and ram68k_req = ram68k_ack then
			write(L, string'("68K "));
			if FX68_RNW = '0' then
				write(L, string'("WR"));
			else
				write(L, string'("RD"));
			end if;
			write(L, string'(" RAM-68K ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			if FX68_RNW = '0' then
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, FX68_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, FX68_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, FX68_DO(7 downto 0));
				end if;				
			else
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, ram68k_q);
				elsif FX68_UDS_N = '0' then
					hwrite(L, ram68k_q(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, ram68k_q(7 downto 0));
				end if;								
			end if;
			write(L, string'("]"));
			writeline(F,L);			
		end if;		

		
		-- Z80 RAM ACCESS
		if ZRC = ZRC_ACC3 and ZRCP = ZRCP_FX68 then
			write(L, string'("68K "));
			if FX68_RNW = '0' then
				write(L, string'("WR"));
			else
				write(L, string'("RD"));
			end if;
			write(L, string'(" RAM-Z80 ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			if FX68_RNW = '0' then
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, FX68_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, FX68_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, FX68_DO(7 downto 0));
				end if;				
			else
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, zram_q & zram_q);
				elsif FX68_UDS_N = '0' then
					hwrite(L, zram_q);
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, zram_q);
				end if;				
			end if;
			write(L, string'("]"));
			writeline(F,L);			
		end if;		

		
		-- 68K CTRL ACCESS
		if FX68_CTRL_SEL = '1' and FX68_CTRL_DTACK_N = '1' then
			write(L, string'("68K "));
			if FX68_RNW = '0' then
				write(L, string'("WR"));
			else
				write(L, string'("RD"));
			end if;
			write(L, string'("    CTRL ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			if FX68_RNW = '0' then
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, FX68_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, FX68_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, FX68_DO(7 downto 0));
				end if;				
			else
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					write(L, string'("????"));
				elsif FX68_UDS_N = '0' then
					write(L, string'("??"));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					write(L, string'("??"));
				end if;								
			end if;
			write(L, string'("]"));
			writeline(F,L);							
		end if;

		-- 68K I/O ACCESS
		if IOC = IOC_FX68_ACC and IO_DTACK_N = '0' then
			write(L, string'("68K "));
			if FX68_RNW = '0' then
				write(L, string'("WR"));
			else
				write(L, string'("RD"));
			end if;
			write(L, string'("     I/O ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			if FX68_RNW = '0' then
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, FX68_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, FX68_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, FX68_DO(7 downto 0));
				end if;				
			else
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, IO_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, IO_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, IO_DO(7 downto 0));
				end if;								
			end if;
			write(L, string'("]"));
			writeline(F,L);					
		end if;
		
		-- 68K VDP ACCESS
		if VDPC = VDPC_FX68_ACC and VDP_DTACK_N = '0' then
			write(L, string'("68K "));
			if FX68_RNW = '0' then
				write(L, string'("WR"));
			else
				write(L, string'("RD"));
			end if;
			write(L, string'("     VDP ["));
			hwrite(L, FX68_A(23 downto 0));
			write(L, string'("] = ["));
			if FX68_RNW = '0' then
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, FX68_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, FX68_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, FX68_DO(7 downto 0));
				end if;				
			else
				if FX68_UDS_N = '0' and FX68_LDS_N = '0' then
					hwrite(L, VDP_DO);
				elsif FX68_UDS_N = '0' then
					hwrite(L, VDP_DO(15 downto 8));
					write(L, string'("  "));
				else
					write(L, string'("  "));
					hwrite(L, VDP_DO(7 downto 0));
				end if;								
			end if;
			write(L, string'("]"));
			writeline(F,L);					
		end if;
		
	end if;
end process;
-- synthesis translate_on

end rtl;
