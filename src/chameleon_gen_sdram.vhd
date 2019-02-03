-- -----------------------------------------------------------------------
--
-- Turbo Chameleon
--
-- Multi purpose FPGA expansion for the Commodore 64 computer
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2011 by Peter Wendrich (pwsoft@syntiac.com)
-- All Rights Reserved.
--
-- Your allowed to re-use this file for non-commercial applications
-- developed for the Turbo Chameleon 64 cartridge. Either open or closed
-- source whatever might be required by other licenses.
--
-- Used with persmission in the FPGAPCE project.
--
-- http://www.syntiac.com/chameleon.html
-- -----------------------------------------------------------------------
--
-- SDRAM controller
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

-- -----------------------------------------------------------------------

entity chameleon_sdram is
	generic (
		-- SDRAM cols/rows  8/12 = 8 Mbyte, 9/12 = 16 Mbyte, 9/13 = 32 Mbyte
		colAddrBits : integer := 9;
		rowAddrBits : integer := 12;

		-- Controller settings
		initTimeout : integer := 10000;
	-- SDRAM timing
		casLatency : integer := 2;
		rasCasTiming : integer := 2;
		prechargeTiming: integer := 2;
		t_refresh_ms  : real := 64.0;
		t_ck_ns  : real := 10.0 -- Clock cycle time
	);
	port (
-- System
		clk : in std_logic;
		reset_n: in std_logic;

		reserve : in std_logic := '0';
		delay_refresh : in std_logic := '0';

-- SDRAM interface
		sd_data : inout std_logic_vector(15 downto 0);
		sd_addr : out std_logic_vector((rowAddrBits-1) downto 0);
		sd_we_n : out std_logic;
		sd_ras_n : out std_logic;
		sd_cas_n : out std_logic;
		sd_ba_0 : out std_logic;
		sd_ba_1 : out std_logic;
		sd_ldqm : out std_logic;
		sd_udqm : out std_logic;

		romwr_req : in std_logic;
		romwr_ack : out std_logic;
		romwr_we : in std_logic;
		romwr_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		romwr_d : in std_logic_vector(15 downto 0);
		romwr_q : out std_logic_vector(15 downto 0);

		romrd_req : in std_logic;
		romrd_ack : out std_logic;
		romrd_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 3);
		romrd_q : out std_logic_vector(63 downto 0);

		ram68k_req : in std_logic;
		ram68k_ack : out std_logic;
		ram68k_we : in std_logic;
		ram68k_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		ram68k_d : in std_logic_vector(15 downto 0);
		ram68k_q : out std_logic_vector(15 downto 0);
		ram68k_u_n : in std_logic;
		ram68k_l_n : in std_logic;

		sram_req : in std_logic;
		sram_ack : out std_logic;
		sram_we : in std_logic;
		sram_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		sram_d : in std_logic_vector(15 downto 0);
		sram_q : out std_logic_vector(15 downto 0);
		sram_u_n : in std_logic;
		sram_l_n : in std_logic;

		vram_req : in std_logic;
		vram_ack : out std_logic;
		vram_we : in std_logic;
		vram_a : in std_logic_vector((colAddrBits+rowAddrBits+2) downto 1);
		vram_d : in std_logic_vector(15 downto 0);
		vram_q : out std_logic_vector(15 downto 0);
		vram_u_n : in std_logic;
		vram_l_n : in std_logic;

--GE Temporary
		initDone : out std_logic;
-- Debug ports
		debugIdle : out std_logic;  -- '1' memory is idle
		debugRefresh : out std_logic; -- '1' memory is being refreshed
		debugvram_q : out std_logic_vector(15 downto 0);
		debugcache_q : out std_logic_vector(15 downto 0)
	);
end entity;

-- -----------------------------------------------------------------------

architecture rtl of chameleon_sdram is
	constant refresh_interval : integer := integer((t_refresh_ms*1000000.0) / (t_ck_ns * 2.0**rowAddrBits));
-- ram state machine
	type ramStates is (
		RAM_INIT,
		RAM_INIT_PRECHARGE,
		RAM_INITAUTO1,
		RAM_INITAUTO2,
		RAM_SETMODE,
		RAM_IDLE,

		RAM_ACTIVATE,

		RAM_READ_1,
		RAM_READ_CACHE_FILL,
		RAM_READ_TERMINATE_BURST,
		RAM_READ_2,
		RAM_READ_3,
		RAM_READ_4,
		RAM_READ_5,
		RAM_WRITE_1,

		RAM_PRECHARGE_ALL,
		RAM_AUTOREFRESH
	);

	type ramPorts is (
		PORT_NONE,
		PORT_ROMRD,
		PORT_ROMWR,
		PORT_RAM68K,
		PORT_VRAM,
		PORT_SRAM
	);

	subtype row_t is std_logic_vector((rowAddrBits-1) downto 0);
	subtype col_t is std_logic_vector((colAddrBits-1) downto 0);

	signal ramTimer : integer range 0 to 32767;
	signal ramState : ramStates := RAM_INIT;
	signal ramDone : std_logic;

	signal ram_data_reg : std_logic_vector(sd_data'range);

-- Registered sdram signals
	signal sd_data_reg : std_logic_vector(15 downto 0);
	signal sd_data_ena : std_logic := '0';
	signal sd_addr_reg : std_logic_vector((rowAddrBits-1) downto 0);
	signal sd_we_n_reg : std_logic;
	signal sd_ras_n_reg : std_logic;
	signal sd_cas_n_reg : std_logic;
	signal sd_ba_0_reg : std_logic;
	signal sd_ba_1_reg : std_logic;
	signal sd_ldqm_reg : std_logic;
	signal sd_udqm_reg : std_logic;

	signal romwr_ackReg : std_logic := '0';
	signal romrd_ackReg : std_logic := '0';
	signal ram68k_ackReg : std_logic := '0';
	signal vram_ackReg : std_logic := '0';
	signal sram_ackReg : std_logic := '0';

	signal romwr_qReg : std_logic_vector(15 downto 0);
	signal romrd_qReg : std_logic_vector(63 downto 0);
	signal ram68k_qReg : std_logic_vector(15 downto 0);
	signal vram_qReg : std_logic_vector(15 downto 0);
	signal sram_qReg : std_logic_vector(15 downto 0);

	signal initDoneReg : std_logic := '0';

-- Active rows in SDRAM
	type bankRowDef is array(0 to 3) of row_t;
	signal bankActive : std_logic_vector(0 to 3) := (others => '0');
	signal bankRow : bankRowDef;

-- Memory auto refresh
	constant refreshClocks : integer := 9;
	signal refreshTimer : integer range 0 to 2047 := 0;
	signal refreshActive : std_logic := '0';
	signal refreshSubtract : std_logic := '0';

	signal currentState : ramStates;
	signal currentPort : ramPorts;
	signal currentBank : std_logic_vector(1 downto 0);
	signal currentRow : row_t;
	signal currentCol : col_t;
	signal currentRdData : std_logic_vector(63 downto 0);
	signal currentWrData : std_logic_vector(15 downto 0);
	signal currentLdqm : std_logic;
	signal currentUdqm : std_logic;

	signal nextRamBank : std_logic_vector(1 downto 0);
	signal nextRamRow : row_t;
	signal nextRamCol : col_t;
	signal nextRamPort : ramPorts;
	signal nextRamState : ramStates;
	signal nextLdqm : std_logic;
	signal nextUdqm : std_logic;

	signal cache_ack_reg : std_logic := '0';
	signal cache_write : std_logic := '0';
	signal cache_writeack : std_logic := '0';
	signal cache_ready : std_logic;
	signal cache_req : std_logic;
	signal cache_req_d : std_logic_vector(1 downto 0);
	signal cache_ack : std_logic;
	signal cache_valid : std_logic;
	signal cache_wr : std_logic;
	signal cache_wrl : std_logic;
	signal cache_wrh : std_logic;
	signal cache_q : std_logic_vector(15 downto 0);
	signal cache_sdram_req : std_logic;
	signal cache_fill : std_logic;

	subtype addr_bankbits is Natural range colAddrBits+2 downto colAddrBits+1;
	subtype addr_rowbits is Natural range colAddrBits+rowAddrBits+2 downto colAddrBits+3;
	subtype addr_colbits is Natural range colAddrBits downto 1;
	subtype addr_colbits_64bit is Natural range colAddrBits downto 3;

	COMPONENT TwoWayCache
		PORT (
			clk                     : IN STD_LOGIC;
			reset_n                 : IN std_logic;
			ready                   : out std_logic;
			cpu_addr                : IN std_logic_vector(31 DOWNTO 0);
			cpu_req                 : IN STD_LOGIC;
			cpu_ack                 : OUT STD_LOGIC;
			cpu_cachevalid          : OUT STD_LOGIC;
			cpu_rw_n                : IN STD_LOGIC;
			cpu_rwl_n               : in std_logic;
			cpu_rwu_n               : in std_logic;
			data_from_cpu           : IN std_logic_vector(15 DOWNTO 0);
			data_to_cpu             : OUT std_logic_vector(15 DOWNTO 0);
			data_from_sdram         : IN std_logic_vector(15 DOWNTO 0);
			sdram_req               : OUT STD_LOGIC;
			sdram_fill              : IN STD_LOGIC
	);
	END COMPONENT;
begin

	mytwc : component TwoWayCache
	PORT map (
		clk => clk,
		reset_n => reset_n,
		ready => cache_ready,
		cpu_addr(31 downto colAddrBits+rowAddrBits+3) => (others => '0'),
		cpu_addr(colAddrBits+rowAddrBits+2 downto 0) => vram_a&'0',
		cpu_req => cache_req,
		cpu_ack => cache_ack,
		cpu_cachevalid => cache_valid,
		cpu_rw_n => not vram_we,
		cpu_rwl_n => vram_l_n,
		cpu_rwu_n => vram_u_n,
		data_from_cpu => vram_d,
		data_to_cpu => cache_q,--vram_q,
		data_from_sdram => ram_data_reg,
		sdram_req => cache_sdram_req,
		sdram_fill => cache_fill
	);
	cache_req <= '1' when (vram_req /= vram_ackReg) and (currentPort /= PORT_VRAM) and cache_ack = '0' else '0';

	debugvram_q <= vram_qReg;
	debugcache_q <= cache_q;
-- -----------------------------------------------------------------------

	ram_data_reg <= sd_data;

-- -----------------------------------------------------------------------
-- Refresh timer
	process(clk)
	begin
		if rising_edge(clk) then
			if refreshSubtract = '1' then
				refreshTimer <= refreshTimer - refresh_interval;
			else
-- synthesis translate_off
				if refreshTimer < 2047 then --GE
-- synthesis translate_on
					refreshTimer <= refreshTimer + 1;
-- synthesis translate_off
				end if;
-- synthesis translate_on
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- State machine
	process(clk, currentPort, cache_sdram_req,
		romrd_a, romrd_req, romrd_ackReg,
		romwr_a, romwr_we, romwr_req, romwr_ackReg,
		ram68k_a, ram68k_we, ram68k_req, ram68k_ackReg, ram68k_l_n, ram68k_u_n,
		vram_a, vram_we, vram_req, vram_ackReg, vram_l_n, vram_u_n,
		sram_a, sram_we, sram_req, sram_ackReg, sram_l_n, sram_u_n)
	begin
		--if rising_edge(clk) then
			nextRamState <= RAM_IDLE;
			nextRamPort <= PORT_NONE;
			nextRamBank <= "00";
			nextRamRow <= ( others => '0');
			nextRamCol <= ( others => '0');
			nextLdqm <= '0';
			nextUdqm <= '0';

			if (romwr_req /= romwr_ackReg) and (currentPort /= PORT_ROMWR) then
				nextRamState <= RAM_READ_1;
				if romwr_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_ROMWR;
				nextRamBank <= romwr_a(addr_bankbits);
				nextRamRow <= romwr_a(addr_rowbits);
				nextRamCol <= romwr_a(addr_colbits);
				nextLdqm <= '0';
				nextUdqm <= '0';
			elsif (vram_req /= vram_ackReg) and (currentPort /= PORT_VRAM) and (vram_we = '1' or cache_sdram_req = '1') then
				nextRamState <= RAM_READ_1;
				if vram_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_VRAM;
				nextRamBank <= vram_a(addr_bankbits);
				nextRamRow <= vram_a(addr_rowbits);
				nextRamCol <= vram_a(addr_colbits);
				nextLdqm <= vram_l_n;
				nextUdqm <= vram_u_n;
			elsif (romrd_req /= romrd_ackReg) and (currentPort /= PORT_ROMRD) then
				nextRamState <= RAM_READ_1;
				nextRamPort <= PORT_ROMRD;
				nextRamBank <= romrd_a(addr_bankbits);
				nextRamRow <= romrd_a(addr_rowbits);
				nextRamCol <= romrd_a(addr_colbits_64bit) & "00";
				nextLdqm <= '0';
				nextUdqm <= '0';
			elsif (ram68k_req /= ram68k_ackReg) and (currentPort /= PORT_RAM68K) then
				nextRamState <= RAM_READ_1;
				if ram68k_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_RAM68K;
				nextRamBank <= ram68k_a(addr_bankbits);
				nextRamRow <= ram68k_a(addr_rowbits);
				nextRamCol <= ram68k_a(addr_colbits);
				nextLdqm <= ram68k_l_n;
				nextUdqm <= ram68k_u_n;
			elsif (sram_req /= sram_ackReg) and (currentPort /= PORT_SRAM) then
				nextRamState <= RAM_READ_1;
				if sram_we = '1' then
					nextRamState <= RAM_WRITE_1;
				end if;
				nextRamPort <= PORT_SRAM;
				nextRamBank <= sram_a(addr_bankbits);
				nextRamRow <= sram_a(addr_rowbits);
				nextRamCol <= sram_a(addr_colbits);
				nextLdqm <= sram_l_n;
				nextUdqm <= sram_u_n;
			end if;
		--end if;
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			sd_data <= (others => 'Z');
			if sd_data_ena = '1' then
				sd_data <= sd_data_reg;
			end if;
			sd_addr <= sd_addr_reg;
			sd_ras_n <= sd_ras_n_reg;
			sd_cas_n <= sd_cas_n_reg;
			sd_we_n <= sd_we_n_reg;
			sd_ba_0 <= sd_ba_0_reg;
			sd_ba_1 <= sd_ba_1_reg;
			sd_ldqm <= sd_ldqm_reg;
			sd_udqm <= sd_udqm_reg;
		end if;
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			refreshSubtract <= '0';
			ramDone <= '0';
			sd_data_ena <= '0';
			sd_addr_reg <= (others => '0');
			sd_ras_n_reg <= '1';
			sd_cas_n_reg <= '1';
			sd_we_n_reg <= '1';

			sd_ba_0_reg <= '0';
			sd_ba_1_reg <= '0';

			sd_ldqm_reg <= '0';
			sd_udqm_reg <= '0';

			cache_fill <= '0';

			if ramTimer /= 0 then
				ramTimer <= ramTimer - 1;
			else
				case ramState is
				when RAM_INIT =>
					-- Wait for clock to stabilise and PLL locks
					-- Then follow init steps in datasheet:
					--   precharge all banks
					--   perform a few autorefresh cycles (we do 2 of them)
					--   setmode (burst and CAS latency)
					--   after a few clocks ram is ready for use (we wait 10 just to be sure).
					ramTimer <= 20000;
					ramState <= RAM_INIT_PRECHARGE;
				when RAM_INIT_PRECHARGE =>
					-- Precharge all banks, part of initialisation sequence.
					ramTimer <= 100;
					ramState <= RAM_INITAUTO1;
					sd_ras_n_reg <= '0';
					sd_we_n_reg <= '0';
					sd_addr_reg(10) <= '1'; -- precharge all banks
				when RAM_INITAUTO1 =>
					-- refresh cycle to init ram (1st)
					ramTimer <= 10;
					ramState <= RAM_INITAUTO2;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_INITAUTO2 =>
					-- refresh cycle to init ram (2nd)
					ramTimer <= 10;
					ramState <= RAM_SETMODE;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_SETMODE =>
					-- Set mode bits of RAM.
					ramTimer <= 10;
					ramState <= RAM_IDLE; -- ram is ready for commands after set-mode
					sd_addr_reg <= std_logic_vector(resize("001000100010", sd_addr'length)); -- CAS2, Burstlength 4 (8 bytes, 64 bits), no burst on writes
					if casLatency = 3 then
						sd_addr_reg(6 downto 4) <= "011";
					end if;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_IDLE =>
					initDoneReg <= '1'; --GE
					refreshActive <= '0';
					currentPort <= PORT_NONE;
					if nextRamState /= RAM_IDLE then
						currentState <= nextRamState;
						currentPort <= nextRamPort;
						currentBank <= nextRamBank;
						currentRow <= nextRamRow;
						currentCol <= nextRamCol;
						currentLdqm <= nextLdqm;
						currentUdqm <= nextUdqm;

						case nextRamPort is
						when PORT_VRAM => --GE
							currentWrData <= vram_d;
						when PORT_RAM68K => --GE
							currentWrData <= ram68k_d;
						when PORT_SRAM => --GE
							currentWrData <= sram_d;
						when PORT_ROMWR =>
							currentWrData <= romwr_d;
						when others =>
							null;
						end case;
						ramState <= nextRamState;

						if bankActive(to_integer(unsigned(nextRamBank))) = '0' then
							-- Current bank not active. Activate a row first
							ramTimer <= rasCasTiming - 1;
							sd_addr_reg <= nextRamRow;
							sd_ras_n_reg <= '0';
							sd_ba_0_reg <= nextRamBank(0);
							sd_ba_1_reg <= nextRamBank(1);
							bankRow(to_integer(unsigned(nextRamBank))) <= nextRamRow;
							bankActive(to_integer(unsigned(nextRamBank))) <= '1';
						elsif bankRow(to_integer(unsigned(nextRamBank))) /= nextRamRow then
							-- Wrong row active in bank, do precharge then activate a row.
							ramTimer <= prechargeTiming - 1;
							sd_we_n_reg <= '0';
							sd_ras_n_reg <= '0';
							sd_ba_0_reg <= nextRamBank(0);
							sd_ba_1_reg <= nextRamBank(1);
							bankActive(to_integer(unsigned(nextRamBank))) <= '0';
							ramState <= RAM_ACTIVATE;
						end if;
					elsif delay_refresh = '0' and reserve = '0' and refreshTimer > refresh_interval then
						-- Refresh timeout, perform auto-refresh cycle
						refreshActive <= '1';
						refreshSubtract <= '1';
						if bankActive /= "0000" then
							-- There are still rows active, so we precharge them first
							ramState <= RAM_PRECHARGE_ALL;
						else
							ramState <= RAM_AUTOREFRESH;
						end if;
					end if;

				when RAM_ACTIVATE =>
					ramTimer <= rasCasTiming - 1;
					ramState <= currentState;
					sd_addr_reg <= currentRow;
					sd_ras_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
					bankRow(to_integer(unsigned(currentBank))) <= currentRow;
					bankActive(to_integer(unsigned(currentBank))) <= '1';

				when RAM_READ_1 =>
					if currentPort = PORT_VRAM then
						ramTimer <= casLatency;
						ramState <= RAM_READ_CACHE_FILL;
					elsif currentPort = PORT_ROMRD then
						ramTimer <= casLatency + 1;
						ramState <= RAM_READ_2;
					else
						ramState <= RAM_READ_TERMINATE_BURST;
					end if;
					sd_addr_reg <= std_logic_vector(resize(unsigned(currentCol), sd_addr'length));
					--GE sd_addr_reg <= resize(currentCol, sd_addr'length) or resize("10000000000", sd_addr'length); --GE Auto precharge
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);

				when RAM_READ_CACHE_FILL =>
					cache_fill <= '1';
					ramState <= RAM_READ_2;

				when RAM_READ_TERMINATE_BURST =>
					sd_we_n_reg <= '0';
					ramTimer <= casLatency;
					ramState <= RAM_READ_2;

				when RAM_READ_2 =>
					if currentPort = PORT_ROMRD or currentPort = PORT_VRAM then
						ramState <= RAM_READ_3;
					else
						ramDone <='1';
						ramState <= RAM_IDLE;
					end if;
					currentRdData(15 downto 0) <= ram_data_reg;

					case currentPort is
						when PORT_ROMWR => --GE
							romwr_qReg <= ram_data_reg;
						when PORT_RAM68K => --GE
							ram68k_qReg <= ram_data_reg;
						when PORT_VRAM => --GE
							vram_qReg <= ram_data_reg;
						when PORT_SRAM => --GE
							sram_qReg <= ram_data_reg;
						when others =>
							null;
					end case;

				when RAM_READ_3 =>
					ramState <= RAM_READ_4;
					currentRdData(31 downto 16) <= ram_data_reg;

				when RAM_READ_4 =>
					ramState <= RAM_READ_5;
					currentRdData(47 downto 32) <= ram_data_reg;

				when RAM_READ_5 =>
					currentRdData(63 downto 48) <= ram_data_reg;
					ramState <= RAM_IDLE;
					ramDone <= '1';

				when RAM_WRITE_1 =>
					ramState <= RAM_IDLE;
					sd_data_ena <= '1';
					sd_we_n_reg <= '0';
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);

					sd_addr_reg <= std_logic_vector(resize(unsigned(currentCol), sd_addr'length));
					--GE sd_addr_reg <= resize(currentCol, sd_addr'length) or resize("10000000000", sd_addr'length); --GE Auto precharge

					sd_data_reg <= currentWrData;
					sd_ldqm_reg <= currentLdqm;
					sd_udqm_reg <= currentUdqm;
					ramDone <= '1';

				when RAM_PRECHARGE_ALL =>
					ramTimer <= prechargeTiming - 1;
					ramState <= RAM_IDLE;
					if refreshActive = '1' then
						ramTimer <= 1;
						ramState <= RAM_AUTOREFRESH;
					end if;
					sd_addr_reg(10) <= '1'; -- All banks
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					bankActive <= "0000";
				when RAM_AUTOREFRESH =>
					ramTimer <= refreshClocks;
					ramState <= RAM_IDLE;
					sd_we_n_reg <= '1';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				end case;
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- Debug and measurement signals
	debugIdle <= '1' when ((refreshActive = '0') and (ramState = RAM_IDLE)) else '0';
	debugRefresh <= refreshActive;

--GE -----------------------------------------------------------------------
--GE PCE VDC port
	process(clk)
	begin
		if rising_edge(clk) then
			if (currentPort = PORT_VRAM and ((vram_we='1' and ramDone = '1') or (vram_we='0' and cache_ack = '1')))
			or (cache_valid = '1' and cache_ack = '1' and vram_we = '0')
			--if (currentPort = PORT_VRAM and ramDone = '1')
			then
				vram_ackReg <= vram_req;
			end if;
		end if;
	end process;
	vram_ack <= vram_ackReg;
	vram_q <= cache_q;
--	vram_q <= vram_qReg;


	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMWR
			and ramDone = '1' then
				romwr_ackReg <= not romwr_ackReg;
			end if;
		end if;
	end process;
	romwr_ack <= romwr_ackReg;
	romwr_q <= romwr_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMRD
			and ramDone = '1' then
				romrd_ackReg <= not romrd_ackReg;
				romrd_qReg <= currentRdData;
			end if;
		end if;
	end process;
	romrd_ack <= romrd_ackReg;
	romrd_q <= romrd_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_RAM68K and ramDone = '1' then
				ram68k_ackReg <= not ram68k_ackReg;
			end if;
		end if;
	end process;
	ram68k_ack <= ram68k_ackReg;
	ram68k_q <= ram68k_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_SRAM and ramDone = '1' then
				sram_ackReg <= not sram_ackReg;
			end if;
		end if;
	end process;
	sram_ack <= sram_ackReg;
	sram_q <= sram_qReg;

	initDone <= initDoneReg; --Ge

end architecture;
