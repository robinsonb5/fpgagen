library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
use work.build_id.all;
use work.mist.ALL;
 
entity MIST_Toplevel is
	port
	(
		CLOCK_27		:	 in std_logic_vector(1 downto 0);
		
		LED			: 	out std_logic;

		UART_TX		:	 out STD_LOGIC;
		UART_RX		:	 in STD_LOGIC;

		SDRAM_DQ		:	 inout std_logic_vector(15 downto 0);
		SDRAM_A	:	 out std_logic_vector(12 downto 0);
		SDRAM_DQMH	:	 out STD_LOGIC;
		SDRAM_DQML	:	 out STD_LOGIC;
		SDRAM_nWE	:	 out STD_LOGIC;
		SDRAM_nCAS	:	 out STD_LOGIC;
		SDRAM_nRAS	:	 out STD_LOGIC;
		SDRAM_nCS	:	 out STD_LOGIC;
		SDRAM_BA		:	 out std_logic_vector(1 downto 0);
		SDRAM_CLK	:	 out STD_LOGIC;
		SDRAM_CKE	:	 out STD_LOGIC;

		SPI_DO	: inout std_logic;
		SPI_DI	: in std_logic;
		SPI_SCK		:	 in STD_LOGIC;
		SPI_SS2		:	 in STD_LOGIC; -- FPGA
		SPI_SS3		:	 in STD_LOGIC; -- OSD
		SPI_SS4		:	 in STD_LOGIC; -- "sniff" mode
		CONF_DATA0  : in std_logic; -- SPI_SS for user_io

		VGA_HS		:	buffer STD_LOGIC;
		VGA_VS		:	buffer STD_LOGIC;
		VGA_R		:	out std_logic_vector(5 downto 0);
		VGA_G		:	out std_logic_vector(5 downto 0);
		VGA_B		:	out std_logic_vector(5 downto 0);

		AUDIO_L : out std_logic;
		AUDIO_R : out std_logic
	);
END entity;

architecture rtl of MIST_Toplevel is

signal reset_n : std_logic;
signal reset_d : std_logic;
signal pll_locked : std_logic;
signal MCLK      : std_logic;
signal memclk      : std_logic;

signal audiol : std_logic_vector(15 downto 0);
signal audior : std_logic_vector(15 downto 0);

signal gen_red      : std_logic_vector(3 downto 0);
signal gen_green    : std_logic_vector(3 downto 0);
signal gen_blue     : std_logic_vector(3 downto 0);
signal gen_hs		: std_logic;
signal gen_vs		: std_logic;

-- user_io
signal buttons: std_logic_vector(1 downto 0);
signal status:  std_logic_vector(31 downto 0) := (others => '0');
signal joy_0: std_logic_vector(31 downto 0);
signal joy_1: std_logic_vector(31 downto 0);
signal ypbpr: std_logic;
signal scandoubler_disable: std_logic;
signal no_csync : std_logic;
signal mouse_x: signed(8 downto 0);
signal mouse_y: signed(8 downto 0);
signal mouse_flags: std_logic_vector(7 downto 0);
signal mouse_strobe: std_logic;

-- sd io
signal sd_lba:  unsigned(31 downto 0);
signal sd_rd:   std_logic_vector(1 downto 0) := "00";
signal sd_wr:   std_logic_vector(1 downto 0) := "00";
signal sd_ack:  std_logic;
signal sd_ackD:  std_logic;
signal sd_conf: std_logic;
signal sd_sdhc: std_logic;
signal sd_din:  std_logic_vector(7 downto 0);
signal sd_din_strobe:  std_logic;
signal sd_dout: std_logic_vector(7 downto 0);
signal sd_dout_strobe:  std_logic;
signal sd_buff_addr: std_logic_vector(8 downto 0);
signal img_mounted  : std_logic_vector(1 downto 0);
signal img_mountedD : std_logic;
signal img_size : std_logic_vector(31 downto 0);

-- backup ram controller
signal bk_state : std_logic := '0';
signal bk_ena   : std_logic := '0';
signal bk_load  : std_logic := '0';
signal bk_loadD : std_logic := '0';
signal bk_saveD : std_logic := '0';

-- data_io
signal downloading      : std_logic;
signal data_io_wr       : std_logic;
signal data_io_clkref   : std_logic;
signal data_io_d        : std_logic_vector(7 downto 0);
signal downloadingD     : std_logic;
signal downloadingD_MCLK: std_logic;
signal d_state          : std_logic_vector(1 downto 0);

-- external controller signals
signal ext_reset_n      : std_logic_vector(2 downto 0) := "111";
signal ext_bootdone     : std_logic := '0';
signal ext_data         : std_logic_vector(15 downto 0);
signal ext_data_req     : std_logic;
signal ext_data_ack     : std_logic := '0';
signal ext_sw           : std_logic_vector( 15 downto 0); --DIP switches
signal core_led         : std_logic;

constant SVP_EN         : std_logic := '0';

function core_name return string is
begin
	if SVP_EN = '1' then return "GEN_SVP"; else return "GENESIS"; end if;
end function;

constant CONF_DBG_STR : string := "";
--constant CONF_DBG_STR : string :=
--    "O3,VRAM Speed,Slow,Fast;"&
--    "O4,FM Sound,Enable,Disable;"&
--    "O5,PSG Sound,Enable,Disable;";

constant CONF_STR : string := core_name &
    ";BINGENMD ;"&
    "S,SAV,Mount;"&
    "TE,Write Save RAM;"&
    "P1,Video & Audio;"&
    "P2,Controls;"&
    "P3,System;"&
    "P1OBC,Scanlines,Off,25%,50%,75%;"&
    "P1OH,PCM HiFi sound,Disable,Enable;"&
    "P1OJ,Border,Disable,Enable;"&
    "P1OK,Blending,Disable,Enable;"&
    "P2O6,Joystick swap,Off,On;"&
    "P2O9,Swap Y axis,Off,On;"&
    "P2OA,Only 3 buttons,Off,On;"&
    "P2OFG,Mouse,Off,Port 1,Port 2;"&
    "P3O78,Region,Auto,EU,JP,US;"&
    "P3OD,Fake EEPROM,Off,On;"&
    "P3OI,CPU Turbo,Off,On;"&
    CONF_DBG_STR&
    "T0,Reset;"&
    "V,v"&BUILD_DATE;

-- convert string to std_logic_vector to be given to user_io
function to_slv(s: string) return std_logic_vector is
  constant ss: string(1 to s'length) := s;
  variable rval: std_logic_vector(1 to 8 * s'length);
  variable p: integer;
  variable c: integer;
begin
  for i in ss'range loop
    p := 8 * i;
    c := character'pos(ss(i));
    rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
  end loop;
  return rval;
end function;

-- Sigma Delta audio
COMPONENT hybrid_pwm_sd
	PORT
	(
		clk		:	 IN STD_LOGIC;
		n_reset		:	 IN STD_LOGIC;
		din		:	 IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		dout		:	 OUT STD_LOGIC
	);
END COMPONENT;

component data_io
    generic ( ROM_DIRECT_UPLOAD : boolean := true );
    port (  clk_sys        : in std_logic;
            clkref_n       : in std_logic;
            ioctl_wr       : out std_logic;
            ioctl_addr     : out std_logic_vector(24 downto 0);
            ioctl_dout     : out std_logic_vector(7 downto 0);
            ioctl_download : out std_logic;
            ioctl_index    : out std_logic_vector(7 downto 0);

            SPI_SCK        : in std_logic;
            SPI_SS2        : in std_logic;
            SPI_SS4        : in std_logic;
            SPI_DI         : in std_logic;
            SPI_DO         : in std_logic  -- yes, sdo used as input
        );
    end component data_io;

begin

LED <= not core_led and not downloading and not bk_ena;

U00 : entity work.pll
    port map(
        inclk0 => CLOCK_27(0),	-- 27 MHz external
        c0     => MCLK,			-- 54 MHz internal
        c2     => memclk,			-- 108 Mhz
        c3     => SDRAM_CLK,		-- 108 Mhz external
		locked => pll_locked
    );

--SDRAM_A(12)<='0';

-- reset from IO controller
-- status bit 0 is always triggered by the io controller on its own reset
-- button 1 is the core specfic button in the mists front
-- reset <= '0' when status(0)='1' or buttons(1)='1' or pll_locked='0' else '1';

process(MCLK)
begin
	if rising_edge(MCLK) then
		reset_d<=not (status(0) or buttons(1)) and pll_locked;
		reset_n<=reset_d;
	end if;
end process;

ext_sw(1) <= SVP_EN; -- SVP
ext_sw(2) <= status(6); --joy swap
ext_sw(3) <= status(5); --psg en
ext_sw(4) <= status(4); --fm en
ext_sw(5) <= status(7); --Export
ext_sw(6) <= not status(8); --PAL
ext_sw(7) <= status(9); --swap Y
ext_sw(8) <= status(10); --3 buttons
ext_sw(9) <= not status(3); -- VRAM speed emulation
ext_sw(10) <= status(13); -- Fake EEPROM
ext_sw(12 downto 11) <= status(16 downto 15); -- Mouse
ext_sw(13) <= status(17); -- HiFi PCM
ext_sw(14) <= status(18); -- CPU Turbo
ext_sw(15) <= status(19); -- Border

--SDRAM_A(12)<='0';
sdram_top : entity work.fpgagen_sdram_top
port map(
	reset_n => reset_n,
	MCLK => MCLK,
	SDR_CLK => memclk,

	FPGA_INIT_N => pll_locked,
    DRAM_CKE => SDRAM_CKE,
    DRAM_CS_N => SDRAM_nCS,
    DRAM_RAS_N => SDRAM_nRAS,
    DRAM_CAS_N => SDRAM_nCAS,
    DRAM_WE_N => SDRAM_nWE,
    DRAM_UDQM => SDRAM_DQMH,
    DRAM_LDQM => SDRAM_DQML,
    DRAM_BA_1 => SDRAM_BA(1),
    DRAM_BA_0 => SDRAM_BA(0),
    DRAM_ADDR => SDRAM_A,
    DRAM_DQ => SDRAM_DQ,

    -- Joystick ports (Port_A, Port_B)
	joya => joy_1(11 downto 0),
	joyb => joy_0(11 downto 0),

    -- Mouse
    mouse_x => std_logic_vector(mouse_x)(7 downto 0),
    mouse_y => std_logic_vector(mouse_y)(7 downto 0),
    mouse_flags => mouse_flags,
    mouse_strobe => mouse_strobe,

	-- Video, Audio/CMT ports
    RED => gen_red,
    GREEN => gen_green,
    BLUE => gen_blue,

    HS => gen_hs,
    VS => gen_vs,
	 
    LED => core_led,

    DAC_LDATA => audiol,
    DAC_RDATA => audior,

    -- save ram
    saveram_addr    => std_logic_vector(sd_lba)(5 downto 0) & sd_buff_addr,
    saveram_we      => sd_dout_strobe,
    saveram_din     => sd_dout,
    saveram_rd      => sd_din_strobe,
    saveram_dout    => sd_din,

    ext_reset_n  => ext_reset_n(2) and ext_reset_n(1) and ext_reset_n(0),
    ext_bootdone => ext_bootdone,
    ext_data     => ext_data,
    ext_data_req => ext_data_req,
    ext_data_ack => ext_data_ack,
    
    ext_sw       => ext_sw
);

sd_conf <= '0';

user_io_inst : user_io
    generic map (
        STRLEN => CONF_STR'length,
        ROM_DIRECT_UPLOAD => true
	)
    port map (
        clk_sys => MCLK,
        clk_sd => MCLK,
        SPI_CLK => SPI_SCK,
        SPI_SS_IO => CONF_DATA0,
        SPI_MISO => SPI_DO,
        SPI_MOSI => SPI_DI,
        conf_str => to_slv(CONF_STR),
        status => status,
        ypbpr => ypbpr,
        no_csync => no_csync,
        scandoubler_disable => scandoubler_disable,

        joystick_0 => joy_0,
        joystick_1 => joy_1,
        joystick_analog_0 => open,
        joystick_analog_1 => open,
--      switches => switches,
        buttons => buttons,

        sd_lba  => std_logic_vector(sd_lba),
        sd_rd   => sd_rd,
        sd_wr   => sd_wr,
        sd_ack  => sd_ack,
        sd_sdhc => '1',
        sd_conf => sd_conf,
        sd_dout => sd_dout,
        sd_dout_strobe => sd_dout_strobe,
        sd_din => sd_din,
        sd_din_strobe => sd_din_strobe,
        sd_buff_addr => sd_buff_addr,
        img_mounted => img_mounted,
        img_size => img_size,

        ps2_kbd_clk => open,
        ps2_kbd_data => open,
        ps2_mouse_clk => open,
        ps2_mouse_data => open,
        mouse_x => mouse_x,
        mouse_y => mouse_y,
        mouse_flags => mouse_flags,
        mouse_strobe => mouse_strobe
 );

process (MCLK) begin
    if rising_edge(MCLK) then

        downloadingD_MCLK <= downloading;
        if downloadingD_MCLK = '0' and downloading = '1' then
            bk_ena <= '0';
        end if;

        img_mountedD <= img_mounted(0);
        if img_mountedD = '0' and img_mounted(0) = '1' then
            bk_ena <= '1';
            bk_load <= '1';
        end if;

        bk_loadD <= bk_load;
        bk_saveD <= status(14);
        sd_ackD  <= sd_ack;

        if sd_ackD = '0' and sd_ack = '1' then
            sd_rd(0) <= '0';
            sd_wr(0) <= '0';
        end if;

        if bk_state = '0' then
            if bk_ena = '1' and ((bk_loadD = '0' and bk_load = '1') or (bk_saveD = '0' and status(14) = '1')) then
                bk_state <= '1';
                sd_lba <= (others =>'0');
                sd_rd(0) <= bk_load;
                sd_wr(0) <= not bk_load;
            end if;
        else
            if sd_ackD = '1' and sd_ack = '0' then
                if sd_lba(5 downto 0) = "111111" then
                    bk_load <= '0';
                    bk_state <= '0';
                else
                    sd_lba <= sd_lba + 1;
                    sd_rd(0)  <= bk_load;
                    sd_wr(0)  <= not bk_load;
                end if;
            end if;
        end if;
    end if;
end process;

data_io_inst: data_io
    port map (
        clk_sys        => MCLK,
        clkref_n       => not data_io_clkref,
        ioctl_wr       => data_io_wr,
        ioctl_addr     => open,
        ioctl_dout     => data_io_d,
        ioctl_download => downloading,
        ioctl_index    => open,

        SPI_SCK        => SPI_SCK,
        SPI_SS2        => SPI_SS2,
        SPI_SS4        => SPI_SS4,
        SPI_DI         => SPI_DI,
        SPI_DO         => SPI_DO
    );

process(MCLK)
begin
    if rising_edge( MCLK ) then
        downloadingD <= downloading;
        ext_reset_n <= ext_reset_n(1 downto 0)&'1'; --stretch reset
        ext_data_ack <= '0';
        if (downloadingD = '0' and downloading = '1') then
            -- ROM downloading start
            ext_bootdone <= '0';
            ext_reset_n(0) <= '0';
            d_state <= "00";
            data_io_clkref <= '1';
        elsif (downloading = '0') then
            -- ROM downloading finished
            ext_bootdone <= '1';
            data_io_clkref <= '0';
        elsif (downloading = '1') then
            -- ROM downloading in progress
            case d_state is
            when "00" =>
                if data_io_wr = '1' then
                    ext_data(15 downto 8) <= data_io_d;
                    data_io_clkref <= '1';
                    d_state <= "01";
                end if;
            when "01" =>
                if data_io_wr = '1' then
                    ext_data(7 downto 0) <= data_io_d;
                    data_io_clkref <= '0';
                    d_state <= "10";
                end if;
            when "10" =>
                if ext_data_req = '1' then
                    ext_data_ack <= '1';
                    d_state <= "11";
                end if;
            when "11" =>
                data_io_clkref <= '1';
                d_state <= "00";
            end case;
        end if;
    end if;
end process;

mist_video : work.mist.mist_video
    generic map (
        SD_HCNT_WIDTH => 10,
		COLOR_DEPTH => 4,
		OSD_COLOR => "001" --blue
    )
    port map (
        clk_sys     => MCLK,
        scanlines   => status(12 downto 11),
        scandoubler_disable => scandoubler_disable,
        ypbpr       => ypbpr,
        no_csync    => no_csync,
        rotate      => "00",
        blend       => status(20),

        SPI_SCK     => SPI_SCK,
        SPI_SS3     => SPI_SS3,
        SPI_DI      => SPI_DI,

        HSync       => gen_hs,
        VSync       => gen_vs,
        R           => gen_red,
        G           => gen_green,
        B           => gen_blue,

        VGA_HS      => VGA_HS,
        VGA_VS      => VGA_VS,
        VGA_R       => VGA_R,
        VGA_G       => VGA_G,
        VGA_B       => VGA_B
    );

-- Do we have audio?  If so, instantiate a two DAC channels.
leftsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset_n,
		din => not audiol(15) & std_logic_vector(audiol(14 downto 0)),
		dout => AUDIO_L
	);
	
rightsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset_n,
		din => not audior(15) & std_logic_vector(audior(14 downto 0)),
		dout => AUDIO_R
	);

end architecture;
