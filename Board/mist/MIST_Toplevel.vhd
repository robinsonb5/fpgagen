library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
 
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

signal reset : std_logic;
signal reset_d : std_logic;
signal pll_locked : std_logic;
signal MCLK      : std_logic;
signal memclk      : std_logic;

signal audiol : std_logic_vector(15 downto 0);
signal audior : std_logic_vector(15 downto 0);

signal vga_tred : unsigned(7 downto 0);
signal vga_tgreen : unsigned(7 downto 0);
signal vga_tblue : unsigned(7 downto 0);
signal vga_ths		: std_logic;
signal vga_tvs		: std_logic;
signal vga_window : std_logic;
signal vid_15khz	: std_logic;

-- 
signal vga_red_o : std_logic_vector(5 downto 0);
signal vga_green_o : std_logic_vector(5 downto 0);
signal vga_blue_o : std_logic_vector(5 downto 0);
signal osd_red_o : std_logic_vector(5 downto 0);
signal osd_green_o : std_logic_vector(5 downto 0);
signal osd_blue_o : std_logic_vector(5 downto 0);
signal vga_y_o : std_logic_vector(5 downto 0);
signal vga_pb_o : std_logic_vector(5 downto 0);
signal vga_pr_o : std_logic_vector(5 downto 0);

-- user_io
signal buttons: std_logic_vector(1 downto 0);
signal status:  std_logic_vector(7 downto 0);
signal joy_0: std_logic_vector(7 downto 0);
signal joy_1: std_logic_vector(7 downto 0);
signal joy_ana_0: std_logic_vector(15 downto 0);
signal joy_ana_1: std_logic_vector(15 downto 0);
signal txd:     std_logic;
signal par_out_data: std_logic_vector(7 downto 0);
signal par_out_strobe: std_logic;
signal ypbpr: std_logic;
signal scandoubler_disable: std_logic;

-- data_io
signal downloading      : std_logic;
signal data_io_wr       : std_logic;
signal data_io_clkref   : std_logic;
signal data_io_a        : std_logic_vector(24 downto 0);
signal data_io_d        : std_logic_vector(7 downto 0);
signal data_io_index    : std_logic_vector(7 downto 0);
signal downloadingD     : std_logic;
signal d_state          : std_logic_vector(1 downto 0);

-- sd card emulation
signal sd_cs:	std_logic;
signal sd_sck:	std_logic;
signal sd_sdi:	std_logic;
signal sd_sdo:	std_logic;

-- PS/2 Keyboard
signal ps2_keyboard_clk_in : std_logic;
signal ps2_keyboard_dat_in : std_logic;
signal ps2_keyboard_clk_out : std_logic;
signal ps2_keyboard_dat_out : std_logic;

-- PS/2 Mouse
signal ps2_mouse_clk_in : std_logic;
signal ps2_mouse_dat_in : std_logic;
signal ps2_mouse_clk_out : std_logic;
signal ps2_mouse_dat_out : std_logic;

-- external controller signals
signal ext_reset_n      : std_logic := '1';
signal ext_bootdone     : std_logic := '0';
signal ext_data         : std_logic_vector(15 downto 0);
signal ext_data_req     : std_logic;
signal ext_data_ack     : std_logic := '0';
signal ext_sw           : std_logic_vector( 15 downto 0); --DIP switches
signal core_led         : std_logic;

constant CONF_STR : string :=
    "GENESYS;BINGENMD ;"&
    "O4,FM Sound,Enable,Disable;"&
    "O5,PSG Sound,Enable,Disable;"&
    "O6,Joystick swap,Off,On;"&
    "T0,Reset;";

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

COMPONENT video_vga_dither
	GENERIC ( outbits : INTEGER := 4 );
	PORT
	(
		clk		:	 IN STD_LOGIC;
		hsync		:	 IN STD_LOGIC;
		vsync		:	 IN STD_LOGIC;
		vid_ena		:	 IN STD_LOGIC;
		iRed		:	 IN UNSIGNED(7 DOWNTO 0);
		iGreen		:	 IN UNSIGNED(7 DOWNTO 0);
		iBlue		:	 IN UNSIGNED(7 DOWNTO 0);
		oRed		:	 OUT UNSIGNED(outbits-1 DOWNTO 0);
		oGreen		:	 OUT UNSIGNED(outbits-1 DOWNTO 0);
		oBlue		:	 OUT UNSIGNED(outbits-1 DOWNTO 0)
	);
END COMPONENT;
  
-- 
COMPONENT rgb2ypbpr
	PORT
	(
        red     :	 IN std_logic_vector(5 DOWNTO 0);
        green   :	 IN std_logic_vector(5 DOWNTO 0);
        blue    :	 IN std_logic_vector(5 DOWNTO 0);
        y       :	 OUT std_logic_vector(5 DOWNTO 0);
        pb      :	 OUT std_logic_vector(5 DOWNTO 0);
        pr      :	 OUT std_logic_vector(5 DOWNTO 0)
	);
END COMPONENT;

component user_io 
	generic ( STRLEN : integer := 0 );
   port (
        clk_sys : in std_logic;
        clk_sd  : in std_logic;
        SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
        SPI_MISO : out std_logic;
        conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
        joystick_0 : out std_logic_vector(7 downto 0);
        joystick_1 : out std_logic_vector(7 downto 0);
        joystick_analog_0 : out std_logic_vector(15 downto 0);
        joystick_analog_1 : out std_logic_vector(15 downto 0);
        status: out std_logic_vector(7 downto 0);
        switches : out std_logic_vector(1 downto 0);
        buttons : out std_logic_vector(1 downto 0);
        scandoubler_disable: out std_logic;
        ypbpr: out std_logic;
        ps2_kbd_clk : out std_logic;
        ps2_kbd_data : out std_logic;
        ps2_mouse_clk : out std_logic;
        ps2_mouse_data : out std_logic;
        serial_data : in std_logic_vector(7 downto 0);
        serial_strobe : in std_logic
      );
  end component user_io;

component data_io
    port (  clk         : in std_logic;
            clkref      : in std_logic;
            wr          : out std_logic;
            a           : out std_logic_vector(24 downto 0);
            d           : out std_logic_vector(7 downto 0);
            downloading : out std_logic;
            index       : out std_logic_vector(7 downto 0);

            sck         : in std_logic;
            ss          : in std_logic;
            sdi         : in std_logic
        );
    end component data_io;

component osd
    port (  clk_sys     : in std_logic;

            R_in        : in std_logic_vector(5 downto 0);
            G_in        : in std_logic_vector(5 downto 0);
            B_in        : in std_logic_vector(5 downto 0);
            HSync       : in std_logic;
            VSync       : in std_logic;

            R_out       : out std_logic_vector(5 downto 0);
            G_out       : out std_logic_vector(5 downto 0);
            B_out       : out std_logic_vector(5 downto 0);

            SPI_SCK     : in std_logic;
            SPI_SS3     : in std_logic;
            SPI_DI      : in std_logic
        );
    end component osd;
	
component mist_console
	generic ( CLKFREQ : integer := 100 );
   port (  clk 	:	in std_logic;
           n_reset:	in std_logic;
           ser_in :	in std_logic;
           par_out_data :	out std_logic_vector(7 downto 0);
           par_out_strobe :	out std_logic
  );
  end component mist_console;

begin

LED <= core_led and not downloading;

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
-- status bit 0 is always triggered by the i ocontroller on its own reset
-- button 1 is the core specfic button in the mists front
-- reset <= '0' when status(0)='1' or buttons(1)='1' or pll_locked='0' else '1';

process(MCLK)
begin
	if rising_edge(MCLK) then
		reset_d<=not (status(0) or status(2) or buttons(1)) and pll_locked;
		reset<=reset_d;
	end if;
end process;

ext_sw(0) <= scandoubler_disable;
ext_sw(2) <= status(6); --joy swap
ext_sw(3) <= status(5); --psg en
ext_sw(4) <= status(4); --fm en

--SDRAM_A(12)<='0';
virtualtoplevel : entity work.Virtual_Toplevel
	generic map(
		rasCasTiming => 3,
		prechargeTiming => 3
	)
	port map(
		reset => reset,
		MCLK => MCLK,
		SDR_CLK => memclk,

    -- SDRAM DE1 ports
--	 pMemClk => DRAM_CLK,
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

    -- PS/2 keyboard ports
	 ps2k_clk_out => ps2_keyboard_clk_out,
	 ps2k_dat_out => ps2_keyboard_dat_out,
	 ps2k_clk_in => ps2_keyboard_clk_in,
	 ps2k_dat_in => ps2_keyboard_dat_in,
 
--    -- Joystick ports (Port_A, Port_B)
	joya => joy_1,
	joyb => joy_0,

    -- SD/MMC slot ports
	spi_clk => sd_sck,
	spi_mosi => sd_sdi,
	spi_cs => sd_cs,
	spi_miso => sd_sdo,

	-- Video, Audio/CMT ports
    unsigned(VGA_R) => vga_tred,
    unsigned(VGA_G) => vga_tgreen,
    unsigned(VGA_B) => vga_tblue,

    VGA_HS => vga_ths,
    VGA_VS => vga_tvs,
	 VID_15KHZ => vid_15khz,
	 
    LED => core_led,

    DAC_LDATA => audiol,
    DAC_RDATA => audior,
	 
    RS232_RXD => UART_RX,
    RS232_TXD => UART_TX,
     
    ext_controller => '1', --Use MiST OSD and ROM loader
    ext_reset_n  => ext_reset_n,
    ext_bootdone => ext_bootdone,
    ext_data     => ext_data,
    ext_data_req => ext_data_req,
    ext_data_ack => ext_data_ack,
    
    ext_sw       => ext_sw
);


-- UART_TX <='1';

mist_console_d: component mist_console
	generic map
	( CLKFREQ => 108)
	port map
	(
		clk => memclk,
		n_reset => reset,
		ser_in => txd,
		par_out_data => par_out_data,
		par_out_strobe => par_out_strobe
	);

user_io_inst : user_io
    generic map (STRLEN => CONF_STR'length)
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
        scandoubler_disable => scandoubler_disable,

        joystick_0 => joy_0,
        joystick_1 => joy_1,
        joystick_analog_0 => joy_ana_0,
        joystick_analog_1 => joy_ana_1,
--      switches => switches,
        buttons => buttons,
        ps2_kbd_clk => ps2_keyboard_clk_in,
        ps2_kbd_data => ps2_keyboard_dat_in,
        ps2_mouse_clk => ps2_mouse_clk_in,
        ps2_mouse_data => ps2_mouse_dat_in,
        serial_data => par_out_data,
        serial_strobe => par_out_strobe
 );

data_io_inst: data_io
    port map (
        clk     => memclk,
        clkref  => data_io_clkref,
        wr      => data_io_wr,
        a       => data_io_a,
        d       => data_io_d,
        downloading => downloading,
        index   => data_io_index,

        sck     => SPI_SCK,
        ss      => SPI_SS2,
        sdi     => SPI_DI
    );

process(memclk)
begin
    if rising_edge( memclk ) then
        downloadingD <= downloading;
        ext_reset_n <= '1';
        ext_bootdone <= '0';
        ext_data_ack <= '0';
        if (downloadingD = '0' and downloading = '1') then
            -- ROM downloading start
            ext_reset_n <= '0';
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

vga_window<='1';
mydither : component video_vga_dither
	generic map (
		outbits => 6
	)
	port map (
		clk => MCLK,
		hsync => vga_ths,
		vsync => vga_tvs,
		vid_ena => vga_window,
		iRed => vga_tred,
		iGreen => vga_tgreen,
		iBlue => vga_tblue,
		std_logic_vector(oRed) => vga_red_o,
		std_logic_vector(oGreen) => vga_green_o,
		std_logic_vector(oBlue) => vga_blue_o
	);

osd_inst: osd
    port map (
        clk_sys     => MCLK,

        SPI_SCK     => SPI_SCK,
        SPI_SS3     => SPI_SS3,
        SPI_DI      => SPI_DI,

        R_in        => vga_red_o,
        G_in        => vga_green_o,
        B_in        => vga_blue_o,
        HSync       => vga_ths,
        VSync       => vga_tvs,

        R_out       => osd_red_o,
        G_out       => osd_green_o,
        B_out       => osd_blue_o
    );

--
rgb2component: component rgb2ypbpr
	port map
	(
	   red => osd_red_o,
	   green => osd_green_o,
	   blue => osd_blue_o,
	   y => vga_y_o,
	   pb => vga_pb_o,
	   pr => vga_pr_o
	);
 
 -- If 15kHz Video - composite sync to VGA_HS and VGA_VS high for MiST RGB cable
VGA_HS <= not (vga_ths xor vga_tvs) when vid_15khz='1' or ypbpr='1' else vga_ths;
VGA_VS <= '1' when vid_15khz='1' or ypbpr='1' else vga_tvs;
VGA_R <= vga_pr_o when ypbpr='1' else osd_red_o;
VGA_G <= vga_y_o  when ypbpr='1' else osd_green_o;
VGA_B <= vga_pb_o when ypbpr='1' else osd_blue_o;

-- Do we have audio?  If so, instantiate a two DAC channels.
leftsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset,
		din => not audiol(15) & std_logic_vector(audiol(14 downto 0)),
		dout => AUDIO_L
	);
	
rightsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset,
		din => not audior(15) & std_logic_vector(audior(14 downto 0)),
		dout => AUDIO_R
	);

end architecture;
