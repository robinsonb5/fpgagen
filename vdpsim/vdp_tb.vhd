library work;
use work.vram.all;
use work.video.all;

library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

-- `timescale 1us/1ns
    
entity vdp_tb is
end;

architecture vdp_tb of vdp_tb is

  -- include vdp
component vdp
       port(
                RST_N           : in std_logic;
                CLK                     : in std_logic;
                MEMCLK  : in std_logic;

                SEL                     : in std_logic;
                A                       : in std_logic_vector(4 downto 0);
                RNW                     : in std_logic;
                UDS_N           : in std_logic;
                LDS_N           : in std_logic;
                DI                      : in std_logic_vector(15 downto 0);
                DO                      : out std_logic_vector(15 downto 0);
                DTACK_N         : out std_logic;

                vram_req : out std_logic;
                vram_ack : in std_logic;
                vram_we : out std_logic;
                vram_a : buffer std_logic_vector(14 downto 0);
                vram_d : out std_logic_vector(15 downto 0);
                vram_q : in std_logic_vector(15 downto 0);
                vram_u_n : out std_logic;
                vram_l_n : out std_logic;

                INTERLACE       : in std_logic;

                HINT            : out std_logic;
                HINT_ACK        : in std_logic;

                VINT_TG68       : out std_logic;
                VINT_T80        : out std_logic;
                VINT_TG68_ACK   : in std_logic;
                VINT_T80_ACK    : in std_logic;

                VBUS_ADDR               : out std_logic_vector(23 downto 0);
                VBUS_UDS_N              : out std_logic;
                VBUS_LDS_N              : out std_logic;
                VBUS_DATA               : in std_logic_vector(15 downto 0);

                VBUS_SEL                : out std_logic;
                VBUS_DTACK_N    : in std_logic;

                PAL             : in std_logic := '0';
                R               : out std_logic_vector(3 downto 0);
                G               : out std_logic_vector(3 downto 0);
                B               : out std_logic_vector(3 downto 0);
                HS              : out std_logic;
                VS              : out std_logic;

                VGA_R           : out std_logic_vector(3 downto 0);
                VGA_G           : out std_logic_vector(3 downto 0);
                VGA_B           : out std_logic_vector(3 downto 0);
                VGA_HS          : out std_logic;
                VGA_VS          : out std_logic
        );
end component ;

signal   clk      : std_logic := '0';
signal   reset_n  : std_logic := '1';

signal vram_req_loop: std_logic;
signal vram_a: std_logic_vector(14 downto 0);
signal vram_d: std_logic_vector(15 downto 0);

signal CPU_SEL: std_logic;
signal CPU_A: std_logic_vector(4 downto 0);
signal CPU_RNW: std_logic;
signal CPU_UDS_N: std_logic;
signal CPU_LDS_N: std_logic;
signal CPU_DI: std_logic_vector(15 downto 0);

signal VIDEO_R: std_logic_vector(3 downto 0);
signal VIDEO_G: std_logic_vector(3 downto 0);
signal VIDEO_B: std_logic_vector(3 downto 0);
signal VIDEO_HS: std_logic;
signal VIDEO_VS: std_logic;
    
begin
  -- wire up vdp
  vdp_i : vdp 
    port map (
      RST_N => reset_n,
      CLK => clk,
      MEMCLK => clk,

      -- CPU bus interface
      SEL => CPU_SEL,
      A => CPU_A,
      RNW => CPU_RNW,
      UDS_N => CPU_UDS_N,
      LDS_N => CPU_LDS_N,
      DI => CPU_DI,
      
      vram_a => vram_a,
--      vram_d => ,
      vram_req => vram_req_loop,
      vram_ack => vram_req_loop,
      vram_q => vram_d,

      INTERLACE => '0',

      HINT_ACK => '0',
      VINT_TG68_ACK => '0',
      VINT_T80_ACK => '0',

      VBUS_DATA => "0000000000000000",

      VBUS_DTACK_N => '0',

      PAL => '0',
      R => VIDEO_R,
      G => VIDEO_G,
      B => VIDEO_B,
      HS => VIDEO_HS,
      VS => VIDEO_VS
  );

-- generate a 50mhz clock
  clock : process
  begin
    wait for 10 ns; clk  <= not clk;
  end process clock;

  stimulus : process
    procedure WRITE_REG
      ( a: in std_logic_vector(3 downto 0);
        d: in std_logic_vector(15 downto 0) ) is
    begin
      CPU_UDS_N <= '0';
      CPU_LDS_N <= '0';
      CPU_RNW <= '0';
      CPU_A <= "0" & a;
      CPU_DI <= d;  
      CPU_SEL <= '1';
      wait for 60 ns;
      CPU_SEL <= '0';
      wait for 40 ns;   

    end WRITE_REG;
    
    procedure WRITE_VDP_REG
      ( d: in std_logic_vector(15 downto 0) ) is
    begin
      WRITE_REG(x"4", d);
    end WRITE_VDP_REG;

  begin
    report "start";

    -- reset cpu signals
    CPU_SEL <= '0';
    CPU_A <= "00000"; 
    CPU_RNW <= '1';
    CPU_UDS_N <= '1';
    CPU_LDS_N <= '1';
    CPU_DI <= "0000000000000000";
      
    reset_n <= '0';
    wait for 5 ns; reset_n <= '1';
    
    assert false report "vdp out of reset"
      severity note;

    -- from Sprite main.asm
    WRITE_VDP_REG(x"8004"); -- Enable the palette
    WRITE_VDP_REG(x"8144"); -- Enable the display/mode 5
    WRITE_VDP_REG(x"8230"); -- Set the scroll A name table base to 0xC000
    WRITE_VDP_REG(x"8405"); -- Set the scroll B name table base to 0xA000
    WRITE_VDP_REG(x"8570"); -- Set the sprite table base to 0xE000
    WRITE_VDP_REG(x"8D3F"); -- Set the hscroll base to 0xFC00
    WRITE_VDP_REG(x"9001"); -- Set the scroll size to V32 H64
    WRITE_VDP_REG(x"8F02"); -- Set the auto-increment data to 2

    -- CRAM
    WRITE_REG(x"4", x"C000"); WRITE_REG(x"4", x"0000");
    WRITE_REG(x"0", x"0000"); WRITE_REG(x"0", x"0EEE");
    WRITE_REG(x"0", x"000E"); WRITE_REG(x"0", x"00E0");
    WRITE_REG(x"0", x"0E00"); WRITE_REG(x"0", x"0888");
    WRITE_REG(x"0", x"0008"); WRITE_REG(x"0", x"0080");
    WRITE_REG(x"0", x"0800"); WRITE_REG(x"0", x"0000");
    WRITE_REG(x"0", x"0000"); WRITE_REG(x"0", x"0000");
    WRITE_REG(x"0", x"0000"); WRITE_REG(x"0", x"0800");
   
    wait;
  end process stimulus;
  
  memory : process (clk)
    variable c : std_logic;
    variable a : std_logic_vector(14 downto 0);
    variable d : std_logic_vector(15 downto 0);
  begin
    -- wire memory
    c := clk;
    a := vram_a;
    vram_c(c,a,d);
    
    if (clk = '0' and clk'event) then
      vram_d <= d;    
    end if;   
 end process memory;

 video : process (clk)
    variable c : std_logic;
    variable hs : std_logic;
    variable vs : std_logic;
    variable r : std_logic_vector(3 downto 0);
    variable g : std_logic_vector(3 downto 0);
    variable b : std_logic_vector(3 downto 0);
  begin
    -- wire memory
    c := clk;
    hs := VIDEO_HS; vs := VIDEO_VS;
    r := VIDEO_R; g := VIDEO_G; b := VIDEO_B;
    video_c(c,r,g,b,hs,vs);
 end process video;

end vdp_tb;

