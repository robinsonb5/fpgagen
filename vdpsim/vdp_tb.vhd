library work;
use work.vram.all;
use work.video.all;
use work.cpu.all;

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
signal CPU_DTACK_N: std_logic;

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
      DTACK_N => CPU_DTACK_N,
      
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
  begin
    report "start";

    reset_n <= '0';
    wait for 5 ns; reset_n <= '1';
    
    assert false report "vdp out of reset"
      severity note;

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

 cpu : process (clk, reset_n)
    variable c : std_logic;
    variable r_n : std_logic;
    variable sel : std_logic;
    variable dtack_n : std_logic;
    variable rnw : std_logic;
    variable ds_n : std_logic_vector(1 downto 0);
    variable a : std_logic_vector(4 downto 0);
    variable d : std_logic_vector(15 downto 0);
  begin
    c := clk;
    r_n := reset_n;
    dtack_n := CPU_DTACK_N;
    
    cpu_c(c,r_n,sel,dtack_n,rnw,ds_n,a,d);

    CPU_SEL <= sel;
    CPU_RNW <= rnw;
    CPU_UDS_N <= ds_n(0);
    CPU_LDS_N <= ds_n(1);
    CPU_A <= a;
    CPU_DI <= d;
 end process cpu;

end vdp_tb;

