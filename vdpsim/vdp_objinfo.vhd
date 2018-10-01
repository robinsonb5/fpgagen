LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

ENTITY vdp_objinfo IS
	PORT
	(
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		rdaddress		: IN STD_LOGIC_VECTOR (6 DOWNTO 0);
		wraddress		: IN STD_LOGIC_VECTOR (6 DOWNTO 0);
		wren		: IN STD_LOGIC  := '0';
		q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
	);
END vdp_objinfo;

architecture RTL of vdp_objinfo is

   type ram_type is array (0 to (2**rdaddress'length)-1) of std_logic_vector(data'range);
   signal ram : ram_type;

begin

  RamProc: process(clock) is

  begin
    if rising_edge(clock) then
      if wren = '1' then
        ram(to_integer(unsigned(wraddress))) <= data;
      end if;
      
      q <= ram(to_integer(unsigned(rdaddress)));
    end if;
  end process RamProc;

end architecture RTL;
