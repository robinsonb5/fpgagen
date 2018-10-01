LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

ENTITY vdp_colinfo IS
	PORT
	(
		address_a		: IN STD_LOGIC_VECTOR (8 DOWNTO 0);
		address_b		: IN STD_LOGIC_VECTOR (8 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		data_a		: IN STD_LOGIC_VECTOR (6 DOWNTO 0);
		data_b		: IN STD_LOGIC_VECTOR (6 DOWNTO 0);
		wren_a		: IN STD_LOGIC  := '0';
		wren_b		: IN STD_LOGIC  := '0';
		q_a		: OUT STD_LOGIC_VECTOR (6 DOWNTO 0);
		q_b		: OUT STD_LOGIC_VECTOR (6 DOWNTO 0)
	);
END vdp_colinfo;


architecture RTL of vdp_colinfo is

   type ram_type is array (0 to (2**address_a'length)-1) of std_logic_vector(data_a'range);
   signal ram : ram_type;

begin

  RamProc: process(clock) is

  begin
    if rising_edge(clock) then
      if wren_a = '1' then
        ram(to_integer(unsigned(address_a))) <= data_a;
      end if;
      if wren_b = '1' then
        ram(to_integer(unsigned(address_b))) <= data_b;
      end if;
      q_a <= ram(to_integer(unsigned(address_a)));
      q_b <= ram(to_integer(unsigned(address_b)));
    end if;
  end process RamProc;

end architecture RTL;
