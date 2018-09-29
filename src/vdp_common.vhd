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

library IEEE;
	use IEEE.STD_LOGIC_1164.ALL;
	use IEEE.STD_LOGIC_UNSIGNED.ALL;

package vdp_common is

constant H_DISP_CLOCKS          : integer := 2560;

constant CLOCKS_PER_LINE_MAX    : integer := 3420;
constant CLOCKS_PER_LINE_H32    : integer := 342*10;
constant CLOCKS_PER_LINE_H40    : integer := 427*8; -- 3416

constant HS_CLOCKS              : integer := 240; -- multiple of 8 and 10, too
constant VGA_HS_CLOCKS          : integer := 204;       -- 3.77 us

constant VGA_VS_LINES           : integer := 1;         -- 0.06 ms
constant VS_LINES               : integer := 3;

constant H_DISP_START_H32       : integer := HS_CLOCKS + 46*10;
constant H_DISP_START_H40       : integer := HS_CLOCKS + 46*8;

constant H_DISP_WIDTH_H32       : integer := 256; -- in cells
constant H_DISP_WIDTH_H40       : integer := 320;

constant V_DISP_START_V28       : integer := 27;
constant V_DISP_START_V30       : integer := 46;

constant V_DISP_HEIGHT_V28      : integer := 224;
constant V_DISP_HEIGHT_V30      : integer := 240;

constant NTSC_LINES             : integer := 262;
constant PAL_LINES              : integer := 312;

end vdp_common;
