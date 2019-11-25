//
// data_io.v
//
// io controller writable ram for the MiST board
// http://code.google.com/p/mist-board/
//
// Copyright (c) 2014 Till Harbaum <till@harbaum.org>
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

module data_io (
	// io controller spi interface
    input               sck,
    input               ss,
    input               ss4,
    input               sdi,
    input               sdo,

    output reg          downloading,   // signal indicating an active download
    output reg [7:0]    index,         // menu index used to upload the file

    // external ram interface
    input               clk,
    input               clkref,
    output reg          wr,
    output reg [24:0]   a,
    output reg [7:0]    d
);

parameter START_ADDR = 25'h0;

// *********************************************************************************
// spi client
// *********************************************************************************

reg [6:0]      sbuf;
reg [7:0]      cmd;
reg [7:0]      data;
reg [3:0]      cnt;
reg            rclk = 0;

reg [9:0]      bytecnt;
reg [6:0]      sbuf2;
reg [7:0]      data2;
reg [2:0]      cnt2;
reg            rclk2 = 0;

reg [24:0]     addr;
reg            addr_reset = 0;

localparam UIO_FILE_TX      = 8'h53;
localparam UIO_FILE_TX_DAT  = 8'h54;
localparam UIO_FILE_INDEX   = 8'h55;

reg       downloading_reg = 1'b0;
reg [7:0] index_reg;

// data_io has its own SPI interface to the io controller
always@(posedge sck, posedge ss) begin
	if(ss == 1'b1)
		cnt <= 0;
	else begin
		// don't shift in last bit. It is evaluated directly
		// when writing to ram
		if(cnt != 15)
			sbuf <= { sbuf[5:0], sdi};

		// count 0-7 8-15 8-15 ... 
		if(cnt == 15) cnt <= 4'd8;
		else cnt <= cnt + 1'd1;

		// finished command byte
		 if(cnt == 7)
			cmd <= {sbuf, sdi};

		// prepare/end transmission
		if((cmd == UIO_FILE_TX) && (cnt == 15)) begin
			// prepare
			if(sdi) begin
				addr_reset <= ~addr_reset;
				downloading_reg <= 1'b1; 
			end else
				downloading_reg <= 1'b0; 
		end

		// command 0x54: UIO_FILE_TX
		if((cmd == UIO_FILE_TX_DAT) && (cnt == 15)) begin
			data <= {sbuf, sdi};
			rclk <= ~rclk;
		end

		// expose file (menu) index
		if((cmd == UIO_FILE_INDEX) && (cnt == 15))
			index_reg <= {sbuf[3:0], sdi};
	end
end

// direct transfer
always@(posedge sck, posedge ss4) begin
	if(ss4 == 1'b1) begin
		cnt2 <= 0;
		bytecnt <= 0;
	end else begin
		// don't shift in last bit. It is evaluated directly
		// when writing to ram
		if(cnt2 != 7)
			sbuf2 <= { sbuf2[5:0], sdo};

		cnt2 <= cnt2 + 1'd1;

		// received a byte
		 if(cnt2 == 7) begin
			bytecnt <= bytecnt + 1'd1;
			// read 514 byte/sector (512 + 2 CRC)
			if (bytecnt == 513) bytecnt <= 0;
			// don't send the CRC bytes
			if (~bytecnt[9]) begin
				data2 <= {sbuf2, sdo};
				rclk2 <= ~rclk2;
			end
		end
	end
end

always@(posedge clk) begin
	// bring rclk from spi clock domain into core clock domain
	reg rclkD, rclkD2;
	reg rclk2D, rclk2D2;
	reg addr_resetD, addr_resetD2;
	reg wr_int, wr_int_direct;

	{ rclkD, rclkD2 } <= { rclk, rclkD };
	{ rclk2D ,rclk2D2 } <= { rclk2, rclk2D };
	{ addr_resetD, addr_resetD2 } <= { addr_reset, addr_resetD };

	wr <= 0;

	downloading <= downloading_reg;
	index <= index_reg;

	if (clkref) begin
		wr_int <= 0;
		wr_int_direct <= 0;
		if (wr_int || wr_int_direct) begin
			d <= wr_int ? data : data2;
			wr <= 1'b1;
			addr <= addr + 1'd1;
			a <= addr;
		end
	end

	// detect transfer start from the SPI receiver
	if(addr_resetD ^ addr_resetD2)
		addr <= START_ADDR;

	// detect new byte from the SPI receiver
	if (rclkD ^ rclkD2)   wr_int <= 1;
	if (rclk2D ^ rclk2D2) wr_int_direct <= 1;
end

endmodule