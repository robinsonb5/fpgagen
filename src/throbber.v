module throbber
(
	input clk,
	input reset_n,
	output reg q
);

reg [7:0] pwm;
reg [16:0] counter;

wire [7:0] limit = counter[15:8] ^ {8{counter[16]}};

always @(posedge clk,negedge reset_n)
begin
	if(!reset_n) begin
		counter<=16'b0;
		pwm<=8'b0;
	end else begin
		pwm<=pwm+1'b1;
		
		if(pwm==limit)
			q<=1'b1;
		
		if(!(|pwm)) begin
			q<=1'b0;
			counter<=counter+1'b1;
		end
	end
end

endmodule
