`timescale 1ns / 1ps
`default_nettype none
//////////////////////////////////////////////////////////////////////////////////
//
// Engineer: LEDOUX Louis
// Github : Bynaryman
// Description: SV wrapper adding valid_in and valid_out to the systolic array
//
//////////////////////////////////////////////////////////////////////////////////


module top
(
    // System signals
    input  wire clk,
    input  wire rst_n,

    input  wire valid_i,
    input wire [18-1:0] data_i,

    output logic valid_o,
    output logic [8-1:0] data_o
);

// localparams
localparam integer POSIT_WIDTH = 4;
localparam integer POSIT_ES    = 0;
localparam integer N = 2;
localparam integer M = 2;
// this number exists only after a flopoco run
localparam integer PDFDP_PP_DEPTH = 1;
localparam integer Q2P_PP_DEPTH = 1;

// signals

// SA
logic [18-1:0] sa_data_i;
logic [8-1:0] sa_data_o;
logic sa_eob;
logic sa_eob_q;
logic sa_valid_o;
logic sa_sob;

assign sa_data_i = (valid_i) ? data_i : {18{1'b0}};
assign sa_eob = sa_data_i[17];
assign sa_sob = sa_data_i[16];

SystolicArray sa_inst (

    // System
    .clk     ( clk                                            ),
    .rst     ( ~rst_n                                         ),

    // IOs
    .rowsA   ( sa_data_i[(N*POSIT_WIDTH)-1:0]                 ),
    .colsB   ( sa_data_i[((M+N)*POSIT_WIDTH)-1:N*POSIT_WIDTH] ),
    .SOB     ( sa_sob                                         ),
    .EOB     ( sa_eob                                         ),
    .colsC   ( sa_data_o                                      ),
    .EOB_Q_o ( sa_eob_q                                       )

);

// valid_o from systolic array logic
// we will create a shift register of size N+2+PP_DEPTH(PDFDP)-1
// we connect the eob_q of PE(N-1,M-1) as the input bit
// the OR reduction from the N MSB bits are the valid signal
localparam integer size = N+2+PDFDP_PP_DEPTH-1+Q2P_PP_DEPTH-1;
logic [size-1:0] shift_register;
always_ff @(posedge clk or negedge rst_n) begin
	if ( ~rst_n ) begin
		shift_register <= 0;
	end
	else begin
		shift_register <= {shift_register[size-2:0],sa_eob_q};
	end
end
assign sa_valid_o = |shift_register[(size-1) -: N];

// OUT
assign valid_o  = sa_valid_o;
assign data_o = sa_data_o;

endmodule
`default_nettype wire
