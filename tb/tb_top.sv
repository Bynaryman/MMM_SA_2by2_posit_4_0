`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
//
// Engineer: LEDOUX Louis
// Github : Bynaryman
//
// Description: test bench for systolic array
// 	read a file that contains one data / line
//
//////////////////////////////////////////////////////////////////////////////////


module tb_top();

    localparam CLK_PERIOD      = 2;
    localparam CLK_HALF_PERIOD = CLK_PERIOD / 2;

    //----------------------------------------------------------------
    // Signals, clocks, and reset
    //----------------------------------------------------------------

    logic tb_clk;
    logic tb_reset_n;

    logic start = 0;

    logic s_axis_in_aclk;
    logic s_axis_in_tready;
    logic s_axis_in_aresetn;
    logic s_axis_in_tvalid;
    logic [18-1:0] s_axis_in_tdata;
    logic [7:0] s_axis_in_tstrb;

    // output signals
    logic [7:0] matrix_o;
    logic valid_o;


    axi_stream_generator_from_file  #
    (
        .WIDTH(18),
	.base_path("./"),
        .path("matrices.txt"),
	.READ_B_OR_H( "B" ),
	.nb_data ( 2 )
    ) axi_stream_generator_inst
    (
        .rst_n ( tb_reset_n ),
        // Starts an axi_stream transaction
        .start                        ( start ),  //  in std_logic;

        // axi stream ports
        .m_axis_clk                   ( tb_clk           ),  // in  std_logic;
        .m_axis_tvalid                ( s_axis_in_tvalid ),  //  out std_logic;
        .m_axis_tdata                 ( s_axis_in_tdata  ),  //  out std_logic_vector(31 downto 0);
        .m_axis_tstrb                 ( s_axis_in_tstrb  ),  //  out std_logic_vector(3 downto 0);
        .m_axis_tlast                 ( s_axis_in_tlast  )   //  out std_logic
    );



    // instanciate DUT
    top top_inst (
        .clk     ( tb_clk           ),
	.rst_n   ( tb_reset_n       ),

	.valid_i ( s_axis_in_tvalid ),
	.data_i  ( s_axis_in_tdata  ),

	.valid_o ( valid_o          ),
	.data_o  ( matrix_o         )
    );

    //----------------------------------------------------------------
    // clk_gen
    //
    // Always running clock generator process.
    //----------------------------------------------------------------
    always
    begin : clk_gen
        #CLK_HALF_PERIOD;
        tb_clk = !tb_clk;
    end // clk_gen


    //----------------------------------------------------------------
    // reset_dut()
    //
    // Toggle reset to put the DUT into a well known state.
    //----------------------------------------------------------------
    task reset_dut;
        begin
            $display("*** Toggle reset.");
            tb_reset_n = 0;
            #(2 * CLK_PERIOD);
            tb_reset_n = 1;
        end
    endtask // reset_dut


    //----------------------------------------------------------------
    // init_sim()
    //
    // All the init part
    //----------------------------------------------------------------
    task init_sim;
        begin
            $display("*** init sim.");
            tb_clk = 0;
            tb_reset_n = 1;
        end
    endtask // reset_dut

    //----------------------------------------------------------------
    // init sim
    //----------------------------------------------------------------
    initial begin

        assign s_axis_in_aclk    = tb_clk;
        assign s_axis_in_aresetn = tb_reset_n;

        reset_dut();
        init_sim();

        start = 1;

    end


endmodule
