`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

`include "vcTest.v"

module vuVXU_TestHarness;

  reg clk = 0;
  reg reset = 1;

  always #0.5 clk = ~clk;

  `VC_TEST_SUITE_BEGIN( "vxu" )

  wire       fire_val;
  wire       fire_done;
  wire [8:0] fire_bits;

  wire         cmd_val;
  wire         cmd_done;
  wire [125:0] cmd_bits;

  wire        vlu_rdy;
  wire        vlu_val;
  wire        vlu_sel;
  wire        vlu_done;
  wire [64:0] vlu_bits;

  wire        vsu_rdy;
  wire        vsu_val;
  wire        vsu_sel;
  wire        vsu_done;
  wire [64:0] vsu_bits;

  vcTestSource#(9, 0) fire
  (
    .clk(clk),
    .reset(reset),

    .bits(fire_bits),
    .val(fire_val),
    .rdy(1'b1),
    .done(fire_done)
  );

  vcTestSource#(126, 0) cmd
  (
    .clk(clk),
    .reset(reset),

    .bits(cmd_bits),
    .val(cmd_val),
    .rdy(1'b1),
    .done(cmd_done)
  );

  vcTestSource#(65, 0) vlu
  (
    .clk(clk),
    .reset(reset),

    .bits(vlu_bits),
    .val(vlu_val),
    .rdy(vlu_rdy),
    .done(vlu_done)
  );

  reg [7:0]  fire_fn_viu;
  reg [2:0]  fire_fn_vau0;
  reg [2:0]  fire_fn_vau1;
  reg [2:0]  fire_fn_vau2;
  reg [13:0]  fire_vlen;
  reg [5:0]  fire_vs;
  reg [5:0]  fire_vt;
  reg [5:0]  fire_vr;
  reg [5:0]  fire_vd;
  reg [5:0]  fire_stride;
  reg [64:0] fire_imm;

  always @(*)
  begin
    {fire_fn_viu,fire_fn_vau0,fire_fn_vau1,fire_fn_vau2,fire_vlen,fire_vs,fire_vt,fire_vr,fire_vd,fire_stride,fire_imm}
      = cmd_bits;
  end

  wire expand_last;

  wire expand_viu;
  wire expand_vau0;
  wire expand_vau1;
  wire expand_vau2;
  wire expand_vlu;
  wire expand_vsu;
  wire expand_vgu;
  wire expand_vlxu;
  wire expand_vsxu;

  wire [7:0]  expand_fn_viu;
  wire [2:0]  expand_fn_vau0;
  wire [2:0]  expand_fn_vau1;
  wire [2:0]  expand_fn_vau2;
  wire [2:0]  expand_cnt;
  wire [7:0]  expand_vs;
  wire [7:0]  expand_vt;
  wire [7:0]  expand_vr;
  wire [7:0]  expand_vd;
  wire [64:0] expand_imm;

  wire        in_ren;
  wire        in_rlast;
  wire [2:0]  in_rcnt;
  wire [7:0]  in_raddr;
  wire [1:0]  in_ropcen;
  wire [7:0]  in_rblen;

  wire        in_wen;
  wire        in_wlast;
  wire [2:0]  in_wcnt;
  wire [7:0]  in_waddr;
  wire [2:0]  in_wsel;

  wire        in_viu_val;
  wire [7:0]  in_viu_fn;
  wire [64:0] in_viu_imm;
  wire        in_vau0_val;
  wire [2:0]  in_vau0_fn;
  wire        in_vau1_val;
  wire [2:0]  in_vau1_fn;
  wire        in_vau2_val;
  wire [2:0]  in_vau2_fn;
  wire        in_vgu_val;
  wire        in_vlu_rdy;
  wire        in_vlu_sel;
  wire        in_vsu_val;
  wire        in_vsu_sel;

  vuVXU_Banked8_Seq b8seq
  (
    .clk(clk),
    .reset(reset),

    .vldq_qstall(1'b0),
    .vsdq_qstall(1'b0),
    .utaq_qstall(1'b0),
    .utldq_qstall(1'b0),
    .utsdq_qstall(1'b0),

    .fire_viu(fire_bits[0]),
    .fire_vau0(fire_bits[1]),
    .fire_vau1(fire_bits[2]),
    .fire_vau2(fire_bits[3]),
    .fire_vgslu(fire_bits[4]),
    .fire_vglu(fire_bits[5]),
    .fire_vgsu(fire_bits[6]),
    .fire_vlu(fire_bits[7]),
    .fire_vsu(fire_bits[8]),

    .fire_fn_viu(fire_fn_viu),
    .fire_fn_vau0(fire_fn_vau0),
    .fire_fn_vau1(fire_fn_vau1),
    .fire_fn_vau2(fire_fn_vau2),

    .fire_vlen(fire_vlen),
    .fire_vs(fire_vs),
    .fire_vt(fire_vt),
    .fire_vr(fire_vr),
    .fire_vd(fire_vd),
    .fire_stride(fire_stride),
    .fire_imm(fire_imm),

    .expand_last(expand_last),

    .expand_viu(expand_viu),
    .expand_vau0(expand_vau0),
    .expand_vau1(expand_vau1),
    .expand_vau2(expand_vau2),
    .expand_vlu(expand_vlu),
    .expand_vsu(expand_vsu),
    .expand_vgu(expand_vgu),
    .expand_vlxu(expand_vlxu),
    .expand_vsxu(expand_vsxu),

    .expand_fn_viu(expand_fn_viu),
    .expand_fn_vau0(expand_fn_vau0),
    .expand_fn_vau1(expand_fn_vau1),
    .expand_fn_vau2(expand_fn_vau2),

    .expand_cnt(expand_cnt),
    .expand_vs(expand_vs),
    .expand_vt(expand_vt),
    .expand_vr(expand_vr),
    .expand_vd(expand_vd),
    .expand_imm(expand_imm)
  );

  vuVXU_Banked8_Expand b8expand
  (
    .clk(clk),
    .reset(reset),

    .expand_last(expand_last),

    .expand_viu(expand_viu),
    .expand_vau0(expand_vau0),
    .expand_vau1(expand_vau1),
    .expand_vau2(expand_vau2),
    .expand_vlu(expand_vlu),
    .expand_vsu(expand_vsu),
    .expand_vgu(expand_vgu),
    .expand_vlxu(expand_vlxu),
    .expand_vsxu(expand_vsxu),

    .expand_fn_viu(expand_fn_viu),
    .expand_fn_vau0(expand_fn_vau0),
    .expand_fn_vau1(expand_fn_vau1),
    .expand_fn_vau2(expand_fn_vau2),

    .expand_cnt(expand_cnt),
    .expand_vs(expand_vs),
    .expand_vt(expand_vt),
    .expand_vr(expand_vr),
    .expand_vd(expand_vd),
    .expand_imm(expand_imm),

    .in_ren(in_ren),
    .in_rlast(in_rlast),
    .in_rcnt(in_rcnt),
    .in_raddr(in_raddr),
    .in_ropcen(in_ropcen),
    .in_rblen(in_rblen),

    .in_wen(in_wen),
    .in_wlast(in_wlast),
    .in_wcnt(in_wcnt),
    .in_waddr(in_waddr),
    .in_wsel(in_wsel),

    .in_viu_val(in_viu_val),
    .in_viu_fn(in_viu_fn),
    .in_viu_imm(in_viu_imm),

    .in_vau0_val(in_vau0_val),
    .in_vau0_fn(in_vau0_fn),
    .in_vau1_val(in_vau1_val),
    .in_vau1_fn(in_vau1_fn),
    .in_vau2_val(in_vau2_val),
    .in_vau2_fn(in_vau2_fn),
    .in_vgu_val(in_vgu_val),
    .in_vlu_rdy(in_vlu_rdy),
    .in_vlu_sel(in_vlu_sel),
    .in_vsu_val(in_vsu_val),
    .in_vsu_sel(in_vsu_sel)
  );

  vuVXU_Banked8_Hazard b8hazard
  (
    .clk(clk),
    .reset(reset),

    .check_vs(6'd1),
    .check_vt(6'd2),
    .check_vr(6'd3),
    .check_vd(6'd3),

    .vs_dhazard(),
    .vt_dhazard(),
    .vr_dhazard(),
    .vd_dhazard(),

    .vau0_shazard(),
    .vau1_shazard(),
    .vau2_shazard(),
    .vgu_shazard(),
    .vlu_shazard(),
    .vsu_shazard(),

    .r1w1_bhazard(),
    .r2w1_bhazard(),
    .r3w1_bhazard(),
    .vgslu_bhazard(),
    .vglu_bhazard(),
    .vgsu_bhazard(),
    .vlu_bhazard(),
    .vsu_bhazard(),

    .fire_viu(fire_bits[0]),
    .fire_vau0(fire_bits[1]),
    .fire_vau1(fire_bits[2]),
    .fire_vau2(fire_bits[3]),
    .fire_vgslu(fire_bits[4]),
    .fire_vglu(fire_bits[5]),
    .fire_vgsu(fire_bits[6]),
    .fire_vlu(fire_bits[7]),
    .fire_vsu(fire_bits[8]),

    .fire_fn_viu(fire_fn_viu),
    .fire_fn_vau0(fire_fn_vau0),
    .fire_fn_vau1(fire_fn_vau1),
    .fire_fn_vau2(fire_fn_vau2),

    .fire_vd(fire_vd),

    .in_ren(in_ren),
    .in_rlast(in_rlast),
    .in_wen(in_wen),
    .in_wlast(in_wlast)
  );

  vuVXU_Banked8_Lane b8lane
  (
    .clk(clk),
    .reset(reset),

    .in_ren(in_ren),
    .in_rcnt(in_rcnt),
    .in_raddr(in_raddr),
    .in_ropcen(in_ropcen),
    .in_rblen(in_rblen),

    .in_wen(in_wen),
    .in_wcnt(in_wcnt),
    .in_waddr(in_waddr),
    .in_wsel(in_wsel),

    .in_viu_val(in_viu_val),
    .in_viu_fn(in_viu_fn),
    .in_viu_imm(in_viu_imm),

    .in_vau0_val(in_vau0_val),
    .in_vau0_fn(in_vau0_fn),
    .in_vau1_val(in_vau1_val),
    .in_vau1_fn(in_vau1_fn),
    .in_vau2_val(in_vau2_val),
    .in_vau2_fn(in_vau2_fn),
    .in_vgu_val(in_vgu_val),
    .in_vlu_rdy(in_vlu_rdy),
    .in_vlu_sel(in_vlu_sel),
    .in_vsu_val(in_vsu_val),
    .in_vsu_sel(in_vsu_sel),

    .vgu_val(),
    .vgu_rdata(),
    .vlu_rdy(vlu_rdy),
    .vlu_sel(vlu_sel),
    .vlu_wdata(vlu_bits),
    .vsu_val(vsu_val),
    .vsu_sel(vsu_sel),
    .vsu_rdata(vsu_bits)
  );

  vcTestSink#(65, 0) vsu
  (
    .clk    (clk),
    .reset  (reset),

    .bits   (vsu_bits),
    .val    (vsu_val),
    .rdy    (vsu_rdy),
    .done   (vsu_done)
  );


  initial
  begin
    vlu.m[0] = {65'd1};
    vlu.m[1] = {65'd2};
    vlu.m[2] = {65'd3};
    vlu.m[3] = {65'd4};
    vlu.m[4] = {65'd5};
    vlu.m[5] = {65'd6};
    vlu.m[6] = {65'd7};
    vlu.m[7] = {65'd8};
    vlu.m[8] = {65'd9};
    vlu.m[9] = {65'd10};
    vlu.m[10] = {65'd11};
    vlu.m[11] = {65'd12};
    vlu.m[12] = {65'd13};
    vlu.m[13] = {65'd14};
    vlu.m[14] = {65'd15};
    vlu.m[15] = {65'd16};
    vlu.m[16] = {65'd17};
    vlu.m[17] = {65'd18};
    vlu.m[18] = {65'd19};
    vlu.m[19] = {65'd20};
    vlu.m[20] = {65'd21};
    vlu.m[21] = {65'd22};
    vlu.m[22] = {65'd23};
    vlu.m[23] = {65'd24};

    fire.m[0] = {9'b01_000_0000};
    fire.m[1] = {9'd0};
    fire.m[2] = {9'd0};
    fire.m[3] = {9'd0};
    fire.m[4] = {9'd0};
    fire.m[5] = {9'd0};
    fire.m[6] = {9'd0};
    fire.m[7] = {9'd0};
    fire.m[8] = {9'b01_000_0000};
    fire.m[9] = {9'd0};
    fire.m[10] = {9'd0};
    fire.m[11] = {9'd0};
    fire.m[12] = {9'd0};
    fire.m[13] = {9'd0};
    fire.m[14] = {9'd0};
    fire.m[15] = {9'd0};
    fire.m[16] = {9'b00_001_0000};
    //fire.m[17] = {9'b00_000_0100};
    fire.m[17] = {9'd0};
    fire.m[18] = {9'd0};

    cmd.m[0] = {8'd0, 3'd0, 3'd0, 3'd0, 14'd3, 6'd0, 6'd0, 6'd0, 6'd1, 6'd63, 65'd0};
    cmd.m[1] = {126'd0};
    cmd.m[2] = {126'd0};
    cmd.m[3] = {126'd0};
    cmd.m[4] = {126'd0};
    cmd.m[5] = {126'd0};
    cmd.m[6] = {126'd0};
    cmd.m[7] = {126'd0};
    cmd.m[8] = {8'd0, 3'd0, 3'd0, 3'd0, 14'd3, 6'd0, 6'd0, 6'd0, 6'd2, 6'd63, 65'd0};
    cmd.m[9] = {126'd0};
    cmd.m[10] = {126'd0};
    cmd.m[11] = {126'd0};
    cmd.m[12] = {126'd0};
    cmd.m[13] = {126'd0};
    cmd.m[14] = {126'd0};
    cmd.m[15] = {126'd0};
    cmd.m[16] = {8'd0, 3'd0, 3'd0, 3'd0, 14'd3, 6'd1, 6'd2, 6'd0, 6'd3, 6'd63, 65'd0};
    //cmd.m[17] = {8'd0, 3'd0, `VAU1_FMA_D, 3'd0, 14'd3, 6'd1, 6'd2, 6'd3, 6'd3, 6'd63, 65'd0};
    cmd.m[17] = {126'd0};
    cmd.m[18] = {126'd0};
  end

  initial $vcdplusmemon();

  `VC_TEST_CASE_BEGIN( 0, "vxu" )
  begin
    #1; reset = 1'b1;
    #50; reset = 1'b0;
    #2000; `VC_TEST_CHECK( "Is sink finished?",
      fire_done & cmd_done & vlu_done & vsu_done );
  end
  `VC_TEST_CASE_END

  `VC_TEST_SUITE_END( 1 )

endmodule
