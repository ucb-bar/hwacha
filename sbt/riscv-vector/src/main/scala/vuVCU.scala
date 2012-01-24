package riscvVector
{
  import Chisel._
  import Node._
  import Config._
  import Commands._
  import Instructions._

  class vuVCU extends Component
  {
    val io = new io_vcu();

    val cmd = io.vec_cmdq.bits(VCMD_CMCODE);

    val y = Bool(true);
    val n = Bool(false);

    val FWD  = Bits(0,2);
    val LDWB = Bits(1,2);
    val STAC = Bits(2,2);

    val cs = 
      ListLookup(cmd,
                           // decode_fence_cv
                           // |  decode_fence_v
                           // |  |  decode_setvl
                           // |  |  |  deq_vec_ximm1q
                           // |  |  |  |  deq_vec_ximm2q
                           // |  |  |  |  |  enq_vxu_cmdq
                           // |  |  |  |  |  |  enq_vxu_immq
                           // |  |  |  |  |  |  |  enq_vmu_vcmdq
                           // |  |  |  |  |  |  |  |  enq_vmu_vbaseq
                           // |  |  |  |  |  |  |  |  |  enq_vmu_vstrideq
                           // |  |  |  |  |  |  |  |  |  |  sel_vxu_cmdq
                           // |  |  |  |  |  |  |  |  |  |  |
                         List(n, n, n, n, n, y, n, n, n, n, FWD), Array(
        CMD_VVCFGIVL  -> List(n, n, y, y, n, y, y, n, n, n, FWD),
        CMD_VSETVL    -> List(n, n, y, y, n, y, y, n, n, n, FWD),
        CMD_VF        -> List(n, n, n, y, n, y, y, n, n, n, FWD),

        CMD_FENCE_L_V -> List(n, y, n, n, n, y, n, y, n, n, FWD),
        CMD_FENCE_G_V -> List(n, y, n, n, n, y, n, y, n, n, FWD),
        CMD_FENCE_L_CV-> List(y, n, n, n, n, y, n, y, n, n, FWD),
        CMD_FENCE_G_CV-> List(y, n, n, n, n, y, n, y, n, n, FWD),

        CMD_VMVV      -> List(n, n, n, n, n, y, n, n, n, n, FWD),
        CMD_VMSV      -> List(n, n, n, y, n, y, y, n, n, n, FWD),
        CMD_VFMVV     -> List(n, n, n, n, n, y, n, n, n, n, FWD),

        CMD_VLD       -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLW       -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLWU      -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLH       -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLHU      -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLB       -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VLBU      -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VSD       -> List(n, n, n, y, n, y, n, y, y, n, STAC),
        CMD_VSW       -> List(n, n, n, y, n, y, n, y, y, n, STAC),
        CMD_VSH       -> List(n, n, n, y, n, y, n, y, y, n, STAC),
        CMD_VSB       -> List(n, n, n, y, n, y, n, y, y, n, STAC),

        CMD_VFLD      -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VFLW      -> List(n, n, n, y, n, y, n, y, y, n, LDWB),
        CMD_VFSD      -> List(n, n, n, y, n, y, n, y, y, n, STAC),
        CMD_VFSW      -> List(n, n, n, y, n, y, n, y, y, n, STAC),

        CMD_VLSTD     -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTW     -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTWU    -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTH     -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTHU    -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTB     -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VLSTBU    -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VSSTD     -> List(n, n, n, y, y, y, n, y, y, y, STAC),
        CMD_VSSTW     -> List(n, n, n, y, y, y, n, y, y, y, STAC),
        CMD_VSSTH     -> List(n, n, n, y, y, y, n, y, y, y, STAC),
        CMD_VSSTB     -> List(n, n, n, y, y, y, n, y, y, y, STAC),

        CMD_VFLSTD    -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VFLSTW    -> List(n, n, n, y, y, y, n, y, y, y, LDWB),
        CMD_VFSSTD    -> List(n, n, n, y, y, y, n, y, y, y, STAC),
        CMD_VFSSTW    -> List(n, n, n, y, y, y, n, y, y, y, STAC)
      ));

    val decode_fence_cv::decode_fence_v::decode_setvl::deq_vec_ximm1q::deq_vec_ximm2q::enq_vxu_cmdq::enq_vxu_immq::enq_vmu_vcmdq::enq_vmu_vbaseq::enq_vmu_vstrideq::sel_vxu_cmdq::Nil = cs;

    val mask_vec_ximm1q_val = !deq_vec_ximm1q || io.vec_ximm1q.valid;
    val mask_vec_ximm2q_val = !deq_vec_ximm2q || io.vec_ximm2q.valid;
    val mask_vxu_cmdq_rdy = !enq_vxu_cmdq || io.vxu_cmdq.ready;
    val mask_vxu_immq_rdy = !enq_vxu_immq || io.vxu_immq.ready;
    val mask_vmu_vcmdq_rdy = !enq_vmu_vcmdq || io.vmu_vcmdq.ready;
    val mask_vmu_vbaseq_rdy = !enq_vmu_vbaseq || io.vmu_vbaseq.ready;
    val mask_vmu_vstrideq_rdy = !enq_vmu_vstrideq || io.vmu_vstrideq.ready;

    val fire_fence_cv =
      decode_fence_cv &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    val fire_fence_v =
      decode_fence_v &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    val fire_setvl =
      decode_setvl &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    /////////////////////////////////////////////////////////////////////////////
    // REGISTERS
    /////////////////////////////////////////////////////////////////////////////

    val VCU_FORWARD = Bits(0,2);
    val VCU_FENCE_CV = Bits(1,2);
    val VCU_FENCE_V = Bits(2,2);

    val state = Reg(resetVal = VCU_FORWARD);

    switch (state)
    {
      is (VCU_FORWARD)
      {
        when (fire_fence_cv) {
          state <== VCU_FENCE_CV;
        }
        when (fire_fence_v) {
          state <== VCU_FENCE_V;
        }
      }
      is (VCU_FENCE_CV)
      {
        when (io.vmu_vackq.valid && io.vxu_ackq.valid && io.vec_ackq.ready) {
          state <== VCU_FORWARD;
        }
      }
      is (VCU_FENCE_V)
      {
        when (io.vmu_vackq.valid && io.vxu_ackq.valid) {
          state <== VCU_FORWARD;
        }
      }
    }

    val vlen = Reg(resetVal = Bits(0, VLENMAX_SZ));

    when (fire_setvl)
    {
      vlen <== io.vec_ximm1q.bits(VLENMAX_SZ-1,0);
    }

    /////////////////////////////////////////////////////////////////////////////
    // SIGNALS
    /////////////////////////////////////////////////////////////////////////////

    val forward = (state === VCU_FORWARD);

    io.vec_cmdq.ready :=
    forward &&
    Bool(true) && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vec_ximm1q.ready :=
    forward &&
    io.vec_cmdq.valid && deq_vec_ximm1q && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vec_ximm2q.ready :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && deq_vec_ximm2q &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vxu_cmdq.valid :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    enq_vxu_cmdq && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vxu_immq.valid :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && enq_vxu_immq &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vmu_vcmdq.valid :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    enq_vmu_vcmdq && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

    io.vmu_vbaseq.valid :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && enq_vmu_vbaseq && mask_vmu_vstrideq_rdy;

    io.vmu_vstrideq.valid :=
    forward &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && enq_vmu_vstrideq;

    io.vxu_cmdq.bits := MuxCase(
      io.vec_cmdq.bits, Array(
        (sel_vxu_cmdq === LDWB) -> Cat(CMD_LDWB, io.vec_cmdq.bits(11,0)),
        (sel_vxu_cmdq === STAC) -> Cat(CMD_STAC, io.vec_cmdq.bits(11,0))));

      val vxu_immq_bits = io.vec_ximm1q.bits;
      val vmu_vcmdq_bits = Cat(cmd, vlen);
      val vmu_vbaseq_bits = io.vec_ximm1q.bits(31,0);
      val vmu_vstrideq_bits = io.vec_ximm2q.bits;

      switch(state)
      {
        is(VCU_FORWARD)
        {
        }
        is(VCU_FENCE_CV)
        {
          when(io.vxu_ackq.valid && io.vmu_vackq.valid)
          {
            io.vec_ackq.bits <== Bits(1,32);
            io.vec_ackq.valid <== Bool(true);
          }
          io.vxu_ackq.ready <== io.vmu_vackq.valid;
          io.vmu_vackq.ready <== io.vxu_ackq.valid;
        }
        is(VCU_FENCE_V)
        {
          io.vxu_ackq.ready <== io.vmu_vackq.valid;
          io.vmu_vackq.ready <== io.vxu_ackq.valid;
        }
        otherwise
        {
          io.vec_ackq.bits <== Bits(0,32);
          io.vec_ackq.valid <== Bool(false);
          io.vxu_ackq.ready <== Bool(false);
          io.vmu_vackq.ready <== Bool(false);
        }
      }


    }
  }
