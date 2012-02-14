package hwacha
{
  import Chisel._
  import Node._
  import Config._
  import Commands._
  import Instructions._
  import Interface._

  class vuVCU extends Component
  {
    val io = new io_vcu()

    val cmd = io.vec_cmdq.bits(VCMD_CMCODE)

    val y = Bool(true)
    val n = Bool(false)

    val FWD  = Bits(0,2)
    val LDWB = Bits(1,2)
    val STAC = Bits(2,2)

    val cs = 
      ListLookup(cmd,
                           // decode_fence_cv
                           // |  decode_fence_v
                           // |  |  decode_setvl
                           // |  |  |  deq_vec_ximm1q
                           // |  |  |  |  deq_vec_ximm2q
                           // |  |  |  |  |  enq_vxu_cmdq
                           // |  |  |  |  |  |  enq_vxu_immq
                           // |  |  |  |  |  |  |  enq_vxu_imm2q
                           // |  |  |  |  |  |  |  |  enq_vmu_vbaseq
                           // |  |  |  |  |  |  |  |  |  enq_vmu_vstrideq
                           // |  |  |  |  |  |  |  |  |  |  sel_vxu_cmdq
                           // |  |  |  |  |  |  |  |  |  |  |
                         List(n, n, n, n, n, y, n, n, n, n, FWD), Array(
        CMD_VVCFGIVL  -> List(n, n, y, y, n, y, y, n, n, n, FWD),
        CMD_VSETVL    -> List(n, n, y, y, n, y, y, n, n, n, FWD),
        CMD_VF        -> List(n, n, n, y, n, y, y, n, n, n, FWD),

        CMD_FENCE_L_V -> List(n, y, n, n, n, y, n, n, n, n, FWD),
        CMD_FENCE_G_V -> List(n, y, n, n, n, y, n, n, n, n, FWD),
        CMD_FENCE_L_CV-> List(y, n, n, n, n, y, n, n, n, n, FWD),
        CMD_FENCE_G_CV-> List(y, n, n, n, n, y, n, n, n, n, FWD),

        CMD_VMVV      -> List(n, n, n, n, n, y, n, n, n, n, FWD),
        CMD_VMSV      -> List(n, n, n, y, n, y, y, n, n, n, FWD),
        CMD_VFMVV     -> List(n, n, n, n, n, y, n, n, n, n, FWD),

        CMD_VLD       -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLW       -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLWU      -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLH       -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLHU      -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLB       -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VLBU      -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VSD       -> List(n, n, n, y, n, y, y, n, y, n, STAC),
        CMD_VSW       -> List(n, n, n, y, n, y, y, n, y, n, STAC),
        CMD_VSH       -> List(n, n, n, y, n, y, y, n, y, n, STAC),
        CMD_VSB       -> List(n, n, n, y, n, y, y, n, y, n, STAC),

        CMD_VFLD      -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VFLW      -> List(n, n, n, y, n, y, y, n, y, n, LDWB),
        CMD_VFSD      -> List(n, n, n, y, n, y, y, n, y, n, STAC),
        CMD_VFSW      -> List(n, n, n, y, n, y, y, n, y, n, STAC),

        CMD_VLSTD     -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTW     -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTWU    -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTH     -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTHU    -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTB     -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VLSTBU    -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VSSTD     -> List(n, n, n, y, y, y, y, y, y, y, STAC),
        CMD_VSSTW     -> List(n, n, n, y, y, y, y, y, y, y, STAC),
        CMD_VSSTH     -> List(n, n, n, y, y, y, y, y, y, y, STAC),
        CMD_VSSTB     -> List(n, n, n, y, y, y, y, y, y, y, STAC),

        CMD_VFLSTD    -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VFLSTW    -> List(n, n, n, y, y, y, y, y, y, y, LDWB),
        CMD_VFSSTD    -> List(n, n, n, y, y, y, y, y, y, y, STAC),
        CMD_VFSSTW    -> List(n, n, n, y, y, y, y, y, y, y, STAC)
      ))

    val decode_fence_cv::decode_fence_v::decode_setvl::deq_vec_ximm1q::deq_vec_ximm2q::enq_vxu_cmdq::enq_vxu_immq::enq_vxu_imm2q::enq_vmu_vbaseq::enq_vmu_vstrideq::sel_vxu_cmdq::Nil = cs

    val mask_vec_ximm1q_val = !deq_vec_ximm1q || io.vec_ximm1q.valid
    val mask_vec_ximm2q_val = !deq_vec_ximm2q || io.vec_ximm2q.valid
    val mask_vxu_cmdq_rdy = !enq_vxu_cmdq || io.vxu_cmdq.ready
    val mask_vxu_immq_rdy = !enq_vxu_immq || io.vxu_immq.ready
    val mask_vxu_imm2q_rdy = !enq_vxu_imm2q || io.vxu_imm2q.ready


    val fire_fence_cv =
      decode_fence_cv &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    val fire_fence_v =
      decode_fence_v &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    val fire_setvl =
      decode_setvl &&
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    //----------------------------------------------------------------\\
    // REGISTERS                                                      \\
    //----------------------------------------------------------------\\

    val vlen = Reg(resetVal = Bits(0, VLENMAX_SZ))

    when (fire_setvl)
    {
      vlen := io.vec_ximm1q.bits(VLENMAX_SZ-1,0)
    }

    //-------------------------------------------------------------------------\\
    // SIGNALS                                                                 \\
    //-------------------------------------------------------------------------\\

    io.vec_cmdq.ready :=
    Bool(true) && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    io.vec_ximm1q.ready :=
    io.vec_cmdq.valid && deq_vec_ximm1q && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    io.vec_ximm2q.ready :=
    io.vec_cmdq.valid && mask_vec_ximm1q_val && deq_vec_ximm2q &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    io.vxu_cmdq.valid :=
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    enq_vxu_cmdq && mask_vxu_immq_rdy && mask_vxu_imm2q_rdy

    io.vxu_immq.valid :=
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && enq_vxu_immq && mask_vxu_imm2q_rdy

    io.vxu_imm2q.valid := 
    io.vec_cmdq.valid && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy && enq_vxu_imm2q

    // io.vxu_cmdq.bits := MuxCase(
    //   io.vec_cmdq.bits, Array(
    //     (sel_vxu_cmdq === LDWB) -> Cat(CMD_LDWB, io.vec_cmdq.bits(11,0)),
    //     (sel_vxu_cmdq === STAC) -> Cat(CMD_STAC, io.vec_cmdq.bits(11,0))))

    // new 
    io.vxu_cmdq.bits := io.vec_cmdq.bits
    io.vxu_immq.bits := io.vec_ximm1q.bits
    io.vxu_imm2q.bits := io.vec_ximm2q.bits
    // new

  }
}
