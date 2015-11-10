package hwacha

import Chisel._
import cde.Parameters

class VRU(implicit p: Parameters) extends Module {
  import Commands._

  val io = new Bundle {
    val toicache = new FrontendIO // checked, matches vxu, icache has .flip
    val fromscalar_active = Bool(INPUT) 
    val fromscalar_req = Valid(new FrontendReq).flip
    val cmdq = new CMDQIO().flip 
  }

  val delay = 4
  val vru_req = ShiftRegister(io.fromscalar_req,delay)
  io.toicache.req := vru_req
  io.toicache.active := ShiftRegister(io.fromscalar_active, delay)
  io.toicache.resp.ready := Bool(true)

  // addr regfile
  val arf = Mem(UInt(width = 64), 32)

  val decode_vmss    = io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa    = io.cmdq.cmd.bits === CMD_VMSA
  val decode_vsetcfg = io.cmdq.cmd.bits === CMD_VSETCFG
  val decode_vsetvl  = io.cmdq.cmd.bits === CMD_VSETVL
  val decode_vf      = io.cmdq.cmd.bits === CMD_VF
  val decode_vft     = io.cmdq.cmd.bits === CMD_VFT

  val deq_imm = decode_vmss || decode_vmsa || decode_vf || decode_vft || decode_vsetvl || decode_vsetcfg
  val deq_rd  = decode_vmss || decode_vmsa

  val mask_imm_valid = !deq_imm || io.cmdq.imm.valid
  val mask_rd_valid  = !deq_rd  || io.cmdq.rd.valid

  def fire_cmdq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      io.cmdq.cmd.valid,
      mask_imm_valid,
      mask_rd_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  
  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(io.cmdq.imm.valid, deq_imm)
  io.cmdq.rd.ready := fire_cmdq(io.cmdq.rd.valid, deq_rd)


  when (fire_cmdq(null, decode_vmsa)) {
    printf("VMSA:\n")
    printf("%b\n", io.cmdq.cmd.bits)
    printf("%b\n", io.cmdq.imm.bits)
    printf("%b\n", io.cmdq.rd.bits)
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
  }



}
