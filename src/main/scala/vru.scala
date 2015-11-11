package hwacha

import Chisel._
import cde.Parameters

class VRU(implicit p: Parameters) extends Module {
  import Commands._

  val io = new Bundle {
    val toicache = new FrontendIO // checked, matches vxu, icache has .flip
    val cmdq = new CMDQIO().flip 
  }

  // addr regfile
  val arf = Mem(UInt(width = 64), 32)

  val vf_active = Reg(init=Bool(false)) 

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

  val vlen = Reg(init=UInt(0))

  def fire_cmdq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !vf_active,
      io.cmdq.cmd.valid,
      mask_imm_valid,
      mask_rd_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  
  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(io.cmdq.imm.valid, deq_imm)
  io.cmdq.rd.ready := fire_cmdq(io.cmdq.rd.valid, deq_rd)

  // should never get a vmss
  assert(!fire_cmdq(null, decode_vmss), "VRU should not receive VMSS")

  // handle vmsa
  when (fire_cmdq(null, decode_vmsa)) {
    printf("VMSA:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
    printf("RD:  0x%x\n", io.cmdq.rd.bits)
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
  }

  // handle vsetcfg
  when (fire_cmdq(null, decode_vsetcfg)) {
    printf("VSETCFG:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
  }

  // handle vsetvl
  when (fire_cmdq(null, decode_vsetvl)) {
    printf("VSETVL:\n")
    printf("Setting VL = 0x%x\n", io.cmdq.imm.bits)
    vlen := io.cmdq.imm.bits
  }

  // handle vf

  val fire_vf = fire_cmdq(null, decode_vf)

  when (fire_vf) {
    printf("VF:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
    vf_active := Bool(true)
  }

  // do a fetch
  io.toicache.req.valid := fire_vf
  io.toicache.req.bits.pc := io.cmdq.imm.bits
  io.toicache.active := vf_active 
  io.toicache.invalidate := Bool(false)
  io.toicache.resp.ready := Bool(true) // for now...

  when (io.toicache.resp.valid && vf_active) {
    // hacky match against unit-strided load/store insts
    // Could probably do this right with IntCtrlSigs
    val loaded_inst = io.toicache.resp.bits.data(0)
    printf("INST PC  recv'd: 0x%x\n", io.toicache.resp.bits.pc)
    printf("INST VAL recv'd: 0x%x\n", loaded_inst)

    when (loaded_inst === HwachaElementInstructions.VSTOP) {
      vf_active := Bool(false)
    }

    when (loaded_inst === HwachaElementInstructions.VLD || 
          loaded_inst === HwachaElementInstructions.VSD) {
      printf("GOT VLD/VSD.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d double elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
    }

    when (loaded_inst === HwachaElementInstructions.VLW || 
          loaded_inst === HwachaElementInstructions.VSW || 
          loaded_inst === HwachaElementInstructions.VLWU) {
      printf("GOT VLW/VSW/VLWU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d single elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
    }

    when (loaded_inst === HwachaElementInstructions.VLH || 
          loaded_inst === HwachaElementInstructions.VSH || 
          loaded_inst === HwachaElementInstructions.VLHU) {
      printf("GOT VLH/VSH/VLHU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d half elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
    }

    when (loaded_inst === HwachaElementInstructions.VLB || 
          loaded_inst === HwachaElementInstructions.VSB || 
          loaded_inst === HwachaElementInstructions.VLBU) {
      printf("GOT VLB/VSB/VLBU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d byte elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
    }

  }

}
