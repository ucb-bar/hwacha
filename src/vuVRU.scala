package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import scala.math._

class vuVRU extends Component
{
  val io = new io_vru()

  val VRU_Idle = Bits(0, 2)
  val VRU_IssueShort = Bits(1, 2)
  val VRU_IssueLong = Bits(2, 2)

  val cmd = io.vec_pfcmdq.bits(RG_XCMD_CMCODE)
  val vlen = io.vec_pfcmdq.bits(RG_XCMD_VLEN).toUFix

  val stride_decoded = MuxLookup(cmd, Bits(0,4), Array(
    CMD_VLD    -> Bits(8,4),
    CMD_VLW    -> Bits(4,4),
    CMD_VLWU   -> Bits(4,4),
    CMD_VLH    -> Bits(2,4),
    CMD_VLHU   -> Bits(2,4),
    CMD_VLB    -> Bits(1,4),
    CMD_VLBU   -> Bits(1,4),
    CMD_VSD    -> Bits(8,4),
    CMD_VSW    -> Bits(4,4),
    CMD_VSH    -> Bits(2,4),
    CMD_VSB    -> Bits(1,4),

    CMD_VFLD   -> Bits(8,4),
    CMD_VFLW   -> Bits(4,4),
    CMD_VFSD   -> Bits(8,4),
    CMD_VFSW   -> Bits(4,4),

    CMD_VLSTD  -> Bits(0,4),
    CMD_VLSTW  -> Bits(0,4),
    CMD_VLSTWU -> Bits(0,4),
    CMD_VLSTH  -> Bits(0,4),
    CMD_VLSTHU -> Bits(0,4),
    CMD_VLSTB  -> Bits(0,4),
    CMD_VLSTBU -> Bits(0,4),
    CMD_VSSTD  -> Bits(0,4),
    CMD_VSSTW  -> Bits(0,4),
    CMD_VSSTH  -> Bits(0,4),
    CMD_VSSTB  -> Bits(0,4),

    CMD_VFLSTD -> Bits(0,4),
    CMD_VFLSTW -> Bits(0,4),
    CMD_VFSSTD -> Bits(0,4),
    CMD_VFSSTW -> Bits(0,4)))

  val state = Reg(resetVal = VRU_Idle)
  val addr_reg = Reg(resetVal = UFix(0, SZ_VIMM))
  val last_addr_reg = Reg(resetVal = UFix(0, SZ_VIMM))
  val stride_reg = Reg(resetVal = UFix(0, SZ_VIMM))

  val unit_stride = (stride_decoded != Bits(0,4))
  
  val addr = io.vec_pfximm1q.bits.toUFix
  val stride = Mux(unit_stride, stride_decoded.toUFix, io.vec_pfximm2q.bits.toUFix)

  val mask_vec_pfximm1q_valid = io.vec_pfximm1q.valid
  val mask_vec_pfximm2q_valid = unit_stride || io.vec_pfximm2q.valid

  val deq_vec_pfximm1q = Bool(true)
  val deq_vec_pfximm2q = !unit_stride

  val cmd_val = io.vec_pfcmdq.valid && mask_vec_pfximm1q_valid && mask_vec_pfximm2q_valid

  // cmd(4)==1 -> vector store
  io.vpfvaq.bits.cmd := Mux(cmd(4), M_PFW, M_PFR)
  io.vpfvaq.bits.typ := Bits(0)
  io.vpfvaq.bits.typ_float := Bits(0)
  io.vpfvaq.bits.idx := addr_reg(PGIDX_BITS-1,0)
  io.vpfvaq.bits.vpn := addr_reg(VADDR_BITS, PGIDX_BITS)

  io.vpfvaq.valid := Bool(false)
  io.vec_pfcmdq.ready := Bool(true) && mask_vec_pfximm1q_valid && mask_vec_pfximm2q_valid
  io.vec_pfximm1q.ready := io.vec_pfcmdq.valid && deq_vec_pfximm1q && mask_vec_pfximm2q_valid
  io.vec_pfximm2q.ready := io.vec_pfcmdq.valid && mask_vec_pfximm1q_valid && deq_vec_pfximm2q

  switch (state)
  {
    is (VRU_Idle)
    {
      when (cmd_val)
      {
        addr_reg := addr
        last_addr_reg := addr + (vlen * stride)
        stride_reg := stride

        when (stride < UFix(pow(2,OFFSET_BITS).toInt)) { state := VRU_IssueShort }
        .otherwise { state := VRU_IssueLong }
      }
    }
    is (VRU_IssueShort)
    {
      when (addr_reg > last_addr_reg)
      {
        state := VRU_Idle
      }
      .otherwise
      {
        io.vpfvaq.valid := Bool(true)
        when (io.vpfvaq.ready)
        {
          addr_reg := addr_reg + UFix(pow(2,OFFSET_BITS).toInt)
        }
      }
    }
    is (VRU_IssueLong)
    {
      when (addr_reg > last_addr_reg)
      {
        state := VRU_Idle
      }
      .otherwise
      {
        io.vpfvaq.valid := Bool(true)
        when (io.vpfvaq.ready)
        {
          addr_reg := addr_reg + stride_reg
        }
      }
    }
  }
}
