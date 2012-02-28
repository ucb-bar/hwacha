package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import scala.math._

class io_vru extends Bundle
{
  val vpfvaq = new io_ready_valid()({ new io_vvaq_bundle() })
  
  // command
  val vec_pfcmdq = new io_vec_cmdq().flip()
  // base
  val vec_pfximm1q = new io_vec_ximm1q().flip()
  // stride
  val vec_pfximm2q = new io_vec_ximm2q().flip()
}

class vuVRU extends Component
{
  val io = new io_vru()

  val VRU_Idle = Bits(0, 2)
  val VRU_IssueShort = Bits(1, 2)
  val VRU_IssueLong = Bits(2, 2)
  
  val n = Bool(false)
  val y = Bool(true)

  val cmd = io.vec_pfcmdq.bits(RG_XCMD_CMCODE)

  val cs = ListLookup(cmd, 
  //                stride     setvl
  //                     |         |
                    List(Bits(0,4),n), Array(
    CMD_VVCFGIVL -> List(Bits(0,4),y),
    CMD_VSETVL   -> List(Bits(0,4),y),

    CMD_VLD      -> List(Bits(8,4),n),
    CMD_VLW      -> List(Bits(4,4),n),
    CMD_VLWU     -> List(Bits(4,4),n),
    CMD_VLH      -> List(Bits(2,4),n),
    CMD_VLHU     -> List(Bits(2,4),n),
    CMD_VLB      -> List(Bits(1,4),n),
    CMD_VLBU     -> List(Bits(1,4),n),
    CMD_VSD      -> List(Bits(8,4),n),
    CMD_VSW      -> List(Bits(4,4),n),
    CMD_VSH      -> List(Bits(2,4),n),
    CMD_VSB      -> List(Bits(1,4),n),
    
    CMD_VFLD     -> List(Bits(8,4),n),
    CMD_VFLW     -> List(Bits(4,4),n),
    CMD_VFSD     -> List(Bits(8,4),n),
    CMD_VFSW     -> List(Bits(4,4),n),
    
    CMD_VLSTD    -> List(Bits(0,4),n),
    CMD_VLSTW    -> List(Bits(0,4),n),
    CMD_VLSTWU   -> List(Bits(0,4),n),
    CMD_VLSTH    -> List(Bits(0,4),n),
    CMD_VLSTHU   -> List(Bits(0,4),n),
    CMD_VLSTB    -> List(Bits(0,4),n),
    CMD_VLSTBU   -> List(Bits(0,4),n),
    CMD_VSSTD    -> List(Bits(0,4),n),
    CMD_VSSTW    -> List(Bits(0,4),n),
    CMD_VSSTH    -> List(Bits(0,4),n),
    CMD_VSSTB    -> List(Bits(0,4),n),
    
    CMD_VFLSTD   -> List(Bits(0,4),n),
    CMD_VFLSTW   -> List(Bits(0,4),n),
    CMD_VFSSTD   -> List(Bits(0,4),n),
    CMD_VFSSTW   -> List(Bits(0,4),n)))

  val stride_decoded::setvl::Nil = cs

  val state = Reg(resetVal = VRU_Idle)
  val addr_reg = Reg(resetVal = UFix(0, SZ_VIMM))
  val last_addr_reg = Reg(resetVal = UFix(0, SZ_VIMM))
  val stride_reg = Reg(resetVal = UFix(0, SZ_VIMM))
  val cmd_reg = Reg(resetVal = Bits(0,SZ_XCMD_CMD))
  val vlen_reg = Reg(resetVal = UFix(0, SZ_VLEN))
  
  val unit_stride = (stride_decoded != Bits(0,4))
  
  val addr = io.vec_pfximm1q.bits.toUFix
  val stride = Mux(unit_stride, stride_decoded.toUFix, io.vec_pfximm2q.bits.toUFix)

  val mask_vec_pfximm1q_valid = io.vec_pfximm1q.valid
  val mask_vec_pfximm2q_valid = setvl || unit_stride || io.vec_pfximm2q.valid

  val deq_vec_pfximm1q = Bool(true)
  val deq_vec_pfximm2q = !unit_stride && !setvl

  val cmd_val = io.vec_pfcmdq.valid && mask_vec_pfximm1q_valid && mask_vec_pfximm2q_valid
  val setvl_val = io.vec_pfcmdq.valid && mask_vec_pfximm1q_valid && setvl

  // cmd(4)==1 -> vector store
  io.vpfvaq.bits.cmd := Mux(cmd_reg(4), M_PFW, M_PFR)
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
      when (setvl_val)
      {
        vlen_reg := io.vec_pfcmdq.bits(RG_XCMD_VLEN).toUFix
      }
      .elsewhen (cmd_val)
      {
        addr_reg := addr
        cmd_reg := cmd
        last_addr_reg := addr + (vlen_reg * stride)
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
