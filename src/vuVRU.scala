package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import scala.math._

class io_vru extends Bundle
{
  val vpfvaq = new io_vvaq()
  
  // command
  val vpfcmdq = new io_vcmdq().flip
  // base
  val vpfximm1q = new io_vximm1q().flip
  // stride
  val vpfximm2q = new io_vximm2q().flip
  val vpfcntq = new io_vcntq().flip
}

class vuVRU(resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val io = new io_vru()

  val VRU_Idle = Bits(0, 2)
  val VRU_IssueShort = Bits(1, 2)
  val VRU_IssueLong = Bits(2, 2)
  
  val n = Bool(false)
  val y = Bool(true)

  val cmd = io.vpfcmdq.bits(RG_XCMD_CMCODE)

  val cs = ListLookup(cmd,
  //                               deq_imm2q
  //                             deq_imm1q |
  //                                  pf | |
  //                  stride     setvl | | |
  //                       |         | | | |
                      List(Bits(0,4),n,n,n,n), Array(
    CMD_VVCFGIVL   -> List(Bits(0,4),y,y,y,n),
    CMD_VSETVL     -> List(Bits(0,4),y,y,y,n),

    CMD_VLD        -> List(Bits(8,4),n,y,y,n),
    CMD_VLW        -> List(Bits(4,4),n,y,y,n),
    CMD_VLWU       -> List(Bits(4,4),n,y,y,n),
    CMD_VLH        -> List(Bits(2,4),n,y,y,n),
    CMD_VLHU       -> List(Bits(2,4),n,y,y,n),
    CMD_VLB        -> List(Bits(1,4),n,y,y,n),
    CMD_VLBU       -> List(Bits(1,4),n,y,y,n),
    CMD_VSD        -> List(Bits(8,4),n,y,y,n),
    CMD_VSW        -> List(Bits(4,4),n,y,y,n),
    CMD_VSH        -> List(Bits(2,4),n,y,y,n),
    CMD_VSB        -> List(Bits(1,4),n,y,y,n),
    
    CMD_VFLD       -> List(Bits(8,4),n,y,y,n),
    CMD_VFLW       -> List(Bits(4,4),n,y,y,n),
    CMD_VFSD       -> List(Bits(8,4),n,y,y,n),
    CMD_VFSW       -> List(Bits(4,4),n,y,y,n),
    
    CMD_VLSTD      -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTW      -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTWU     -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTH      -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTHU     -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTB      -> List(Bits(0,4),n,y,y,y),
    CMD_VLSTBU     -> List(Bits(0,4),n,y,y,y),
    CMD_VSSTD      -> List(Bits(0,4),n,y,y,y),
    CMD_VSSTW      -> List(Bits(0,4),n,y,y,y),
    CMD_VSSTH      -> List(Bits(0,4),n,y,y,y),
    CMD_VSSTB      -> List(Bits(0,4),n,y,y,y),
    
    CMD_VFLSTD     -> List(Bits(0,4),n,y,y,y),
    CMD_VFLSTW     -> List(Bits(0,4),n,y,y,y),
    CMD_VFSSTD     -> List(Bits(0,4),n,y,y,y),
    CMD_VFSSTW     -> List(Bits(0,4),n,y,y,y),

    // instructions not relevant for prefetch (just dequeue them)
    CMD_VF         -> List(Bits(1,4),n,n,y,n),
    CMD_FENCE_L_V  -> List(Bits(1,4),n,n,n,n),
    CMD_FENCE_G_V  -> List(Bits(1,4),n,n,n,n),
    CMD_VMVV       -> List(Bits(1,4),n,n,n,n),
    CMD_VMSV       -> List(Bits(1,4),n,n,y,n),
    CMD_VFMVV      -> List(Bits(1,4),n,n,n,n)
  ))

  val stride_decoded::setvl::pf::deq_imm1q::deq_imm2q::Nil = cs

  val state = RegReset(VRU_Idle)

  val vlen_reg = RegReset(SInt(0, SZ_VLEN+1))

  val addr_reg = RegReset(UInt(0, SZ_VIMM))
  val cmd_reg = RegReset(Bits(0,SZ_XCMD_CMD))
  val vec_count_reg = RegReset(SInt(0, SZ_VLEN+1))
  val vec_pfed_count_reg = RegReset(SInt(0, SZ_VLEN+1))
  val stride_reg = RegReset(UInt(0, SZ_VIMM))
  
  val vec_per_pf_reg = RegReset(UInt(0, OFFSET_BITS+1))
  val vec_pf_remainder_reg = RegReset(UInt(0, OFFSET_BITS))
  val stride_remaining_reg = RegReset(UInt(0, OFFSET_BITS))

  val unit_stride = (stride_decoded != Bits(0,4))
  
  val addr = io.vpfximm1q.bits.toUInt
  val stride = Mux(unit_stride, stride_decoded.toUInt, io.vpfximm2q.bits.toUInt)

  val mask_vpfximm1q_valid = io.vpfximm1q.valid || !deq_imm1q
  val mask_vpfximm2q_valid = io.vpfximm2q.valid || !deq_imm2q
  val mask_vpfcntq_valid = Bool(true)

  val cmd_val = io.vpfcmdq.valid && mask_vpfximm1q_valid && mask_vpfximm2q_valid && pf.toBool
  val setvl_val = io.vpfcmdq.valid && mask_vpfximm1q_valid && setvl.toBool && pf.toBool

  val pf_len = UInt(pow(2,OFFSET_BITS).toInt)
  val pf_len_int = pow(2,OFFSET_BITS).toInt

  // cmd(4)==1 -> vector store
  io.vpfvaq.bits.cmd := Mux(cmd_reg(4), mcmd_PFW, mcmd_PFR)
  io.vpfvaq.bits.typ := Bits(0)
  io.vpfvaq.bits.typ_float := Bool(false)
  io.vpfvaq.bits.idx := addr_reg(PGIDX_BITS-1,0)
  io.vpfvaq.bits.vpn := addr_reg(VADDR_BITS, PGIDX_BITS)

  val idle = (state === VRU_Idle)

  io.vpfvaq.valid := Bool(false)
  io.vpfcmdq.ready := Bool(true) && mask_vpfximm1q_valid && mask_vpfximm2q_valid && mask_vpfcntq_valid && idle
  io.vpfximm1q.ready := io.vpfcmdq.valid && deq_imm1q.toBool && mask_vpfximm2q_valid && mask_vpfcntq_valid && idle
  io.vpfximm2q.ready := io.vpfcmdq.valid && mask_vpfximm1q_valid && deq_imm2q.toBool && mask_vpfcntq_valid && idle
  io.vpfcntq.ready := io.vpfcmdq.valid && mask_vpfximm1q_valid && mask_vpfximm2q_valid && Bool(true) && idle

  switch (state)
  {
    is (VRU_Idle)
    {
      when (setvl_val)
      {
        vlen_reg := io.vpfximm1q.bits(RG_XIMM1_VLEN).toUInt
      }
      .elsewhen (cmd_val)
      {
        addr_reg := addr
        cmd_reg := cmd
        stride_reg := stride

        when (stride < pf_len)
        {
          state := VRU_IssueShort

          // vec_per_pf_reg = 2**OFFSET_BITS / stride(OFFSET_BITS-1, 0)
          // Assume OFFSET_BITS >= 4, reasonable since you wouldn't
          // want a cache line with less than two double words
          val stride_lobits = stride(OFFSET_BITS,0)
          var div = MuxLookup(stride(4,0),
                        UInt(0,OFFSET_BITS+1), Array(
            Bits(1)  -> UInt(pf_len_int/1,OFFSET_BITS+1),
            Bits(2)  -> UInt(pf_len_int/2,OFFSET_BITS+1),
            Bits(3)  -> UInt(pf_len_int/3,OFFSET_BITS+1),
            Bits(4)  -> UInt(pf_len_int/4,OFFSET_BITS+1),
            Bits(5)  -> UInt(pf_len_int/5,OFFSET_BITS+1),
            Bits(6)  -> UInt(pf_len_int/6,OFFSET_BITS+1),
            Bits(7)  -> UInt(pf_len_int/7,OFFSET_BITS+1),
            Bits(8)  -> UInt(pf_len_int/8,OFFSET_BITS+1),
            Bits(9)  -> UInt(pf_len_int/9,OFFSET_BITS+1),
            Bits(10) -> UInt(pf_len_int/10,OFFSET_BITS+1),
            Bits(11) -> UInt(pf_len_int/11,OFFSET_BITS+1),
            Bits(12) -> UInt(pf_len_int/12,OFFSET_BITS+1),
            Bits(13) -> UInt(pf_len_int/13,OFFSET_BITS+1),
            Bits(14) -> UInt(pf_len_int/14,OFFSET_BITS+1),
            Bits(15) -> UInt(pf_len_int/15,OFFSET_BITS+1),
            Bits(16) -> UInt(pf_len_int/16,OFFSET_BITS+1)
          ))

          for (i <- (pf_len_int/16)-1 to 1 by -1)
          {
            val lo = UInt(pf_len_int/(i+1)+1)
            val high = UInt(pf_len_int/i)
            div = Mux(stride_lobits <= high && stride_lobits >= lo, UInt(i), div)
          }

          vec_pfed_count_reg := Mux(!io.vpfcntq.valid, SInt(0), io.vpfcntq.bits.toUInt).toUInt
          vec_count_reg := Mux(stride_lobits === UInt(0), SInt(1), vlen_reg)
          vec_per_pf_reg := Mux(stride_lobits === UInt(0), UInt(1), div)
          vec_pf_remainder_reg := pf_len - (div * stride_lobits)
          stride_remaining_reg := stride
        }
        .otherwise
        {
          state := VRU_IssueLong
          vec_count_reg := vlen_reg
        }
      }
    }
    is (VRU_IssueShort)
    {
      // can leave regs intact, next prefetch command
      // will set them as needed
      when (vec_count_reg <= SInt(0))
      {
        state := VRU_Idle
      }
      .otherwise
      {
        when (vec_count_reg <= vlen_reg - vec_pfed_count_reg)
        {
          io.vpfvaq.valid := Bool(true)
        }
        when (io.vpfvaq.ready)
        {
          vec_count_reg := vec_count_reg - vec_per_pf_reg - Mux(stride_remaining_reg <= vec_pf_remainder_reg, SInt(1), SInt(0))
          stride_remaining_reg := Mux(stride_remaining_reg <= vec_pf_remainder_reg,
                                      stride_reg + stride_remaining_reg - vec_pf_remainder_reg,
                                      stride_remaining_reg - vec_pf_remainder_reg)
          addr_reg := addr_reg + pf_len
        }
      }
    }
    is (VRU_IssueLong)
    {
      // can leave regs intact, next prefetch command
      // will set them as needed
      when (vec_count_reg === SInt(0))
      {
        state := VRU_Idle
      }
      .otherwise
      {
        when (vec_count_reg <= vlen_reg - vec_pfed_count_reg)
        {
          io.vpfvaq.valid := Bool(true)
        }
        when (io.vpfvaq.ready)
        {
          addr_reg := addr_reg + stride_reg
          vec_count_reg := vec_count_reg - SInt(1)
        }
      }
    }
  }
}
