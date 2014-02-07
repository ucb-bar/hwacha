package hwacha

import Chisel._
import Node._
import Constants._
import Instructions._
import Commands._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class io_evac_to_xcpt_handler extends Bundle
{
  val done = Bool(OUTPUT)
}

class io_evac_to_vmu extends Bundle
{
  val evac_mode = Bool(OUTPUT)
}

class io_evac_to_aiw extends Bundle
{
  val update_numCnt = new io_update_num_cnt()
}

class Evac extends Module
{
  val io = new Bundle {
    val aiw_cmdb = new io_vxu_cmdq().flip
    val aiw_imm1b = new io_vxu_immq().flip
    val aiw_imm2b = new io_vxu_imm2q().flip
    val aiw_cntb = new io_vxu_cntq().flip
    val aiw_numCntB = new io_vxu_numcntq().flip
    val aiw_numCntB_last = Bool(INPUT)

    val evac_to_aiw = new io_evac_to_aiw()

    val vcmdq = new VCMDQIO().flip

    val vsdq = new io_vsdq()
    val vaq  = new io_vvaq()

    val xcpt_to_evac = new io_xcpt_handler_to_evac().flip()
    val evac_to_xcpt = new io_evac_to_xcpt_handler()
    val evac_to_vmu = new io_evac_to_vmu()
  }
  
  val n = Bits(0,1)
  val y = Bits(1,1)

  val SEL_CMDB = Bool(false)
  val SEL_VCMDQ = Bool(true)

  val cmd_sel_next = Bool()
  val cmd_sel = Reg(next = cmd_sel_next, init = SEL_CMDB)

  cmd_sel_next := cmd_sel

  val cmd = Mux(cmd_sel,
    io.vcmdq.cmd.bits.cmcode,
    new HwachaCommand().fromBits(io.aiw_cmdb.bits).cmcode)

  val cs =
    ListLookup(cmd,
                     //   vf
                     //   |  deq_ircmdb
                     //   |  |  deq_irimm1b
                     //   |  |  |  deq_irimm2b
                     //   |  |  |  |  deq_ircntb
                     //   |  |  |  |  |  deq_vcmdq
                     //   |  |  |  |  |  |  deq_vimm1q
                     //   |  |  |  |  |  |  |  deq_vimm2q
                     //   |  |  |  |  |  |  |  |  deq_vcntq
                     //   |  |  |  |  |  |  |  |  |  is_prefetch
                     //   |  |  |  |  |  |  |  |  |  |
                     List(n, n, n, n, n, n, n, n, n, n), Array(

    CMD_VF->         List(y, y, y, n, y, y, y, n, y, n),

    CMD_VMVV      -> List(n, y, n, n, y, y, n, n, y, n),
    CMD_VMSV      -> List(n, y, y, n, y, y, y, n, y, n),
    CMD_VFMVV     -> List(n, y, n, n, y, y, n, n, y, n),

    CMD_VLD       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLW       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLWU      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLH       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLHU      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLB       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VLBU      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VSD       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VSW       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VSH       -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VSB       -> List(n, y, y, n, y, y, y, n, y, y),

    CMD_VFLD      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VFLW      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VFSD      -> List(n, y, y, n, y, y, y, n, y, y),
    CMD_VFSW      -> List(n, y, y, n, y, y, y, n, y, y),

    CMD_VLSTD     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTW     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTWU    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTH     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTHU    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTB     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VLSTBU    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VSSTD     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VSSTW     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VSSTH     -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VSSTB     -> List(n, y, y, y, y, y, y, y, y, y),

    CMD_VFLSTD    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VFLSTW    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VFSSTD    -> List(n, y, y, y, y, y, y, y, y, y),
    CMD_VFSSTW    -> List(n, y, y, y, y, y, y, y, y, y)
  ))

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: deq_vcmdq :: deq_vimm1q :: deq_vimm2q :: deq_vcntq :: is_prefetch :: Nil = cs.map(_.toBool)

  val STATE_IDLE = Bits(0,4)
  val STATE_CMDB = Bits(1,4)
  val STATE_IMM1B = Bits(2,4)
  val STATE_IMM2B = Bits(3,4)
  val STATE_CNTB = Bits(4,4)
  val STATE_PRE_VCMDQ = Bits(5,4)
  val STATE_VCMDQ = Bits(6,4)
  val STATE_VIMM1Q = Bits(7,4)
  val STATE_VIMM2Q = Bits(8,4)
  val STATE_VCNTQ = Bits(9,4)
  val STATE_DONE = Bits(10,4)

  val state_next = Bits(width=4)
  val addr_next = UInt(width=SZ_ADDR)

  val state = Reg(next = state_next, init = STATE_IDLE)
  val addr_reg = Reg(next = addr_next, init = UInt(0, SZ_ADDR))

  state_next := state
  addr_next := addr_reg

  val addr_plus_8 = addr_reg + UInt(8)

  io.aiw_cmdb.ready := Bool(false)
  io.aiw_imm1b.ready := Bool(false)
  io.aiw_imm2b.ready := Bool(false)
  io.aiw_cntb.ready := Bool(false)
  io.aiw_numCntB.ready := Bool(false)
  io.evac_to_aiw.update_numCnt.valid := Bool(false)

  io.vcmdq.cmd.ready := Bool(false)
  io.vcmdq.imm1.ready := Bool(false)
  io.vcmdq.imm2.ready := Bool(false)
  io.vcmdq.cnt.ready := Bool(false)

  io.vaq.valid := Bool(false)
  io.vaq.bits.checkcnt := Bool(false)
  io.vaq.bits.cnt := UInt(0)
  io.vaq.bits.cmd := M_XWR
  io.vaq.bits.typ := MT_D
  io.vaq.bits.typ_float := MTF_N
  io.vaq.bits.idx := addr_reg(PGIDX_BITS-1, 0)
  io.vaq.bits.vpn := addr_reg(VADDR_BITS, PGIDX_BITS)

  io.vsdq.valid := Bool(false)
  io.vsdq.bits := Bits(0)

  io.evac_to_xcpt.done := Bool(false)
  io.evac_to_vmu.evac_mode := state != STATE_IDLE

  switch (state)
  {

    is (STATE_IDLE) 
    {
      when (io.xcpt_to_evac.start) 
      { 
        state_next := STATE_CMDB 
        cmd_sel_next := SEL_CMDB
        addr_next := io.xcpt_to_evac.addr
      }
    }

    is (STATE_CMDB) 
    {
      // set valid signal
      when (io.aiw_cmdb.valid && deq_ircmdb)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := Cat(Bits(0, 27), is_prefetch, deq_irimm1b, deq_irimm2b, deq_ircntb, Bits(1,1),
                            Bits(0, 32 - SZ_VCMD), io.aiw_cmdb.bits)

        when (io.vsdq.ready && io.vaq.ready) 
        {
          addr_next := addr_plus_8
          when (deq_irimm1b) 
          {
            state_next := STATE_IMM1B
          } .elsewhen (deq_irimm2b) 
          {
            state_next := STATE_IMM2B
          } .elsewhen (deq_ircntb) 
          {
            state_next := STATE_CNTB
          } .otherwise
          {
            state_next := STATE_CMDB
          }
        }

      } .elsewhen (!io.aiw_cmdb.valid) 
      {
        state_next := STATE_VCMDQ
        cmd_sel_next := SEL_VCMDQ
      }
      
      when (deq_ircmdb && io.vsdq.ready && io.vaq.ready) 
      {
        io.aiw_cmdb.ready := !deq_irimm1b && !deq_irimm1b && !deq_ircntb
      }
    }

    is (STATE_IMM1B)
    {
      // set valid signal
      when (io.aiw_imm1b.valid && deq_irimm1b)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.aiw_imm1b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8          
          when (deq_irimm2b)
          {
            state_next := STATE_IMM2B
          } .elsewhen (deq_ircntb) 
          {
            state_next := STATE_CNTB
          } .otherwise 
          {
            state_next := STATE_CMDB
            io.aiw_cmdb.ready := Bool(true)
          }
        }

      }

    }

    is (STATE_IMM2B)
    {
      // set valid signals
      when (io.aiw_imm2b.valid && deq_irimm2b)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.aiw_imm2b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_ircntb)
          {
            state_next := STATE_CNTB
          } .otherwise {
            state_next := STATE_CMDB
            io.aiw_cmdb.ready := Bool(true)
          }
        }

      }

    }

    is (STATE_CNTB)
    {
      // set valid signal
      when (io.aiw_numCntB.valid && !io.aiw_numCntB.bits && deq_ircntb)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.aiw_cntb.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          io.aiw_cntb.ready := Bool(true)
          io.evac_to_aiw.update_numCnt.valid := Bool(true)
          state_next := STATE_CNTB
        }
      } .elsewhen (io.aiw_numCntB.valid && io.aiw_numCntB.bits.toBool)
      {
        state_next := STATE_CMDB
        when (!(io.aiw_numCntB.valid && io.aiw_numCntB_last))
        { 
          io.aiw_cmdb.ready := Bool(true) 
          io.aiw_imm1b.ready := deq_irimm1b
          io.aiw_imm2b.ready := deq_irimm2b
          io.aiw_numCntB.ready := Bool(true)

          state_next := STATE_PRE_VCMDQ
        }
      }

    }

    is (STATE_PRE_VCMDQ) // in this state we are saving a vf that is partially in CNTB and partially in VCNTQ
    {
      // valid signal
      when (io.vcmdq.cnt.valid)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.vcmdq.cnt.bits(10,0)

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8

          when (io.vcmdq.cnt.bits(11)) // the last count of a VF
          {
            state_next := STATE_VCMDQ
            cmd_sel_next := SEL_VCMDQ
          }
          .otherwise // there are more counts to save
          {
            state_next := STATE_PRE_VCMDQ
          }
        }
      }
      .otherwise // nothing in the vcntq, so no forward progress was made on the VF
      {
        state_next := STATE_VCMDQ
        cmd_sel_next := SEL_VCMDQ
      }

      // ready signal
      when (io.vsdq.ready && io.vaq.ready)
      {
        io.vcmdq.cnt.ready := Bool(true)
      }
    }

    is (STATE_VCMDQ)
    {
      // valid signal
      when (io.vcmdq.cmd.valid && deq_vcmdq)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := Cat(Bits(0,27), is_prefetch, deq_vimm1q, deq_vimm2q, deq_vcntq, Bits(1,1),
                            Bits(0, 32 - SZ_VCMD), io.vcmdq.cmd.bits.toBits)

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vimm1q)
          {
            state_next := STATE_VIMM1Q
          } .elsewhen (deq_vimm2q)
          {
            state_next := STATE_VIMM2Q
          } .elsewhen (deq_vcntq && io.vcmdq.cnt.valid)
          {
            state_next := STATE_VCNTQ
          } .otherwise 
          {
            state_next := STATE_VCMDQ
          }
        }

      } .elsewhen (!io.vcmdq.cmd.valid) 
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := Bits("hffff_ffff_ffff_ffff")
        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          state_next := STATE_DONE
        }
      }

      // ready signal
      when (deq_vcmdq && io.vsdq.ready && io.vaq.ready) 
      {
        io.vcmdq.cmd.ready := !deq_vimm1q && !deq_vimm2q && (!deq_vcntq || !io.vcmdq.cnt.valid)
      }
    }

    is (STATE_VIMM1Q)
    {
      // valid signal
      when (io.vcmdq.imm1.valid && deq_vimm1q)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.vcmdq.imm1.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vimm2q)
          {
            state_next := STATE_VIMM2Q
          } .elsewhen (deq_vcntq && io.vcmdq.cnt.valid)
          {
            state_next := STATE_VCNTQ
          } .otherwise 
          {
            state_next := STATE_VCMDQ
            io.vcmdq.cmd.ready := Bool(true)
          }
        }

      }

      // ready signal
      when (deq_vimm1q && io.vsdq.ready && io.vaq.ready) 
      {
        io.vcmdq.imm1.ready := Bool(true)
      }
    }

    is (STATE_VIMM2Q)
    {
      // valid signal
      when (io.vcmdq.imm2.valid && deq_vimm2q)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.vcmdq.imm2.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vcntq && io.vcmdq.cnt.valid)
          {
            state_next := STATE_VCNTQ
          } .otherwise 
          {
            state_next := STATE_VCMDQ
            io.vcmdq.cmd.ready := Bool(true)
          }
        }

      }

      // ready signal
      when (deq_vimm2q && io.vsdq.ready && io.vaq.ready)
      {
        io.vcmdq.imm2.ready := Bool(true)
      }
    }

    is (STATE_VCNTQ)
    {
      // valid signal
      when (io.vcmdq.cnt.valid && deq_vcntq)
      {
        io.vaq.valid := io.vsdq.ready
        io.vsdq.valid := io.vaq.ready
        io.vsdq.bits := io.vcmdq.cnt.bits(10,0)

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (vf)
          {
            state_next := STATE_VCNTQ
          } .otherwise {
            state_next := STATE_VCMDQ
            io.vcmdq.cmd.ready := Bool(true)
          }
        }

      } .elsewhen (!io.vcmdq.cnt.valid) {
        state_next := STATE_VCMDQ
        io.vcmdq.cmd.ready := Bool(true)
      }

      // ready signal
      when (deq_vcntq && io.vsdq.ready && io.vaq.ready)
      {
        io.vcmdq.cnt.ready := Bool(true)
      }
    }

    is (STATE_DONE)
    {
      io.evac_to_xcpt.done := Bool(true)
      state_next := STATE_IDLE
    }

  }
}
