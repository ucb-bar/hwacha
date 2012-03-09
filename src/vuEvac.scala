package hwacha

import Chisel._
import Node._
import Constants._
import Instructions._
import Commands._

class io_evac_to_xcpt_handler extends Bundle
{
  val done = Bool(OUTPUT)
}

class io_vu_evac extends Bundle
{
  val irb_cmdb = new io_vxu_cmdq().flip
  val irb_imm1b = new io_vxu_immq().flip
  val irb_imm2b = new io_vxu_imm2q().flip
  val irb_cntb = new io_vxu_cntq().flip
  val irb_cntb_last = Bool(INPUT)

  val vcmdq = new io_vxu_cmdq().flip
  val vimm1q = new io_vxu_immq().flip
  val vimm2q = new io_vxu_imm2q().flip
  val vcntq = new io_vxu_cntq().flip

  val vsdq = new io_vsdq()
  val vaq  = new io_vvaq()

  val xcpt_to_evac = new io_xcpt_handler_to_evac().flip()
  val evac_to_xcpt = new io_evac_to_xcpt_handler()
}

class vuEvac extends Component
{
  val io = new io_vu_evac()
  
  val n = Bool(false)
  val y = Bool(true)

  val SEL_CMDB = Bits(0,1)
  val SEL_VCMDQ = Bits(1,1)

  val cmd_sel_next = Wire(){ Bits(width=1) }
  val cmd_sel = Reg(cmd_sel_next, resetVal = SEL_CMDB)

  cmd_sel_next := cmd_sel

  val cmd = Mux(cmd_sel, io.vcmdq.bits(RG_XCMD_CMCODE), io.irb_cmdb.bits(RG_XCMD_CMCODE))

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

    CMD_FENCE_L_V -> List(n, y, n, n, n, y, n, n, n, n), 
    CMD_FENCE_G_V -> List(n, y, n, n, n, y, n, n, n, n), 

    CMD_VF->         List(y, y, y, n, y, y, y, n, y, n),

    CMD_VMVV      -> List(n, y, n, n, y, y, n, n, y, n),
    CMD_VMSV      -> List(n, y, y, n, y, y, n, n, y, n),
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

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: deq_vcmdq :: deq_vimm1q :: deq_vimm2q :: deq_vcntq :: is_prefetch :: Nil = cs

  val STATE_IDLE = Bits(0,4)
  val STATE_CMDB = Bits(1,4)
  val STATE_IMM1B = Bits(2,4)
  val STATE_IMM2B = Bits(3,4)
  val STATE_CNTB = Bits(4,4)
  val STATE_VCMDQ = Bits(5,4)
  val STATE_VIMM1Q = Bits(6,4)
  val STATE_VIMM2Q = Bits(7,4)
  val STATE_VCNTQ = Bits(8,4)
  val STATE_DONE = Bits(9,4)

  val state_next = Wire(){ Bits(width=4) }
  val addr_next = Wire(){ UFix(width=SZ_ADDR) }

  val state = Reg(state_next, resetVal = STATE_IDLE)
  val addr_reg = Reg(addr_next, resetVal = UFix(0, SZ_ADDR))

  state_next := state
  addr_next := addr_reg

  val addr_plus_8 = addr_reg + UFix(8)

  io.irb_cmdb.ready := Bool(false)
  io.irb_imm1b.ready := Bool(false)
  io.irb_imm2b.ready := Bool(false)
  io.irb_cntb.ready := Bool(false)

  io.vcmdq.ready := Bool(false)
  io.vimm1q.ready := Bool(false)
  io.vimm2q.ready := Bool(false)
  io.vcntq.ready := Bool(false)

  io.vaq.valid := Bool(false)
  io.vaq.bits.checkcnt := Bool(false)
  io.vaq.bits.cnt := UFix(0)
  io.vaq.bits.cmd := M_XWR
  io.vaq.bits.typ := MT_D
  io.vaq.bits.typ_float := MTF_N
  io.vaq.bits.idx := addr_reg(PGIDX_BITS-1, 0)
  io.vaq.bits.vpn := addr_reg(VADDR_BITS, PGIDX_BITS)

  io.vsdq.valid := Bool(false)
  io.vsdq.bits := Bits(0)

  io.evac_to_xcpt.done := Bool(false)

  switch (state)
  {

    is (STATE_IDLE) 
    {
      when (io.xcpt_to_evac.start) 
      { 
        state_next := STATE_CMDB 
        addr_next := io.xcpt_to_evac.addr
      }
    }

    is (STATE_CMDB) 
    {
      // set valid signal
      when (io.irb_cmdb.valid && deq_ircmdb)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := Cat(Bits(0, 27), is_prefetch, deq_irimm1b, deq_irimm2b, deq_ircntb, Bits(1,1),
                            Bits(0, 32 - SZ_VCMD), io.irb_cmdb.bits)

        when (io.vsdq.ready && io.vaq.ready) 
        {
          addr_next := addr_plus_8
          when (deq_irimm1b) 
          {
            state_next := STATE_IMM1B
          } . elsewhen (deq_irimm2b) 
          {
            state_next := STATE_IMM2B
          } . elsewhen (deq_ircntb) 
          {
            state_next := STATE_CNTB
          } . otherwise
          {
            state_next := STATE_CMDB
          }
        }

      } . elsewhen (!io.irb_cmdb.valid) 
      {
        state_next := STATE_VCMDQ
        cmd_sel_next := SEL_VCMDQ
      }
      
      // set ready signal
      when (deq_ircmdb && io.vsdq.ready && io.vaq.ready) 
      {
        io.irb_cmdb.ready := !deq_irimm1b && !deq_irimm1b && !deq_ircntb
      }
    }

    is (STATE_IMM1B)
    {
      // set valid signal
      when (io.irb_imm1b.valid && deq_irimm1b)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_imm1b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8          
          when (deq_irimm2b)
          {
            state_next := STATE_IMM2B
          } . elsewhen (deq_ircntb) 
          {
            state_next := STATE_CNTB
          } . otherwise 
          {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }

      }

      // set ready signal
      when (deq_irimm1b && io.vsdq.ready && io.vaq.ready)
      {
        io.irb_imm1b.ready := Bool(true)
      }
    }

    is (STATE_IMM2B)
    {
      // set valid signals
      when (io.irb_imm2b.valid && deq_irimm2b)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_imm2b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_ircntb)
          {
            state_next := STATE_CNTB
          } . otherwise {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }

      }

      // set ready signal
      when (deq_irimm2b && io.vsdq.ready && io.vaq.ready) 
      {
        io.irb_imm2b.ready := Bool(true)
      }
    }

    is (STATE_CNTB)
    {
      // set valid signal
      when (io.irb_cntb.valid && deq_ircntb)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_cntb.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8          
          when (vf)
          {
            when (!io.irb_cntb_last)
            {
              state_next := STATE_CNTB
            } . otherwise {
              state_next := STATE_CMDB
              io.irb_cmdb.ready := Bool(true)
            }
          } . otherwise {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }
      } .elsewhen (!io.irb_cntb.valid && vf) 
      {
        state_next := STATE_CMDB
        io.irb_cmdb.ready := Bool(true)
      }

      // set ready signal
      when (deq_ircntb && io.vsdq.ready && io.vaq.ready)
      {
        io.irb_cntb.ready := Bool(true)
      }
    }

    is (STATE_VCMDQ)
    {
      // valid signal
      when (io.vcmdq.valid && deq_vcmdq)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := Cat(Bits(0,28), deq_vimm1q, deq_vimm2q, deq_vcntq, Bits(1,1),
                            Bits(0, 32 - SZ_VCMD), io.vcmdq.bits)

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vimm1q)
          {
            state_next := STATE_VIMM1Q
          } . elsewhen (deq_vimm2q)
          {
            state_next := STATE_VIMM2Q
          } . elsewhen (deq_vcntq)
          {
            state_next := STATE_VCNTQ
          } . otherwise 
          {
            state_next := STATE_VCMDQ
          }
        }

      } . elsewhen (!io.vcmdq.valid) 
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
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
        io.vcmdq.ready := !deq_vimm1q && !deq_vimm2q && !deq_vcntq
      }
    }

    is (STATE_VIMM1Q)
    {
      // valid signal
      when (io.vimm1q.valid && deq_vimm1q)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.vimm1q.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vimm2q)
          {
            state_next := STATE_VIMM2Q
          } . elsewhen (deq_vcntq) 
          {
            state_next := STATE_VCNTQ
          } .otherwise 
          {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      }

      // ready signal
      when(deq_vimm1q && io.vsdq.ready && io.vaq.ready) 
      {
        io.vimm1q.ready := Bool(true)
      }
    }

    is (STATE_VIMM2Q)
    {
      // valid signal
      when (io.vimm2q.valid && deq_vimm2q)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.vimm2q.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (deq_vcntq)
          {
            state_next := STATE_VCNTQ
          } . otherwise {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      }

      // ready signal
      when (deq_vimm2q && io.vsdq.ready && io.vaq.ready)
      {
        io.vimm2q.ready := Bool(true)
      }
    }

    is (STATE_VCNTQ)
    {
      // valid signal
      when (io.vcntq.valid && deq_vcntq)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.vcntq.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          when (vf)
          {
            state_next := STATE_VCNTQ
          } . otherwise {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      } . elsewhen(!io.vcntq.valid && vf) {
        state_next := STATE_VCMDQ
        io.vcmdq.ready := Bool(true)
      }

      // ready signal
      when(deq_vcntq && io.vsdq.ready && io.vaq.ready)
      {
        io.vcntq.ready := Bool(true)
      }
    }

    is (STATE_DONE)
    {
      io.evac_to_xcpt.done := Bool(true)
      when(!io.xcpt_to_evac.start) 
      {
        state_next := STATE_IDLE
      }
    }

  }
}
