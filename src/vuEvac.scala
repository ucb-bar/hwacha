package hwacha

import Chisel._
import Node._
import Config._
import Interface._
import Instructions._
import Commands._

class io_vu_evac extends Bundle
{
  val cpu_exception = new io cpu_exception()

  val irb_cmdb = new io_vxu_cmdq().flip()
  val irb_imm1b = new io_vxu_immq().flip()
  val irb_imm2b = new io_vxu_imm2q().flip()
  val irb_cntb = new io_vxu_cntq().flip()
  val irb_cntb_last = Bool(INPUT)

  val vcmdq = new io_vxu_cmdq().flip()
  val vimm1q = new io_vxu_immq().flip()
  val vimm2q = new io_vxu_imm2q().flip()
  val vcntq = new io_vxu_cntq().flip()

  val vsdq = new io_vsdq()
  val vaq  = new io_lane_vaq()
}

class vuEvac extends Component
{
  val io = new io_vu_evac()
  
  val n = Bool(false)
  val y = Bool(true)

  val SEL_CMDB = Bits(0,0)
  val SEL_VCMDQ = Bits(1,0)

  val cmd_sel_next = Wire(){ Bits(width=1) }
  val cmd_sel = Reg(cmd_sel_next, resetVal = SEL_CMDB)

  cmd_sel_next := cmd_sel

  val cmd = Mux(state, io.vcmdq.bits(XCMD_CMCODE), io.irb_cmdb.bits(XCMD_CMCODE))

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
                     //   |  |  |  |  |  |  |  |  |
                     List(n, n, n, n, n, n, n, n, n), Array(
    CMD_VF->         List(y, y, y, n, y, y, y, n, y),

    CMD_VMVV      -> List(n, y, n, n, y, y, n, n, y),
    CMD_VMSV      -> List(n, y, y, n, y, y, n, n, y),
    CMD_VFMVV     -> List(n, y, n, n, y, y, n, n, y),

    CMD_VLD       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLW       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLWU      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLH       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLHU      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLB       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VLBU      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VSD       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VSW       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VSH       -> List(n, y, y, n, y, y, y, n, y),
    CMD_VSB       -> List(n, y, y, n, y, y, y, n, y),
                     
    CMD_VFLD      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VFLW      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VFSD      -> List(n, y, y, n, y, y, y, n, y),
    CMD_VFSW      -> List(n, y, y, n, y, y, y, n, y),
                     
    CMD_VLSTD     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTW     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTWU    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTH     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTHU    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTB     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VLSTBU    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VSSTD     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VSSTW     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VSSTH     -> List(n, y, y, y, y, y, y, y, y),
    CMD_VSSTB     -> List(n, y, y, y, y, y, y, y, y),

    CMD_VFLSTD    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VFLSTW    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VFSSTD    -> List(n, y, y, y, y, y, y, y, y),
    CMD_VFSSTW    -> List(n, y, y, y, y, y, y, y, y)
  ))

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: deq_vcmdq :: deq_vimm1q :: deq_vimm2q :: deq_vcntq :: Nil = cs

  val STATE_IDLE = Bits(0,4)
  val STATE_CMDB = Bits(1,4)
  val STATE_IMM1B = Bits(2,4)
  val STATE_IMM2B = Bits(3,4)
  val STATE_CNTB = Bits(4,4)
  val STATE_VCMDQ = Bits(5,4)
  val STATE_VIMM1Q = Bits(6,4)
  val STATE_VIMM2Q = Bits(7,4)
  val STATE_VCNTQ = Bits(8,4)

  val state_next = Wire(){ Bits(width=4) }
  val addr_next = Wire(){ UFix(width=SZ_ADDR) }

  val state = Reg(state_next, resetVal = STATE_IDLE)
  val addr_reg = Reg(addr_next, resetVal = UFix(0, SZ_ADDR))

  state_next := state
  addr_next := addr_reg

  val addr_plus_8 = addr_reg + 8

  io.irb_cmdb.ready := Bool(false)
  io.irb_imm1b.ready := Bool(false)
  io.irb_imm2b.ready := Bool(false)
  io.irb_cntb.ready := Bool(false)

  io.vcmdq.ready := Bool(false)
  io.vimm1q.ready := Bool(false)
  io.vimm2q.ready := Bool(false)
  io.vcntq.ready := Bool(false)

  io.vaq.bits.cmd := M_XWR
  io.vaq.bits.typ := MT_D
  io.vaq.bits.type_float := MTF_N
  io.vaq.bits.idx := addr_reg(PGIDX_BITS-1, 0)
  io.vaq.bits.ppn := addr_reg(PADDR_BITS, PGIDX_BITS)

  switch (state)
  {

    is (STATE_IDLE) 
    {
      when (io.cpu_exception.exception) 
      { 
        state_next := STATE_CMD 
        addr_next := io.cpu_exception.addr
      }
    }

    is (STATE_CMDB) 
    {
      when (io.irb_cmdb.valid && deq_ircmdb)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := Cat(Bits(0, 28), deq_irimm1b, deq_irimm2b, deq_ircntb, Bits(1,1),
                            Bits(0, 32 - VCMD_SZ), io.irb_cmdb.bits)

        when (io.vsdq.ready && io.vaq.ready) 
        {
          addr_next := addr_plus_8
          when (deq_irimm1b) 
          {
            state_next := STATE_IMM1B
          } . elsewhen (deq_irimm2b) 
          {
            state_next := STATE_IMM2B
          } . elsewhen (deq_cntb) 
          {
            state_next := STATE_CNTB
          } . otherwise
          {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }

      } . elsewhen (!io.irb_cmdb.valid) 
      {
        state_next := VSTATE_CMDQ
        cmd_sel_next := SEL_VCMDQ
      }
    }

    is (STATE_IMM1B)
    {
      when (io.irb_imm1b.valid && deq_irimm1b)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_imm1b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8          
          io.irb_imm1b.ready := Bool(true)
          when (deq_irimm2b)
          {
            state_next := STATE_IMM2B
          } . elsewhen (deq_cntb) 
          {
            state_next := STATE_CNTB
          } . otherwise 
          {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }

      }
    }

    is (STATE_IMM2B)
    {
      when (io.irb_imm2b.valid && deq_imm2b)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_imm2b.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          io.irb_imm2b.ready := Bool(true)
          when (deq_cntb)
          {
            state_next := STATE_CNTB
          } . otherwise {
            state_next := STATE_CMDB
            io.irb_cmdb.ready := Bool(true)
          }
        }

      }
    }

    is (STATE_CNTB)
    {
      when (io.irb_cntb.valid && deq_cntb)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.irb_cntb.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8          
          when (vf)
          {
            io.irb_cntb.ready := Bool(true)
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
    }

    is (STATE_VCMDQ)
    {
      when (io.vcmdq.valid && deq_vcmdq)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := Cat(Bits(0,28), deq_vimm1q, deq_vimm2q, deq_vcntq, Bits(1,1),
                            Bits(0, 32 - VCMD_SZ), io.vcmdq.bits)

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus8
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
            state_next := STATE_VCNTQ
            io.vcmdq.ready := Bool(true)
          }
        }

      } . elsewhen (!io.vcmdq.valid) 
      {
        state_next := DONE
      }
    }

    is (STATE_VIMM1Q)
    {
      when (io.vimm1q.valid && deq_vimm1q)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.vimm1q.bits

        when (io.vsdq.ready && io.vaq.readY)
        {
          addr_next := addr_plus8
          io.vimm1q.ready := Bool(true)
          when (deq_vimm2q)
          {
            state_next := STATE_VIMM2Q
          } . elsewhen (deq_vcntq) 
          {
            state_next := STATE_VCNTQ
          } .otherwsie 
          {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      }
    }

    is (STATE_VIMM2Q)
    {
      when (io.vimm2q.valid && deq_vimm2q)
      {
        io.vaq.valid := Bool(true)
        io.vsdq.valid := Bool(true)
        io.vsdq.bits := io.vimm2q.bits

        when (io.vsdq.ready && io.vaq.ready)
        {
          addr_next := addr_plus_8
          io.vimm2q.ready := Bool(true)
          when (deq_vcntq)
          {
            state_next := STATE_VCNTQ
          } . otherwise {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      }
    }

    is (STATE_VCNTQ)
    {
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
            io.vcntq.ready := Bool(true)
            when (!io.vcntq_last)
            {
              state_next := STATE_VCNTQ
            } . otherwise {
              state_next := STATE_VCMDQ
              io.vcmdq.ready := Bool(true)
            }
          } . otherwise {
            state_next := STATE_VCMDQ
            io.vcmdq.ready := Bool(true)
          }
        }

      }
    }

  }
}
