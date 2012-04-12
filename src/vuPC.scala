package hwacha

import Chisel._
import Node._
import Constants._


class PCFire extends Bundle
{
  val pc = Bits(SZ_ADDR, OUTPUT)
  val fire = Bool(OUTPUT)
}

class IoPCToVT extends ioDecoupled()( Bits(width=SZ_ADDR) )

class IoPC extends Bundle
{
  val flush = Bool(INPUT)
  val tvecToPC = new PCFire().flip()
  val pc = new IoPCToVT()
  val vtToPC = new IoVTToPC().flip()
}

class vuPC extends Component
{
  val io = new IoPC()

  val next_pc = Wire(){ Bits(width=SZ_ADDR) }
  val next_valid = Wire(){ Bool() }

  val reg_pc = Reg(next_pc, resetVal = Bits(0,SZ_ADDR))
  val reg_valid = Reg(next_valid, resetVal = Bool(false))

  next_pc := reg_pc
  next_valid := reg_valid

  when (io.tvecToPC.fire) 
  {
    next_pc := io.tvecToPC.pc
    next_valid := Bool(true)
  }
  . elsewhen (io.pc.ready) 
  {
    next_pc := reg_pc + UFix(4)
  }

  when (io.vtToPC.stop)
  {
    next_valid := Bool(false)
  }

  when(io.flush)
  {
    next_pc := Bits(0,SZ_ADDR)
    next_valid := Bool(false)
  }

  io.pc.bits := reg_pc
  io.pc.valid := reg_valid
}
