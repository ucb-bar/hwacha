package hwacha

import Chisel._
import rocket._
import uncore._

case class HwachaConfiguration(nbanks: Int, nreg_per_bank: Int)
{
  val nreg_total = nbanks * nreg_per_bank
}

trait HwachaDecodeConstants
{
  val Y = Bool(true)
  val N = Bool(false)
  val X = Bits("b?", 1) 

  val CMD_X       = UInt("b??", 2)
  val CMD_SET_REG = UInt(0, 2)
  val CMD_SET_VL  = UInt(1, 2)
}

object HwachaDecodeTable extends HwachaDecodeConstants
{
  import HwachaInstructions._

  val default = List(N, CMD_X)
  val table = Array( 
    VSETCFG -> List(Y, CMD_SET_REG),
    VSETVL  -> List(Y, CMD_SET_VL)
  )
}

class Hwacha(hc: HwachaConfiguration, rc: RocketConfiguration) extends RoCC(rc) {
  // Decode
  val raw_inst = io.cmd.bits.inst.toBits
  val inst_i_imm = raw_inst(31, 20)
  val logic = DecodeLogic(raw_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u 
  }

  val resp_q = Module( new Queue(io.resp.bits, 2) )
  resp_q.io.deq <> io.resp

  val (inst_val: Bool) :: inst_type :: Nil = cs
  val cmd_valid = inst_val && io.cmd.valid

  // Super-simple defaults just to get some tests running
  io.cmd.ready := Bool(true)
  io.busy := Bool(false)
  io.interrupt := Bool(true)
  resp_q.io.enq.valid := Bool(false)
  resp_q.io.enq.bits.rd := io.cmd.bits.inst.rd
  resp_q.io.enq.bits.data := Bits(0)
  io.mem.req.valid := Bool(false)

  val cfg_maxvl = Reg(init=UInt(0, log2Up(hc.nreg_total)+1))
  val cfg_vl    = Reg(init=UInt(0, log2Up(hc.nreg_total)+1))
  val cfg_nxpr = Reg(init=UInt(0, log2Up(32)+1))
  val cfg_nfpr = Reg(init=UInt(0, log2Up(32)+1))
    
  switch(inst_type) {
    is(HwachaDecodeTable.CMD_SET_REG) {
      val nxpr = (io.cmd.bits.rs1( 5,0) + inst_i_imm( 5,0)).zext.toUInt
      val nfpr = (io.cmd.bits.rs1(11,6) + inst_i_imm(11,6)).zext.toUInt
      when(cmd_valid) {
        cfg_maxvl := Mux(nxpr+nfpr<UInt(2), UInt(hc.nreg_per_bank), UInt(hc.nreg_per_bank) / (nxpr-UInt(1) + nfpr)) << UInt(3)
          // TODO: DO NOT INSTANTIATE INTEGER DIVIDER IN CONTROL LOGIC BLOCK
        cfg_nxpr := nxpr
        cfg_nfpr := nfpr
        cfg_vl := UInt(0)
      }
    }
    is(HwachaDecodeTable.CMD_SET_VL) {
      val vl = Mux(io.cmd.bits.rs1 < cfg_maxvl, io.cmd.bits.rs1, cfg_maxvl)
      io.cmd.ready := Bool(false)
      resp_q.io.enq.valid := cmd_valid
      resp_q.io.enq.bits.data := vl
      when(resp_q.io.enq.ready) {
        io.cmd.ready := Bool(true)
        when(cmd_valid) { cfg_vl := vl }
      }
    }

  }

}
