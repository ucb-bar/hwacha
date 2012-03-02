package hwacha

import Chisel._
import Node._
import Constants._

class RegfileIO extends Bundle
{
  val ren    = Bool(INPUT)
  val raddr  = Bits(SZ_BREGLEN, INPUT)
  val roplen = Bits(SZ_BOPL, INPUT)

  val wen   = Bool(INPUT)
  val waddr = Bits(SZ_BREGLEN, INPUT)
  val wsel  = Bits(SZ_BWPORT, INPUT)

  val rdata = Bits(SZ_DATA, OUTPUT)
  val ropl0 = Bits(SZ_DATA, OUTPUT)
  val ropl1 = Bits(SZ_DATA, OUTPUT)

  val wbl0 = Bits(SZ_DATA, INPUT)
  val wbl1 = Bits(SZ_DATA, INPUT)
  val wbl2 = Bits(SZ_DATA, INPUT)
  val wbl3 = Bits(SZ_DATA, INPUT)

  val viu_rdata = Bits(SZ_DATA, OUTPUT)
  val viu_ropl  = Bits(SZ_DATA, OUTPUT)
  val viu_wdata = Bits(SZ_DATA, INPUT)
}

class vuVXU_Banked8_Bank_Regfile extends Component
{
  val io = new RegfileIO()

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0, SZ_BWPORT) -> io.wbl0,
      Bits(1, SZ_BWPORT) -> io.wbl1,
      Bits(2, SZ_BWPORT) -> io.wbl2,
      Bits(3, SZ_BWPORT) -> io.wbl3,
      Bits(4, SZ_BWPORT) -> io.viu_wdata
    ))

  val rfile = Mem(256, io.wen, io.waddr.toUFix, wdata, resetVal = null)
  rfile.setReadLatency(1)
  rfile.setTarget('inst)
  val rdata_rf = Mux(Reg(io.ren), rfile(io.raddr), Bits(0)) 
  io.rdata := rdata_rf

  val ropl0Reg = Reg(){Bits(width = SZ_DATA)}
  val ropl1Reg = Reg(){Bits(width = SZ_DATA)}
  when(io.roplen(0)) { ropl0Reg := rdata_rf }
  when(io.roplen(1)) { ropl1Reg := rdata_rf }

  io.ropl0 := ropl0Reg
  io.ropl1 := ropl1Reg

  io.viu_rdata := rdata_rf
  io.viu_ropl := io.ropl0
}
