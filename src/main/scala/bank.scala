package hwacha

import Chisel._
import Node._
import Constants._

class CntBundle extends Bundle
{
  val cnt = Bits(width = SZ_BCNT)
}

class BankUopRead extends CntBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val oplen = Bits(width = SZ_BOPL)
  val rblen = Vec.fill(SZ_BRPORT){Bool()}
}

class BankUopWrite extends CntBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val sel = Bits(width = SZ_BWPORT)
}

class BankUopVIU extends CntBundle
{
  val fn = Bits(width = SZ_VIU_FN)
  val utidx = Bits(width = SZ_VLEN)
  val imm = Bits(width = SZ_DATA)
}

class BankUopIO extends Bundle
{
  val read = Valid(new BankUopRead)
  val write = Valid(new BankUopWrite)
  val viu = Valid(new BankUopVIU)
}

class Bank extends Module
{
  val io = new Bundle {
    val active = Bool(INPUT)
    
    val uop = new Bundle {
      val in = new BankUopIO().flip
      val out = new BankUopIO
    }
    
    val rw = new Bundle {
      val rblen = Bits(OUTPUT, SZ_BRPORT)
      val rdata = Bits(OUTPUT, SZ_DATA)
      val ropl0 = Bits(OUTPUT, SZ_DATA)
      val ropl1 = Bits(OUTPUT, SZ_DATA)

      val wbl0 = Bits(INPUT, SZ_DATA)
      val wbl1 = Bits(INPUT, SZ_DATA)
      val wbl2 = Bits(INPUT, SZ_DATA)
      val wbl3 = Bits(INPUT, SZ_DATA)
    }

    val prec = Bits(INPUT, SZ_PREC)
  }

  def uop_valid(uop: ValidIO[CntBundle]) = uop.valid && uop.bits.cnt.orR && io.active

  val read_uop = Reg(Valid(new BankUopRead).asDirectionless)
  read_uop.valid := io.uop.in.read.valid
  when (io.uop.in.read.valid) {
    read_uop.bits.last := io.uop.in.read.bits.last
    read_uop.bits.oplen := io.uop.in.read.bits.oplen
    read_uop.bits.rblen := io.uop.in.read.bits.rblen
    read_uop.bits.cnt := Mux(uop_valid(io.uop.in.read), io.uop.in.read.bits.cnt - UInt(1), UInt(0))
    read_uop.bits.addr := io.uop.in.read.bits.addr
  }
  when (this.reset) {
    read_uop.valid := Bool(false)
    read_uop.bits.last := Bool(false)
  }

  val write_uop = Reg(Valid(new BankUopWrite).asDirectionless)
  write_uop.valid := io.uop.in.write.valid
  when (io.uop.in.write.valid) {
    write_uop.bits.last := io.uop.in.write.bits.last
    write_uop.bits.cnt := Mux(uop_valid(io.uop.in.write), io.uop.in.write.bits.cnt - UInt(1), UInt(0))
    write_uop.bits.addr := io.uop.in.write.bits.addr
    write_uop.bits.sel := io.uop.in.write.bits.sel
  }
  when (this.reset) {
    write_uop.valid := Bool(false)
    write_uop.bits.last := Bool(false)
  }

  val viu_uop = Reg(Valid(new BankUopVIU).asDirectionless)
  viu_uop.valid := io.uop.in.viu.valid
  when (io.uop.in.viu.valid) {
    viu_uop.bits.cnt := Mux(uop_valid(io.uop.in.viu), io.uop.in.viu.bits.cnt - UInt(1), UInt(0))
    viu_uop.bits.fn := io.uop.in.viu.bits.fn
    viu_uop.bits.utidx := io.uop.in.viu.bits.utidx + UInt(1)
    viu_uop.bits.imm := io.uop.in.viu.bits.imm
  }
  when (this.reset) {
    viu_uop.valid := Bool(false)
  }

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  val s1_read_uop_valid = Reg(next=uop_valid(io.uop.in.read))

  io.rw.rblen := read_uop.bits.rblen.toBits & Fill(SZ_BRPORT, s1_read_uop_valid)

  val rfile = Module(new BankRegfile)
  val alu = Module(new BankALU)

  rfile.io.ren := uop_valid(io.uop.in.read)
  rfile.io.raddr := io.uop.in.read.bits.addr
  rfile.io.roplen := read_uop.bits.oplen & Fill(SZ_BOPL, s1_read_uop_valid)
  
  rfile.io.wen := alu.io.wen_masked
  rfile.io.waddr := io.uop.in.write.bits.addr
  rfile.io.wsel := io.uop.in.write.bits.sel

  io.rw.rdata := rfile.io.rdata
  io.rw.ropl0 := rfile.io.ropl0
  io.rw.ropl1 := rfile.io.ropl1

  rfile.io.wbl0 := io.rw.wbl0
  rfile.io.wbl1 := io.rw.wbl1
  rfile.io.wbl2 := io.rw.wbl2
  rfile.io.wbl3 := io.rw.wbl3

  val viu_rdata = rfile.io.viu_rdata
  val viu_ropl = rfile.io.viu_ropl
  rfile.io.viu_wdata := alu.io.out

  val viu_in0 = MuxLookup(
    io.uop.in.viu.bits.fn(RG_VIU_T0), UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      ML -> viu_ropl,
      MR -> viu_rdata
    ))

  val viu_in1 = MuxLookup(
    io.uop.in.viu.bits.fn(RG_VIU_T1), UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      MR -> viu_rdata,
      MI -> io.uop.in.viu.bits.imm
    ))

  rfile.io.prec := io.prec

  alu.io.valid := uop_valid(io.uop.in.viu)
  alu.io.wen := uop_valid(io.uop.in.write)
  alu.io.fn := io.uop.in.viu.bits.fn
  alu.io.utidx := io.uop.in.viu.bits.utidx
  alu.io.in0 := viu_in0
  alu.io.in1 := viu_in1

  io.uop.out.read := Mux(io.active, read_uop, io.uop.in.read)
  io.uop.out.write := Mux(io.active, write_uop, io.uop.in.write)
  io.uop.out.viu := Mux(io.active, viu_uop, io.uop.in.viu)
}

class BankRegfile extends Module
{
  val io = new Bundle {
    val ren = Bool(INPUT)
    val raddr = Bits(INPUT, SZ_BREGLEN)
    val roplen = Bits(INPUT, SZ_BOPL)

    val wen = Bool(INPUT)
    val waddr = Bits(INPUT, SZ_BREGLEN)
    val wsel = Bits(INPUT, SZ_BWPORT)

    val rdata = Bits(OUTPUT, SZ_DATA)
    val ropl0 = Bits(OUTPUT, SZ_DATA)
    val ropl1 = Bits(OUTPUT, SZ_DATA)

    val wbl0 = Bits(INPUT, SZ_DATA)
    val wbl1 = Bits(INPUT, SZ_DATA)
    val wbl2 = Bits(INPUT, SZ_DATA)
    val wbl3 = Bits(INPUT, SZ_DATA)

    val viu_rdata = Bits(OUTPUT, SZ_DATA)
    val viu_ropl = Bits(OUTPUT, SZ_DATA)
    val viu_wdata = Bits(INPUT, SZ_DATA)

    val prec = Bits(INPUT, SZ_PREC)
  }

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0, SZ_BWPORT) -> io.wbl0,
      Bits(1, SZ_BWPORT) -> io.wbl1,
      Bits(2, SZ_BWPORT) -> io.wbl2,
      Bits(3, SZ_BWPORT) -> io.wbl3,
      Bits(4, SZ_BWPORT) -> io.viu_wdata
    ))

  val rfile = Mem(Bits(width = SZ_DATA), 256, seqRead = true)
  val raddr = Reg(Bits())
  when (io.wen) { rfile(io.waddr) := wdata }
  when (io.ren) { raddr := io.raddr }
  val rdata_rf = Mux(Reg(next=io.ren), rfile(raddr), Bits(0)) 
  io.rdata := rdata_rf

  val ropl0Reg = Reg(Bits(width = SZ_DATA))
  val ropl1Reg = Reg(Bits(width = SZ_DATA))
  when (io.roplen(0)) { ropl0Reg := rdata_rf }
  when (io.roplen(1)) { ropl1Reg := rdata_rf }

  io.ropl0 := ropl0Reg
  io.ropl1 := ropl1Reg

  io.viu_rdata := rdata_rf
  io.viu_ropl := io.ropl0
}
