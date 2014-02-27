package hwacha

import Chisel._
import Node._
import Constants._

class BankOpIO extends Bundle
{
  val read = Valid(new ReadBankOp)
  val write = Valid(new WriteBankOp)
  val viu = Valid(new VIUOp)
}

class Bank extends Module
{
  val io = new Bundle {
    val active = Bool(INPUT)
    
    val op = new Bundle {
      val in = new BankOpIO().flip
      val out = new BankOpIO
    }
    
    val rw = new Bundle {
      val rblen = Bits(OUTPUT, SZ_BRPORT)
      val ropl0 = Bits(OUTPUT, SZ_DATA)
      val ropl1 = Bits(OUTPUT, SZ_DATA)
      val ropl2 = Bits(OUTPUT, SZ_DATA)

      val wbl0 = Bits(INPUT, SZ_DATA)
      val wbl1 = Bits(INPUT, SZ_DATA)
      val wbl2 = Bits(INPUT, SZ_DATA)
      val wbl3 = Bits(INPUT, SZ_DATA)
      val wbl4 = Bits(INPUT, SZ_DATA)
      val wbl5 = Bits(INPUT, SZ_DATA)
    }
  }

  def op_valid(op: ValidIO[LaneOp]) = op.valid && op.bits.cnt.orR && io.active

  val s1_read_op = Reg(Valid(new ReadBankOp).asDirectionless)
  s1_read_op.valid := io.op.in.read.valid
  when (io.op.in.read.valid) {
    s1_read_op.bits.ren := io.op.in.read.bits.ren
    s1_read_op.bits.oplen := io.op.in.read.bits.oplen
    s1_read_op.bits.rblen := io.op.in.read.bits.rblen
    s1_read_op.bits.cnt := Mux(op_valid(io.op.in.read), io.op.in.read.bits.cnt - UInt(1), UInt(0))
    s1_read_op.bits.addr := io.op.in.read.bits.addr
  }
  when (this.reset) {
    s1_read_op.valid := Bool(false)
  }

  val s1_write_op = Reg(Valid(new WriteBankOp).asDirectionless)
  s1_write_op.valid := io.op.in.write.valid
  when (io.op.in.write.valid) {
    s1_write_op.bits.cnt := Mux(op_valid(io.op.in.write), io.op.in.write.bits.cnt - UInt(1), UInt(0))
    s1_write_op.bits.addr := io.op.in.write.bits.addr
    s1_write_op.bits.sel := io.op.in.write.bits.sel
  }
  when (this.reset) {
    s1_write_op.valid := Bool(false)
  }

  val s1_viu_op = Reg(Valid(new VIUOp).asDirectionless)
  s1_viu_op.valid := io.op.in.viu.valid
  when (io.op.in.viu.valid) {
    s1_viu_op.bits.cnt := Mux(op_valid(io.op.in.viu), io.op.in.viu.bits.cnt - UInt(1), UInt(0))
    s1_viu_op.bits.fn := io.op.in.viu.bits.fn
    s1_viu_op.bits.utidx := io.op.in.viu.bits.utidx + UInt(1)
    s1_viu_op.bits.imm := io.op.in.viu.bits.imm
  }
  when (this.reset) {
    s1_viu_op.valid := Bool(false)
  }

  // oplen delayed 1 cycle
  // rblen delayed 2 cycle

  val s1_read_op_valid = Reg(next=op_valid(io.op.in.read))
  val s2_rblen = Reg(next = s1_read_op.bits.rblen.toBits & Fill(SZ_BRPORT, s1_read_op_valid))

  io.rw.rblen := s2_rblen

  val rfile = Module(new BankRegfile)
  val alu = Module(new BankALU)

  rfile.io.ren := op_valid(io.op.in.read) && io.op.in.read.bits.ren
  rfile.io.raddr := io.op.in.read.bits.addr
  rfile.io.roplen := s1_read_op.bits.oplen & Fill(SZ_BOPL, s1_read_op_valid)
  
  rfile.io.wen := alu.io.wen_masked
  rfile.io.waddr := io.op.in.write.bits.addr
  rfile.io.wsel := io.op.in.write.bits.sel

  io.rw.ropl0 := rfile.io.ropl0
  io.rw.ropl1 := rfile.io.ropl1
  io.rw.ropl2 := rfile.io.ropl2

  rfile.io.wbl0 := io.rw.wbl0
  rfile.io.wbl1 := io.rw.wbl1
  rfile.io.wbl2 := io.rw.wbl2
  rfile.io.wbl3 := io.rw.wbl3
  rfile.io.wbl4 := io.rw.wbl4
  rfile.io.wbl5 := io.rw.wbl5
  rfile.io.viu_wdata := alu.io.out

  val viu_in0 = MuxLookup(
    io.op.in.viu.bits.fn.t0, UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      ML -> rfile.io.ropl0,
      MR -> rfile.io.rdata
    ))

  val viu_in1 = MuxLookup(
    io.op.in.viu.bits.fn.t1, UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      MR -> rfile.io.rdata,
      MI -> io.op.in.viu.bits.imm
    ))

  alu.io.valid := op_valid(io.op.in.viu)
  alu.io.wen := op_valid(io.op.in.write)
  alu.io.fn := io.op.in.viu.bits.fn
  alu.io.utidx := io.op.in.viu.bits.utidx
  alu.io.in0 := viu_in0
  alu.io.in1 := viu_in1

  io.op.out.read := Mux(io.active, s1_read_op, io.op.in.read)
  io.op.out.write := Mux(io.active, s1_write_op, io.op.in.write)
  io.op.out.viu := Mux(io.active, s1_viu_op, io.op.in.viu)
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
    val ropl2 = Bits(OUTPUT, SZ_DATA)

    val wbl0 = Bits(INPUT, SZ_DATA)
    val wbl1 = Bits(INPUT, SZ_DATA)
    val wbl2 = Bits(INPUT, SZ_DATA)
    val wbl3 = Bits(INPUT, SZ_DATA)
    val wbl4 = Bits(INPUT, SZ_DATA)
    val wbl5 = Bits(INPUT, SZ_DATA)
    val viu_wdata = Bits(INPUT, SZ_DATA)
  }

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0) -> io.wbl0,
      Bits(1) -> io.wbl1,
      Bits(2) -> io.wbl2,
      Bits(3) -> io.wbl3,
      Bits(4) -> io.wbl4,
      Bits(5) -> io.wbl5,
      Bits(6) -> io.viu_wdata
    ))

  val rfile = Mem(Bits(width = SZ_DATA), 256, seqRead = true)
  val raddr = Reg(Bits())
  when (io.wen) { rfile(io.waddr) := wdata }
  when (io.ren) { raddr := io.raddr }
  val rdata_rf = Mux(Reg(next=io.ren), rfile(raddr), Bits(0)) 
  io.rdata := rdata_rf

  val ropl0 = Reg(Bits(width = SZ_DATA))
  val ropl1 = Reg(Bits(width = SZ_DATA))
  val ropl2 = Reg(Bits(width = SZ_DATA))

  when (io.roplen(0)) { ropl0 := rdata_rf }
  when (io.roplen(1)) { ropl1 := rdata_rf }
  when (io.roplen(2)) { ropl2 := rdata_rf }

  io.ropl0 := ropl0
  io.ropl1 := ropl1
  io.ropl2 := ropl2
}
