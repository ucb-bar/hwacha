package hwacha

import Chisel._
import Constants._
import uncore._

trait VMUParameters extends UsesHwachaParameters {
  val tagBits = log2Up(params(HwachaNVectorLoadMetaBufferEntries))

  val vaddrBits = params(VAddrBits)
  val paddrBits = params(PAddrBits)
  val pgIdxBits = params(PgIdxBits)
  val pgSzBytes = 1 << pgIdxBits
  val vpnBits = params(VPNBits)
  val ppnBits = params(PPNBits)
  val maxAddrBits = math.max(vaddrBits, paddrBits)

  val tlBlockAddrBits = params(TLBlockAddrBits)
  val tlBeatAddrBits = log2Up(params(TLDataBeats))
  val tlDataBits = params(TLDataBits)
  val tlDataBytes = tlDataBits >> 3
  val tlByteAddrBits = log2Up(tlDataBytes)

  val palaMax = 1 << (SZ_VLEN-1)
  val palaBits = log2Down(palaMax) + 1
  val valaMax = confvmu.nvvaq
  val valaBits = log2Down(valaMax) + 1

  val tlDataHalves = tlDataBits >> 4
  val tlDataWords = tlDataBits >> 5
  val tlDataDoubles = tlDataBits >> 6
  require((tlDataBits & (SZ_XD-1)) == 0)
}

class VMUOpIO extends Bundle {
  val cmd = Decoupled(new VMUOpCmd)
  val addr = Decoupled(new VMUOpAddr)
}

class VMUIO extends Bundle {
  val issue = new VMUOpIO
  val vaq = new VAQLaneIO
  val vsdq = new VSDQIO
  val vldq = new VLDQIO().flip
}

class VMUDecodedOp extends Bundle {
  val fn = new Bundle {
    val load = Bool()
    val store = Bool()
    val amo = Bool()
    val indexed = Bool()
  }
  val mt = new DecodedMemType
  val unit = Bool()

  val cmd = new VMUOpCmd
  val addr = new VMUOpAddr
}

object VMUDecodedOp {
  def apply(cmd: VMUOpCmd, addr: VMUOpAddr): VMUDecodedOp = {
    val op = new VMUDecodedOp
    op.cmd := cmd
    op.addr := addr

    val mcmd = vmu_op_mcmd(cmd.fn)
    op.fn.load := (mcmd === M_XRD)
    op.fn.store := (mcmd === M_XWR)
    op.fn.amo := isAMO(mcmd)
    op.fn.indexed := !vmu_op_tvec(cmd.fn)
    op.mt := DecodedMemType(cmd.mt)

    op.unit :=
      Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d).zipWithIndex.map(i =>
        i._1 && (addr.stride === UInt(1 << i._2))).reduce(_||_)
    op
  }
}

class VMUIssueOpIO extends Bundle {
  val op = new VMUDecodedOp().asOutput
  val fire = Bool(OUTPUT)
  val busy = Bool(INPUT)
}

class IBox extends VMUModule {
  val io = new Bundle {
    val op = new VMUOpIO().flip
    val abox = new VMUIssueOpIO
    val sbox = new VMUIssueOpIO
  }

  val cmdq = Module(new Queue(new VMUOpCmd, confvmu.ncmdq))
  val addrq = Module(new Queue(new VMUOpAddr, confvmu.naddrq))

  cmdq.io.enq <> io.op.cmd
  addrq.io.enq <> io.op.addr

  val op = VMUDecodedOp(cmdq.io.deq.bits, addrq.io.deq.bits)
  private val backends = Seq(io.abox, io.sbox)
  for (box <- backends) {
    box.op := op
    box.fire := Bool(false)
  }

  val valid = cmdq.io.deq.valid &&
    (op.fn.indexed || addrq.io.deq.valid)
  cmdq.io.deq.ready := Bool(false)
  addrq.io.deq.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (valid) {
        state := s_busy
        io.abox.fire := Bool(true)
        io.sbox.fire := op.fn.store || op.fn.amo
      }
    }

    is (s_busy) {
      when (!backends.map(_.busy).reduce(_||_)) {
        state := s_idle
        cmdq.io.deq.ready := Bool(true)
        addrq.io.deq.ready := !op.fn.indexed
      }
    }
  }
}

class VMU(resetSignal: Bool = null) extends VMUModule(_reset = resetSignal) {

  val io = new Bundle {
    val lane = new VMUIO().flip
    val evac = new Bundle {
      val vaq = new VVAQIO().flip
      val vsdq = new VSDQIO().flip
    }
    val pf = new Bundle {
      val vaq = new VVAPFQIO().flip
    }
    val memif = new VMUMemIO

    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val irq = new IRQIO
    val xcpt = new XCPTIO().flip
  }

  val ibox = Module(new IBox)
  val abox = Module(new ABox)
  val tbox = Module(new TBox)
  val sbox = Module(new SBox)
  val lbox = Module(new LBox)
  val mbox = Module(new MBox)

  ibox.io.op <> io.lane.issue

  abox.io.issue <> ibox.io.abox
  abox.io.xcpt <> io.xcpt
  abox.io.irq <> io.irq
  abox.io.lane <> io.lane.vaq
  abox.io.evac <> io.evac.vaq
  abox.io.pf <> io.pf.vaq

  tbox.io.abox <> abox.io.tbox
  tbox.io.vtlb <> io.vtlb
  tbox.io.vpftlb <> io.vpftlb
  tbox.io.xcpt <> io.xcpt

  sbox.io.issue <> ibox.io.sbox
  sbox.io.xcpt <> io.xcpt
  sbox.io.lane <> io.lane.vsdq
  sbox.io.evac <> io.evac.vsdq

  lbox.io.lane <> io.lane.vldq

  mbox.io.in.abox <> abox.io.mbox
  mbox.io.in.sbox <> sbox.io.mbox
  mbox.io.in.lbox <> lbox.io.mbox
  io.memif <> mbox.io.out
}
