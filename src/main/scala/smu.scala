package hwacha

import Chisel._
import cde.Parameters

abstract class SMUBundle(implicit p: Parameters)
  extends HwachaBundle()(p) with SMUParameters

trait SMUParameters extends MemParameters

class SMUFn extends Bundle {
  val cmd = Bits(width = SZ_SMU_CMD)
  val mt = Bits(width = MT_SZ)
}

trait SMUData extends SMUBundle {
  val data = Bits(width = regLen)
  val tag = UInt(width = log2Up(nSRegs))
}

class SMUReq(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData {
  val fn = new SMUFn
  val addr = UInt(width = bVAddrExtended)
}

class SMUResp(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData

class SMUIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val req = Decoupled(new SMUReq)
  val resp = Decoupled(new SMUResp).flip
}
