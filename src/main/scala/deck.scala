package hwacha

import Chisel._
import Node._
import Constants._
import scala.collection.mutable.ArrayBuffer

class DeckOpIO extends DecoupledIO(new DeckOp)

class Deck(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val op = new DeckOpIO().flip

    val lla = new LookAheadPortIO(log2Down(conf.nvldq)+1).flip()
    val sla = new LookAheadPortIO(log2Down(conf.nvsdq)+1).flip()

    val brqs = Vec.fill(conf.nbanks){new BRQIO().flip}
    val bwqs = Vec.fill(conf.nbanks){new BWQIO}

    val vmu = new vmunit.VMUIO
  }

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  io.op.ready := state === s_idle

  val reg_op = Reg(new DeckOp)
  val next_utidx = reg_op.utidx + UInt(1)

  val lgbank = log2Up(conf.nbanks)
  val bid = reg_op.utidx(lgbank-1,0)
  val belement = reg_op.utidx(SZ_VLEN-1,lgbank)

  switch (state) {
    is (s_idle) {
      when (io.op.valid) {
        state := s_busy
        reg_op := io.op.bits
      }
    }
    is (s_busy) {
      when (io.vmu.sdata.fire()) {
        reg_op.utidx := next_utidx
        when (next_utidx === reg_op.vlen) {
          state := s_idle
        }
      }
    }
  }

  val s_ld = state === s_busy && reg_op.fn.lreq()
  val s_st = state === s_busy && reg_op.fn.sreq()

  for (i <- 0 until conf.nbanks) {
    io.brqs(i).ready := Bool(false)
  }

  val brqs = new ArrayBuffer[DecoupledIO[BRQEntry]]

  var slacntr_available = Bool(true)

  for (i <- 0 until conf.nbanks) {
    val brq = Module(new Queue(new BRQEntry, 2))
    val slacntr = Module(new LookAheadCounter(2, 2))

    brq.io.enq <> io.brqs(i)
    slacntr.io.la.cnt := io.sla.cnt > UInt(i)
    slacntr.io.la.reserve := io.sla.reserve
    slacntr_available = slacntr_available && slacntr.io.la.available
    slacntr.io.inc := (bid === UInt(i)) && io.vmu.sdata.fire()
    slacntr.io.dec := Bool(false)

    brqs += brq.io.deq
  }

   io.sla.available := slacntr_available

  val brqs_deq = Vec(brqs)

  for (i <- 0 until conf.nbanks) {
    brqs_deq(i).ready := Bool(false)
  }

  io.vmu.sdata.valid := s_st && brqs_deq(bid).valid
  io.vmu.sdata.bits := brqs_deq(bid).bits.data
  brqs_deq(bid).ready := s_st && io.vmu.sdata.ready

  for (i <- 0 until conf.nbanks) {
    io.bwqs(i).valid := Bool(false)
  }

//  // VSU
//  // Timing Diagram
//  // | Seq | Exp/SRAM-Addr-Setup | SRAM-Clock-Q | XBar/Recoding/VSDQ-Setup |
//  //                                              ^ io.op.vsu starts here
//  val rf64f64 = Module(new hardfloat.recodedFloat64ToFloat64)
//  rf64f64.io.in := unpack_float_d(io.data.sdata, 0)
//  val sdata_f_dp = rf64f64.io.out
//
//  val rf32f32 = Module(new hardfloat.recodedFloat32ToFloat32)
//  rf32f32.io.in := unpack_float_s(io.data.sdata, 0)
//  val sdata_f_sp = rf32f32.io.out
//
//  val sdata_f_hp = unpack_float_h(io.data.sdata, 0)
//
//  val store_fp = io.op.vsu.bits.fn.float
//  val store_fp_d = store_fp && io.op.vsu.bits.fn.typ === MT_D
//  val store_fp_w = store_fp && io.op.vsu.bits.fn.typ === MT_W
//  val store_fp_h = store_fp && io.op.vsu.bits.fn.typ === MT_H
//
//  io.vmu.sdata.q.valid := io.op.vsu.valid
//  io.vmu.sdata.q.bits := MuxCase(
//    io.data.sdata(63,0), Array(
//      (store_fp_d) -> sdata_f_dp,
//      (store_fp_w) -> sdata_f_sp,
//      (store_fp_h) -> sdata_f_hp
//    ))
//
//  // VLU
//  val s1_vlu_op = Reg(Valid(new VLUOp).asDirectionless)
//  s1_vlu_op.valid := io.op.vlu.valid
//  when (io.op.vlu.valid) {
//    s1_vlu_op.bits := io.op.vlu.bits
//  }
//  val s1_ldata = RegEnable(io.vmu.ldata.q.bits, io.op.vlu.valid)
//
//  val f64rf64 = Module(new hardfloat.float64ToRecodedFloat64)
//  f64rf64.io.in := s1_ldata
//  val s1_ldata_rf_dp = f64rf64.io.out
//
//  val f32rf32 = Module(new hardfloat.float32ToRecodedFloat32)
//  f32rf32.io.in := s1_ldata
//  val s1_ldata_rf_sp = f32rf32.io.out
//
//  val s1_load_fp = s1_vlu_op.bits.fn.float
//  val s1_load_fp_d = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_D
//  val s1_load_fp_w = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_W
//  val s1_load_fp_h = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_H
//
//  io.vmu.ldata.q.ready := io.op.vlu.valid
//  io.data.ldata := MuxCase(
//    s1_ldata, Array(
//      (s1_load_fp_d) -> pack_float_d(s1_ldata_rf_dp, 0),
//      (s1_load_fp_w) -> pack_float_s(s1_ldata_rf_sp, 0),
//      (s1_load_fp_h) -> pack_float_h(s1_ldata, 0)
//    ))
}
