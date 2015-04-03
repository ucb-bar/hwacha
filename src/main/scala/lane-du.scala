package hwacha

import Chisel._
import Node._
import Constants._
import Packing._
import DataGating._

class LaneDecoupledUnitsTag extends LaneDecoupledOp
{
  val fusel = Bits(width = 1) // because we have 2 units idiv/fdiv
}

class LaneDecoupledUnits extends HwachaModule with LaneParameters
{
  val io = new Bundle {
    val in0q = Decoupled(Bits(width = SZ_DATA)).flip
    val in1q = Decoupled(Bits(width = SZ_DATA)).flip

    val idiv = new Bundle {
      val op = Decoupled(new VIDUOp).flip
      val fus = Vec.fill(nSlices){new LaneIDivIO}
      val ack = Valid(new VIDUAck)
    }

    val fdiv = new Bundle {
      val op = Decoupled(new VFDUOp).flip
      val fus = Vec.fill(nSlices){new LaneFDivIO}
      val ack = Valid(new VFDUAck)
    }

    val bwqs = Vec.fill(nbanks){new BWQIO}
  }

  val tagq = Module(new Queue(new LaneDecoupledUnitsTag, nDecoupledUnitWBQueue))

  val deq_in0q = io.fdiv.op.bits.fn.op_is(FD_DIV)
  val mask_in0q_valid = !deq_in0q || io.in0q.valid
  val enq_idivs_req = (0 until nSlices).map { io.idiv.op.bits.pred(_) }
  val enq_fdivs_req = (0 until nSlices).map { io.fdiv.op.bits.pred(_) }
  val mask_idivs_req_ready = io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    !enq_idivs_req(i) || idiv.req.ready }
  val mask_fdivs_req_ready = io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    !enq_fdivs_req(i) || fdiv.req.ready }

  def fire_idiv(exclude: Bool, include: Bool*) = {
    val rvs = List(
      io.idiv.op.valid, io.in0q.valid, io.in1q.valid,
      tagq.io.enq.ready) ++: mask_idivs_req_ready
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  def fire_fdiv(exclude: Bool, include: Bool*) = {
    val rvs = List(
      io.fdiv.op.valid, mask_in0q_valid, io.in1q.valid,
      tagq.io.enq.ready) ++: mask_fdivs_req_ready
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  io.idiv.op.ready := fire_idiv(io.idiv.op.valid)
  io.fdiv.op.ready := fire_fdiv(io.fdiv.op.valid)
  io.in0q.ready := fire_idiv(io.in0q.valid) || fire_fdiv(mask_in0q_valid, deq_in0q)
  io.in1q.ready := fire_idiv(io.in1q.valid) || fire_fdiv(io.in1q.valid)
  tagq.io.enq.valid := fire_idiv(tagq.io.enq.ready) || fire_fdiv(tagq.io.enq.ready)
  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.valid := fire_idiv(mask_idivs_req_ready(i), enq_idivs_req(i)) }
  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.valid := fire_fdiv(mask_fdivs_req_ready(i), enq_fdivs_req(i)) }

  tagq.io.enq.bits.pred := Mux(io.idiv.op.valid, io.idiv.op.bits.pred, io.fdiv.op.bits.pred)
  tagq.io.enq.bits.bank := Mux(io.idiv.op.valid, io.idiv.op.bits.bank, io.fdiv.op.bits.bank)
  tagq.io.enq.bits.addr := Mux(io.idiv.op.valid, io.idiv.op.bits.addr, io.fdiv.op.bits.addr)
  tagq.io.enq.bits.selff := Mux(io.idiv.op.valid, io.idiv.op.bits.selff, io.fdiv.op.bits.selff)
  tagq.io.enq.bits.fusel := io.idiv.op.valid

  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.bits.fn := io.idiv.op.bits.fn
    idiv.req.bits.in0 := unpack_slice(io.in0q.bits, i)
    idiv.req.bits.in1 := unpack_slice(io.in1q.bits, i)
  }

  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.bits.fn := io.fdiv.op.bits.fn
    fdiv.req.bits.in0 := unpack_slice(io.in0q.bits, i)
    fdiv.req.bits.in1 := unpack_slice(io.in1q.bits, i)
  }

  val deq_idivs_resp = (0 until nSlices).map {  tagq.io.deq.bits.fusel.toBool && tagq.io.deq.bits.pred(_) }
  val deq_fdivs_resp = (0 until nSlices).map { !tagq.io.deq.bits.fusel.toBool && tagq.io.deq.bits.pred(_) }
  val mask_idivs_resp_valid = io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    !deq_idivs_resp(i) || idiv.resp.valid }
  val mask_fdivs_resp_valid = io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    !deq_fdivs_resp(i) || fdiv.resp.valid }

  val enq_bwqs = (0 until nbanks).map { tagq.io.deq.bits.bank === UInt(_) }
  val mask_bwqs_ready = io.bwqs.zipWithIndex.map { case (bwq, i) => !enq_bwqs(i) || bwq.ready }

  def fire_bwq(exclude: Bool, include: Bool*) = {
    val rvs = List(tagq.io.deq.valid) ++: mask_idivs_resp_valid ++: mask_fdivs_resp_valid ++: mask_bwqs_ready
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  tagq.io.deq.ready := fire_bwq(tagq.io.deq.valid)
  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.resp.ready := fire_bwq(mask_idivs_resp_valid(i), deq_idivs_resp(i)) }
  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.resp.ready := fire_bwq(mask_fdivs_resp_valid(i), deq_fdivs_resp(i)) }
  io.bwqs.zipWithIndex.map { case (bwq, i) =>
    bwq.valid := fire_bwq(mask_bwqs_ready(i), enq_bwqs(i)) }

  io.bwqs.map { bwq =>
    bwq.bits.selff := tagq.io.deq.bits.selff
    bwq.bits.addr := tagq.io.deq.bits.addr
    bwq.bits.data := Mux(tagq.io.deq.bits.fusel.toBool, repack_slice(io.idiv.fus.map(_.resp.bits.out)),
                                                        repack_slice(io.fdiv.fus.map(_.resp.bits.out)))
    bwq.bits.mask := FillInterleaved(SZ_D/8, tagq.io.deq.bits.pred)
  }

  io.idiv.ack.valid := fire_bwq(null, tagq.io.deq.bits.fusel.toBool)
  io.idiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.valid := fire_bwq(null, !tagq.io.deq.bits.fusel.toBool)
  io.fdiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.bits.exc := io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    dgate(tagq.io.deq.bits.pred(i), fdiv.resp.bits.exc) } reduce(_|_)
}
