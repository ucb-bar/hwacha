package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import DataGating._

class RFirstOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val active = UInt(nSlices.W)
  val pred = UInt(nSlices.W)
  val lsidx = UInt((bVLen - bStrip).W)
  val in = Vec(nSlices, UInt(SZ_D.W))
}

class RFirstResult(implicit p: Parameters) extends VXUBundle()(p) {
  val found = Bool()
  val lsidx = UInt((bVLen - bStrip).W)
  val first = UInt(SZ_D.W)
  val sd = UInt(bSRegs.W)
}

class RFirstIO(implicit p: Parameters) extends VXUBundle()(p) {
  val op = Valid(new VRFUFn)
  val req = Decoupled(new RFirstOperand)
  val result = Flipped(Decoupled(new RFirstResult))
}

class RFirstLane(implicit p: Parameters) extends VXUModule()(p) {
  val io = IO(Flipped(new RFirstIO()))

  val result = Reg(new RFirstResult)

  when (io.op.valid) {
    result.found := false.B
    result.sd := io.op.bits.sd
  }

  io.req.ready := true.B
  val pred = PriorityEncoderOH((io.req.bits.active & io.req.bits.pred).asBools)
  val found = pred.reduce(_ || _)
  when (io.req.fire && !result.found && found) {
    result.found := true.B
    result.lsidx := io.req.bits.lsidx
    result.first := Mux1H(pred, io.req.bits.in)
  }

  io.result.bits := result
}

class RFirstMaster(implicit p: Parameters) extends VXUModule()(p) {
  val io = IO(new Bundle {
    val op = Flipped(Decoupled(new IssueOpML))
    val lane = Flipped(Vec(nLanes, Decoupled(new RFirstResult)))
    val result = Decoupled(new RFirstResult)
  })

  val opq = Module(new Queue(new IssueOpML, 2))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = RegInit(s_idle)
  val deq_lane = Reg(Vec(nLanes, Bool()))
  val fn = Reg(new VRFUFn)

  val mask_lane_valid = (deq_lane zip io.lane) map { case (deq, lane) => !deq || lane.valid }

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(state === s_busy, io.result.ready) ++ mask_lane_valid
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.result.valid := fire(io.result.ready)
  (io.lane.zipWithIndex) map { case (lane, i) =>
    lane.ready := fire(mask_lane_valid(i), deq_lane(i)) }

  opq.io.deq.ready := false.B

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := true.B
      when (opq.io.deq.valid) {
        state := s_busy
        deq_lane := Vec(opq.io.deq.bits.lane.map(_.active))
        fn := opq.io.deq.bits.fn.vrfu()
      }
    }
    is (s_busy) {
      when (fire(null)) { state := s_idle }
    }
  }

  def find_min(n: Int, s: Int): Tuple2[UInt, UInt] = {
    if (n == 1) {
      return (io.lane(s).valid && io.lane(s).bits.found, io.lane(s).bits.lsidx)
    } else {
      require(isPow2(n))
      val half = n/2
      val left = find_min(half, s)
      val right = find_min(half, s+half)
      val left_found = left._1.orR
      val right_found = right._1.orR
      val left_min = (left._2 <= right._2)
      val left_mask = left_found && (!right_found || left_min)
      val right_mask = right_found && (!left_found || !left_min)
      assert(!left_mask || !right_mask, "left and right can't be turned on at the same time")
      return (Cat(dgate(right_mask, right._1), dgate(left_mask, left._1)),
              Mux(left_mask, left._2, right._2))
    }
  }

  val m = find_min(nLanes, 0)
  io.result.bits.found := m._1.orR
  io.result.bits.lsidx := m._2 // approximate eidx
  io.result.bits.first :=
    (m._1.asBools zip io.lane.map(_.bits.first)) map { case (v, f) => dgate(v, f) } reduce(_ | _)
  io.result.bits.sd := fn.sd
}
