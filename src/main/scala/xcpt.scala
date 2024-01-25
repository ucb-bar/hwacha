package hwacha 

import chisel3._
import org.chipsalliance.cde.config._

class XCPTIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val prop = new Bundle {
    val vu = new Bundle {
      val busy = Output(Bool())
      val flush_top = Output(Bool())
      val flush_kill = Output(Bool())
      val flush_aiw = Output(Bool())
      val flush_vxu = Output(Bool())
      val flush_vru = Output(Bool())
      val flush_vmu = Output(Bool())
    }

    val top = new Bundle {
      val stall = Output(Bool())
    }

    val issue = new Bundle {
      val stall = Output(Bool())
    }

    val seq = new Bundle {
      val stall = Output(Bool())
    }

    val vmu = new Bundle {
      val stall = Output(Bool())
      val drain = Output(Bool())
    }

    val evac = new Bundle {
      val start = Output(Bool())
      val addr = Output(UInt(regLen.W))
    }
  }
  val report = new Bundle {
    val exp = new Bundle {
      val empty = Input(Bool())
    }

    val mrt = new Bundle {
      val pending = Input(Bool())
    }

    val evac = new Bundle {
      val done = Input(Bool())
    }
  }
}

class XCPT(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new HwachaBundle {
    val rocc = new Bundle {
      val exception = Input(Bool())
      val evac = Input(Bool())
      val evac_addr = UInt(INPUT, regLen)
      val hold = Input(Bool())
      val kill = Input(Bool())
    }

    val vu = new XCPTIO
  }

  val hold_top = RegInit(false.B)
  val hold_vu = RegInit(false.B)
  val hold_tlb = RegInit(false.B)

  // output assignments
  io.vu.prop.top.stall := hold_top
  io.vu.prop.issue.stall := hold_vu
  io.vu.prop.seq.stall := hold_vu
  io.vu.prop.vmu.stall := hold_tlb
  io.vu.prop.vmu.drain := false.B

  val NORMAL = Bits(0, 3)
  val XCPT_DRAIN = Bits(1, 3)
  val XCPT_FLUSH = Bits(2, 3)
  val XCPT_EVAC = Bits(3, 3)
  val XCPT_DRAIN_EVAC = Bits(4, 3)  
  val HOLD = Bits(5, 3)

  val state = RegInit(NORMAL)
  val addr = RegInit(UInt(0, regLen))
  val evac = RegInit(false.B)
  val kill = RegInit(false.B)

  when (io.rocc.evac) {
    evac := true.B
    addr := io.rocc.evac_addr
  }

  when (io.rocc.kill) {
    kill := true.B
  }

  io.vu.prop.vu.busy := (state =/= NORMAL) && (state =/= HOLD)
  io.vu.prop.vu.flush_top := false.B
  io.vu.prop.vu.flush_kill := false.B
  io.vu.prop.vu.flush_aiw := false.B
  io.vu.prop.vu.flush_vxu := false.B
  io.vu.prop.vu.flush_vru := false.B
  io.vu.prop.vu.flush_vmu := false.B

  io.vu.prop.evac.start := false.B
  io.vu.prop.evac.addr := addr

  switch (state) {

    is (NORMAL) {
      when (io.rocc.exception) {
        hold_top := true.B
        hold_vu := true.B
        hold_tlb := true.B

        evac := false.B
        kill := false.B

        state := XCPT_DRAIN
      }

      when (io.rocc.hold) {
        hold_vu := true.B
        hold_tlb := true.B

        state := HOLD
      }
    }

    is (XCPT_DRAIN) {
      when (io.vu.report.exp.empty && !io.vu.report.mrt.pending) {
        hold_top := false.B

        state := XCPT_FLUSH
      }
    }

    is (XCPT_FLUSH) {
      io.vu.prop.vu.flush_top := true.B
      io.vu.prop.vu.flush_vxu := true.B
      io.vu.prop.vu.flush_vru := true.B
      io.vu.prop.vu.flush_vmu := true.B

      when (kill) {
        io.vu.prop.vu.flush_kill := true.B
        io.vu.prop.vu.flush_aiw := true.B
      }

      when (evac) {
        hold_tlb := false.B

        state := XCPT_EVAC
      }

      when (kill) {
        hold_vu := false.B
        hold_tlb := false.B
        kill := false.B
        
        state := NORMAL
      }
    }

    is (XCPT_EVAC) {
      io.vu.prop.evac.start := true.B
      io.vu.prop.vmu.drain := true.B

      when (io.vu.report.evac.done) {
        state := XCPT_DRAIN_EVAC
      }
    }

    is (XCPT_DRAIN_EVAC) {
      io.vu.prop.vmu.drain := true.B

      when (!io.vu.report.mrt.pending) {
        hold_vu := false.B
        hold_tlb := false.B
        evac := false.B
        
        state := NORMAL
      }
    }

    is (HOLD) {
      when (!io.rocc.hold) {
        hold_vu := false.B
        hold_tlb := false.B

        state := NORMAL
      }
    }

  }
}
