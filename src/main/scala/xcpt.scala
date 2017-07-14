package hwacha 

import Chisel._
import freechips.rocketchip.config._

class XCPTIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val prop = new Bundle {
    val vu = new Bundle {
      val busy = Bool(OUTPUT)
      val flush_top = Bool(OUTPUT)
      val flush_kill = Bool(OUTPUT)
      val flush_aiw = Bool(OUTPUT)
      val flush_vxu = Bool(OUTPUT)
      val flush_vru = Bool(OUTPUT)
      val flush_vmu = Bool(OUTPUT)
    }

    val top = new Bundle {
      val stall = Bool(OUTPUT)
    }

    val issue = new Bundle {
      val stall = Bool(OUTPUT)
    }

    val seq = new Bundle {
      val stall = Bool(OUTPUT)
    }

    val vmu = new Bundle {
      val stall = Bool(OUTPUT)
      val drain = Bool(OUTPUT)
    }

    val evac = new Bundle {
      val start = Bool(OUTPUT)
      val addr = UInt(OUTPUT, regLen)
    }
  }
  val report = new Bundle {
    val exp = new Bundle {
      val empty = Bool(INPUT)
    }

    val mrt = new Bundle {
      val pending = Bool(INPUT)
    }

    val evac = new Bundle {
      val done = Bool(INPUT)
    }
  }
}

class XCPT(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new HwachaBundle {
    val rocc = new Bundle {
      val exception = Bool(INPUT)
      val evac = Bool(INPUT)
      val evac_addr = UInt(INPUT, regLen)
      val hold = Bool(INPUT)
      val kill = Bool(INPUT)
    }

    val vu = new XCPTIO
  }

  val hold_top = Reg(init = Bool(false))
  val hold_vu = Reg(init = Bool(false))
  val hold_tlb = Reg(init = Bool(false))

  // output assignments
  io.vu.prop.top.stall := hold_top
  io.vu.prop.issue.stall := hold_vu
  io.vu.prop.seq.stall := hold_vu
  io.vu.prop.vmu.stall := hold_tlb
  io.vu.prop.vmu.drain := Bool(false)

  val NORMAL = Bits(0, 3)
  val XCPT_DRAIN = Bits(1, 3)
  val XCPT_FLUSH = Bits(2, 3)
  val XCPT_EVAC = Bits(3, 3)
  val XCPT_DRAIN_EVAC = Bits(4, 3)  
  val HOLD = Bits(5, 3)

  val state = Reg(init = NORMAL)
  val addr = Reg(init = UInt(0, regLen))
  val evac = Reg(init = Bool(false))
  val kill = Reg(init = Bool(false))

  when (io.rocc.evac) {
    evac := Bool(true)
    addr := io.rocc.evac_addr
  }

  when (io.rocc.kill) {
    kill := Bool(true)
  }

  io.vu.prop.vu.busy := (state =/= NORMAL) && (state =/= HOLD)
  io.vu.prop.vu.flush_top := Bool(false)
  io.vu.prop.vu.flush_kill := Bool(false)
  io.vu.prop.vu.flush_aiw := Bool(false)
  io.vu.prop.vu.flush_vxu := Bool(false)
  io.vu.prop.vu.flush_vru := Bool(false)
  io.vu.prop.vu.flush_vmu := Bool(false)

  io.vu.prop.evac.start := Bool(false)
  io.vu.prop.evac.addr := addr

  switch (state) {

    is (NORMAL) {
      when (io.rocc.exception) {
        hold_top := Bool(true)
        hold_vu := Bool(true)
        hold_tlb := Bool(true)

        evac := Bool(false)
        kill := Bool(false)

        state := XCPT_DRAIN
      }

      when (io.rocc.hold) {
        hold_vu := Bool(true)
        hold_tlb := Bool(true)

        state := HOLD
      }
    }

    is (XCPT_DRAIN) {
      when (io.vu.report.exp.empty && !io.vu.report.mrt.pending) {
        hold_top := Bool(false)

        state := XCPT_FLUSH
      }
    }

    is (XCPT_FLUSH) {
      io.vu.prop.vu.flush_top := Bool(true)
      io.vu.prop.vu.flush_vxu := Bool(true)
      io.vu.prop.vu.flush_vru := Bool(true)
      io.vu.prop.vu.flush_vmu := Bool(true)

      when (kill) {
        io.vu.prop.vu.flush_kill := Bool(true)
        io.vu.prop.vu.flush_aiw := Bool(true)
      }

      when (evac) {
        hold_tlb := Bool(false)

        state := XCPT_EVAC
      }

      when (kill) {
        hold_vu := Bool(false)
        hold_tlb := Bool(false)
        kill := Bool(false)
        
        state := NORMAL
      }
    }

    is (XCPT_EVAC) {
      io.vu.prop.evac.start := Bool(true)
      io.vu.prop.vmu.drain := Bool(true)

      when (io.vu.report.evac.done) {
        state := XCPT_DRAIN_EVAC
      }
    }

    is (XCPT_DRAIN_EVAC) {
      io.vu.prop.vmu.drain := Bool(true)

      when (!io.vu.report.mrt.pending) {
        hold_vu := Bool(false)
        hold_tlb := Bool(false)
        evac := Bool(false)
        
        state := NORMAL
      }
    }

    is (HOLD) {
      when (!io.rocc.hold) {
        hold_vu := Bool(false)
        hold_tlb := Bool(false)

        state := NORMAL
      }
    }

  }
}
