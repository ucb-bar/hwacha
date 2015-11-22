package hwacha

import Chisel._
import cde.Parameters


class DecL2IO(implicit p: Parameters) extends HwachaBundle()(p) {
  // 64 is hardcoded in scalar-unit
  val addr = UInt(width = 64)
  val curr_vlen = UInt(width = bMLVLen)
  val opwidth = UInt(width=2) // byte = 0, half = 1, word = 2, double = 3
  val ls = UInt(width = 1) // load = 0, store = 1
}

class DecL2Q(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {
  val io = new Bundle {
    val enq = Decoupled(new DecL2IO).flip
    val deq = Decoupled(new DecL2IO)
  }
  
  io.deq <> Queue(io.enq, 10)
}


class ThrottleManager(skipamt: Int, resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {

  val throttleQueueDepth = 10
  val entrywidth = 10

  // TODO: should probably ideally be: (L2 size / vector length) * factor (like 1/2, 
  // to use 1/2 of the L2 for prefetching)
  val MAX_RUNAHEAD = 128 // # loads + stores we're allowed to run ahead

  val io = new Bundle {
    val enq = Decoupled(UInt(width=entrywidth)).flip
    val vf_done_vxu = Bool(INPUT)
    val stall_prefetch = Bool(OUTPUT)
  }

  val shim = Decoupled(UInt(width=entrywidth)).asDirectionless()

  // TODO counter overflow
  val runbehind_counter = Reg(init = SInt(-skipamt, width=32))

  printf("VRU: runbehind_counter: %d\n", runbehind_counter)

  // TODO: this can fall behind and lock things up
  //
  // When handling the fall-behind case, make sure to ignore the initial 
  // vector fetch blocks that we're skipping

  val ls_per_vf_q = Queue(shim, throttleQueueDepth)
  val global_ls_count = Reg(init=UInt(0, width=20))
  io.stall_prefetch := global_ls_count > UInt(MAX_RUNAHEAD)

  shim.valid := io.enq.valid && !(io.vf_done_vxu && !ls_per_vf_q.valid && runbehind_counter >= SInt(0)) && !(runbehind_counter > UInt(0))
  io.enq.ready := shim.ready
  shim.bits := io.enq.bits


  when (runbehind_counter > SInt(0) && !io.vf_done_vxu && io.enq.valid) {
    runbehind_counter := runbehind_counter - SInt(1)
  }

  printf("VRU: global_ls count %d\n", global_ls_count)

  // SO, we assume that if io.enq.ready  is not true, we are not 
  // accepting the value
  val increment_counter_necessary = shim.valid && shim.ready

  // TODO, ignoring the underflow case for now...
  // overflow is not possible
  val decrement_counter_necessary = ls_per_vf_q.valid && io.vf_done_vxu


  when (increment_counter_necessary) {
    printf("VRU: adding to q: %d\n", shim.bits)
  }

  when (decrement_counter_necessary) {
    printf("VRU: popping off: %d\n", ls_per_vf_q.bits)
  } .elsewhen (io.vf_done_vxu) {

    // here we handle the case where have a done message from vxu but we haven't seen the vf block yet

    // 1. we simultaneously have a valid input, just discard it
    // shim ready is &= with io.vf_done_vxu && !ls_per_vf_q.valid
    // handled above

    // 2. we don't, so increment runbehind_counter by one
    when (!io.enq.valid || runbehind_counter < SInt(0)) {
      runbehind_counter := runbehind_counter + SInt(1)
    }


    printf("VRU: NO QUEUE ENTRY PRESENT\n")
  }

  ls_per_vf_q.ready := io.vf_done_vxu

  when (increment_counter_necessary && decrement_counter_necessary) {
    global_ls_count := global_ls_count + shim.bits - ls_per_vf_q.bits
  } .elsewhen (increment_counter_necessary) {
    global_ls_count := global_ls_count + shim.bits
  } .elsewhen (decrement_counter_necessary) {
    global_ls_count := global_ls_count - ls_per_vf_q.bits
  }
}


class VRU(implicit p: Parameters) extends HwachaModule()(p)
  with MemParameters {
  import Commands._
  import uncore._

  // TODOs - use bundles to make types
  // style-wise, data flows right to left in <>

  // skip prefetching for the first skipamt VF blocks to get ahead 
  val skipamt = 2 

  val io = new Bundle {
    val toicache = new FrontendIO
    val cmdq = new CMDQIO().flip 
    val dmem = new ClientUncachedTileLinkIO
    val from_scalar_pop_message = Bool(INPUT)
  }

  // addr regfile
  val arf = Mem(UInt(width = 64), 32)

  // queue of load/store/addr/len to L2 prefetching stage
  val decl2q = Module(new DecL2Q)

  val vf_active = Reg(init=Bool(false)) 
  val decode_vmss    = io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa    = io.cmdq.cmd.bits === CMD_VMSA
  val decode_vsetcfg = io.cmdq.cmd.bits === CMD_VSETCFG
  val decode_vsetvl  = io.cmdq.cmd.bits === CMD_VSETVL
  val decode_vf      = io.cmdq.cmd.bits === CMD_VF
  val decode_vft     = io.cmdq.cmd.bits === CMD_VFT

  val deq_imm = decode_vmsa || decode_vf || decode_vft || decode_vsetvl || decode_vsetcfg
  val deq_rd  = decode_vmsa

  val mask_imm_valid = !deq_imm || io.cmdq.imm.valid
  val mask_rd_valid  = !deq_rd  || io.cmdq.rd.valid

  val vlen = Reg(init=UInt(0, bMLVLen))

  val throttleman = Module(new ThrottleManager(skipamt))

  def fire_cmdq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !vf_active,
      io.cmdq.cmd.valid,
      mask_imm_valid,
      mask_rd_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
 
  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready := fire_cmdq(mask_rd_valid, deq_rd)

  // should never get a vmss
  assert(!fire_cmdq(null, decode_vmss), "VRU should not receive VMSS")

  // handle vmsa
  when (fire_cmdq(null, decode_vmsa)) {
    printf("VRU: VMSA:\n")
    printf("VRU: CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("VRU: IMM: 0x%x\n", io.cmdq.imm.bits)
    printf("VRU: RD:  0x%x\n", io.cmdq.rd.bits)
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
  }

  // handle vsetcfg
  when (fire_cmdq(null, decode_vsetcfg)) {
    printf("VRU: VSETCFG:\n")
    printf("VRU: CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("VRU: IMM: 0x%x\n", io.cmdq.imm.bits)
  }

  // handle vsetvl
  when (fire_cmdq(null, decode_vsetvl)) {
    printf("VRU: VSETVL:\n")
    printf("VRU: Setting VL = 0x%x\n", io.cmdq.imm.bits)
    vlen := io.cmdq.imm.bits
  }

  // handle vf
  // skip prefetching the first two VF blocks so that we can get ahead
  // being close to the VXU when doing mem ops is bad
  val skipcount = Reg(init=UInt(0, width=4))

  val fire_vf = fire_cmdq(null, decode_vf)

  when (fire_vf) {
    printf("VRU: VF:\n")
    printf("VRU: CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("VRU: IMM: 0x%x\n", io.cmdq.imm.bits)
    when (skipcount < UInt(skipamt)) {
      printf("SKIPPING at startup %d\n", skipcount)
      skipcount := skipcount + UInt(1)
    } .otherwise {
      vf_active := Bool(true)
    }
  }

  // do a fetch 
  io.toicache.req.valid := fire_vf
  io.toicache.req.bits.pc := io.cmdq.imm.bits
  io.toicache.active := vf_active 
  io.toicache.invalidate := Bool(false)
  io.toicache.resp.ready := Bool(true) // for now...

  throttleman.io.vf_done_vxu := io.from_scalar_pop_message
  throttleman.io.enq.valid := Bool(false)
  val current_ls_count = Reg(init=UInt(0, width=throttleman.entrywidth))
  throttleman.io.enq.bits := current_ls_count

  val loaded_inst = io.toicache.resp.bits.data(0)
  decl2q.io.enq.valid := Bool(false)
  decl2q.io.enq.bits.addr := arf(loaded_inst(28, 24))
  decl2q.io.enq.bits.curr_vlen := vlen
  decl2q.io.enq.bits.opwidth := UInt(0) // set in when
  decl2q.io.enq.bits.ls := UInt(0) // set in when

  val wait_to_queue = Reg(init=Bool(false))

  when (decl2q.io.enq.valid && !decl2q.io.enq.ready) {
    printf("VRU: internal queue overflow\n")
  }

  when (vf_active && wait_to_queue) {
    // we're stalling waiting to enqueue information about the vf block we 
    // just processed
    printf("VRU: STALLING ON AMT Q\n")
    throttleman.io.enq.valid := Bool(true) && !throttleman.io.stall_prefetch
    when (throttleman.io.enq.ready && !throttleman.io.stall_prefetch) {
      vf_active := Bool(false)
      wait_to_queue := Bool(false)
      current_ls_count := UInt(0)
    }
  }

  // hacky match against insts
  // TODO: do this right with IntCtrlSigs
  when (io.toicache.resp.valid && vf_active && !wait_to_queue) {
    printf("VRU: INST PC  recv'd: 0x%x\n", io.toicache.resp.bits.pc)
    printf("VRU: INST VAL recv'd: 0x%x\n", loaded_inst)

    when (loaded_inst === HwachaElementInstructions.VSTOP) {
      printf("VRU: REACHED END OF VF BLOCK\n")

      throttleman.io.enq.valid := Bool(true) && !throttleman.io.stall_prefetch
      when (throttleman.io.enq.ready && !throttleman.io.stall_prefetch) {
        vf_active := Bool(false)
        current_ls_count := UInt(0)
      } .otherwise {
        wait_to_queue := Bool(true)
      }
    }

    when (loaded_inst === HwachaElementInstructions.VLD) {
      printf("VRU: GOT VLD.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d double elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)

      decl2q.io.enq.bits.opwidth := UInt(3)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)
    }

    when (loaded_inst === HwachaElementInstructions.VSD) {
      printf("VRU: GOT VSD.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d double elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)

      decl2q.io.enq.bits.opwidth := UInt(3)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VLW || 
          loaded_inst === HwachaElementInstructions.VLWU) {
      printf("VRU: GOT VLW/VLWU.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d single elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(2)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VSW) {
      printf("VRU: GOT VSW.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d single elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(2)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VLH || 
          loaded_inst === HwachaElementInstructions.VLHU) {
      printf("VRU: GOT VLH/VLHU.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d half elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(1)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VSH) {
      printf("VRU: GOT VSH.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d half elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(1)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VLB || 
          loaded_inst === HwachaElementInstructions.VLBU) {
      printf("VRU: GOT VLB/VLBU.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d byte elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(0)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

    when (loaded_inst === HwachaElementInstructions.VSB) {
      printf("VRU: GOT VSB.\n")
      printf("VRU: Will issue prefetch at arf(0x%x) = 0x%x of\n%d byte elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(0)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
      current_ls_count := current_ls_count + UInt(1)

    }

  }


  // TODO cleanup with FIREs

  // queue. no backpressure yet...
  val tag_count = Reg(init = UInt(0, tlClientXactIdBits))

  val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits
  val req_addr = decl2q.io.deq.bits.addr(bPAddr-1, tlBlockAddrOffset)
  val req_vlen = decl2q.io.deq.bits.curr_vlen // vector len
  val req_opwidth = decl2q.io.deq.bits.opwidth // byte = 0 ... double = 3
  val req_ls = decl2q.io.deq.bits.ls // load = 0, store = 1
  val prefetch_ip = Reg(init=Bool(false))

  val vec_len_bytes = req_vlen << req_opwidth
  val num_blocks_pf = vec_len_bytes >> tlBlockAddrOffset

  // TODO: calculate width
  val pf_ip_counter = Reg(init = UInt(0, width=20))

  val throttleAmt = p(HwachaVRUThrottle) // approximately 1/3 of the units
  printf("VRU THROTTLE AMT: %d\n", UInt(throttleAmt))
  val throttle = Reg(init=UInt(throttleAmt, width=6))

  assert(throttle <= UInt(throttleAmt), "VRU: THROTTLE TOO LARGE\n")

  // TODO
  val vf_throttle = throttleman.io.stall_prefetch

  io.dmem.acquire.bits := Mux(req_ls === UInt(0), GetPrefetch(tag_count, req_addr+pf_ip_counter), PutPrefetch(tag_count, req_addr+pf_ip_counter))

  io.dmem.acquire.valid := Bool(false)
  decl2q.io.deq.ready := Bool(false)

  when (decl2q.io.deq.valid && !prefetch_ip && io.dmem.acquire.ready) {
    prefetch_ip := Bool(true)
    pf_ip_counter := UInt(0)
  }

  val movecond1 = prefetch_ip && pf_ip_counter < (num_blocks_pf-UInt(1)) && io.dmem.acquire.ready && throttle > UInt(0) && !vf_throttle


  when (movecond1) {
    printf("VRU: SENDING PREFETCH TO L2\n")
    printf("VRU: addr 0x%x tag 0x%x\n", req_addr + pf_ip_counter, tag_count)
    printf("VRU: from 0x%x\n", decl2q.io.deq.bits.addr)

    pf_ip_counter := pf_ip_counter + UInt(1)
    tag_count := tag_count + UInt(1)
    when (!io.dmem.grant.valid) {
      throttle := throttle - UInt(1)
    }
  }

  val movecond2 = prefetch_ip && pf_ip_counter === (num_blocks_pf - UInt(1)) && io.dmem.acquire.ready && throttle > UInt(0) && !vf_throttle

  when (movecond2) {
    printf("VRU: SENDING PREFETCH TO L2\n")
    printf("VRU: addr 0x%x tag 0x%x\n", req_addr + pf_ip_counter, tag_count)
    printf("VRU: from 0x%x\n", decl2q.io.deq.bits.addr)

    pf_ip_counter := UInt(0)
    prefetch_ip := Bool(false)
    tag_count := tag_count + UInt(1)
    decl2q.io.deq.ready := Bool(true)
    when (!io.dmem.grant.valid) {
      throttle := throttle - UInt(1)
    }
  }

  when (vf_throttle) {
    printf("VRU: ran too far ahead. waiting...\n")
  }


  // do not put acquire.ready in this, it's not necessary
  io.dmem.acquire.valid := prefetch_ip && throttle > UInt(0) && !vf_throttle
  io.dmem.grant.ready := Bool(true)
  when (io.dmem.grant.valid) {
    when (!(movecond1 || movecond2)) {
      throttle := throttle + UInt(1)
    }
    printf("VRU: PREFETCH ACK id 0x%x\n", io.dmem.grant.bits.client_xact_id)
  }
}
