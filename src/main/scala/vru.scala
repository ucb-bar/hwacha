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
  
  io.deq <> Queue(io.enq, 5)
}

class VRU(implicit p: Parameters) extends HwachaModule()(p)
  with MemParameters {
  import Commands._
  import uncore._

  // TODOs - use bundles to make types
  // style-wise, data flows right to left in <>

  val io = new Bundle {
    val toicache = new FrontendIO
    val cmdq = new CMDQIO().flip 
    val dmem = new ClientUncachedTileLinkIO
  }

  // addr regfile
  val arf = Mem(UInt(width = 64), 32)

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
    printf("VMSA:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
    printf("RD:  0x%x\n", io.cmdq.rd.bits)
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
  }

  // handle vsetcfg
  when (fire_cmdq(null, decode_vsetcfg)) {
    printf("VSETCFG:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
  }

  // handle vsetvl
  when (fire_cmdq(null, decode_vsetvl)) {
    printf("VSETVL:\n")
    printf("Setting VL = 0x%x\n", io.cmdq.imm.bits)
    vlen := io.cmdq.imm.bits
  }

  // handle vf

  val fire_vf = fire_cmdq(null, decode_vf)

  when (fire_vf) {
    printf("VF:\n")
    printf("CMD: 0x%x\n", io.cmdq.cmd.bits)
    printf("IMM: 0x%x\n", io.cmdq.imm.bits)
    vf_active := Bool(true)
  }

  // do a fetch 
  io.toicache.req.valid := fire_vf
  io.toicache.req.bits.pc := io.cmdq.imm.bits
  io.toicache.active := vf_active 
  io.toicache.invalidate := Bool(false)
  io.toicache.resp.ready := Bool(true) // for now...

  val loaded_inst = io.toicache.resp.bits.data(0)
  decl2q.io.enq.valid := Bool(false)
  decl2q.io.enq.bits.addr := arf(loaded_inst(28, 24))
  decl2q.io.enq.bits.curr_vlen := vlen
  decl2q.io.enq.bits.opwidth := UInt(0) // set in when
  decl2q.io.enq.bits.ls := UInt(0) // set in when

  // hacky match against insts
  // TODO: do this right with IntCtrlSigs
  when (io.toicache.resp.valid && vf_active) {
    printf("INST PC  recv'd: 0x%x\n", io.toicache.resp.bits.pc)
    printf("INST VAL recv'd: 0x%x\n", loaded_inst)

    when (loaded_inst === HwachaElementInstructions.VSTOP) {
      vf_active := Bool(false)
      printf("REACHED END OF VF BLOCK\n")
    }

    when (loaded_inst === HwachaElementInstructions.VLD) {
      printf("GOT VLD.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d double elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)

      decl2q.io.enq.bits.opwidth := UInt(3)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VSD) {
      printf("GOT VSD.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d double elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)

      decl2q.io.enq.bits.opwidth := UInt(3)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VLW || 
          loaded_inst === HwachaElementInstructions.VLWU) {
      printf("GOT VLW/VLWU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d single elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(2)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VSW) {
      printf("GOT VSW.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d single elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(2)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VLH || 
          loaded_inst === HwachaElementInstructions.VLHU) {
      printf("GOT VLH/VLHU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d half elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(1)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VSH) {
      printf("GOT VSH.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d half elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(1)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VLB || 
          loaded_inst === HwachaElementInstructions.VLBU) {
      printf("GOT VLB/VLBU.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d byte elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(0)
      decl2q.io.enq.bits.ls := UInt(0)
      decl2q.io.enq.valid := Bool(true)
    }

    when (loaded_inst === HwachaElementInstructions.VSB) {
      printf("GOT VSB.\n")
      printf("Will issue prefetch at arf(0x%x) = 0x%x of\n%d byte elements.\n", 
        loaded_inst(28, 24), arf(loaded_inst(28, 24)), vlen)
      decl2q.io.enq.bits.opwidth := UInt(0)
      decl2q.io.enq.bits.ls := UInt(1)
      decl2q.io.enq.valid := Bool(true)
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

  val throttleAmt = 1
  val throttle = Reg(init=UInt(throttleAmt, width=4))

  assert(throttle <= UInt(throttleAmt), "THROTTLE TOO LARGE\n")

//  io.dmem.acquire.bits := Get(tag_count, req_addr+pf_ip_counter, UInt(0))
  io.dmem.acquire.bits := GetPrefetch(tag_count, req_addr+pf_ip_counter)

  io.dmem.acquire.valid := Bool(false)
  decl2q.io.deq.ready := Bool(false)

  when (decl2q.io.deq.valid && !prefetch_ip && io.dmem.acquire.ready) {
    prefetch_ip := Bool(true)
    pf_ip_counter := UInt(0)
  }

  val movecond1 = prefetch_ip && pf_ip_counter < (num_blocks_pf-UInt(1)) && io.dmem.acquire.ready && throttle > UInt(0)


  when (movecond1) {
    printf("SENDING PREFETCH TO L2\n")
    printf("addr 0x%x tag 0x%x\n", req_addr + pf_ip_counter, tag_count)
    printf("from 0x%x\n", decl2q.io.deq.bits.addr)

    pf_ip_counter := pf_ip_counter + UInt(1)
    tag_count := tag_count + UInt(1)
    when (!io.dmem.grant.valid) {
      throttle := throttle - UInt(1)
    }
  }

  val movecond2 = prefetch_ip && pf_ip_counter === (num_blocks_pf - UInt(1)) && io.dmem.acquire.ready && throttle > UInt(0)

  when (movecond2) {
    printf("SENDING PREFETCH TO L2\n")
    printf("addr 0x%x tag 0x%x\n", req_addr + pf_ip_counter, tag_count)
    printf("from 0x%x\n", decl2q.io.deq.bits.addr)

    pf_ip_counter := UInt(0)
    prefetch_ip := Bool(false)
    tag_count := tag_count + UInt(1)
    decl2q.io.deq.ready := Bool(true)
    when (!io.dmem.grant.valid) {
      throttle := throttle - UInt(1)
    }
  }

  // do not put acquire.ready in this, it's not necessary
  io.dmem.acquire.valid := prefetch_ip && throttle > UInt(0)
  io.dmem.grant.ready := Bool(true)
  when (io.dmem.grant.valid) {
    when (!(movecond1 || movecond2)) {
      throttle := throttle + UInt(1)
    }
    printf("PREFETCH ACK id 0x%x\n", io.dmem.grant.bits.client_xact_id)
  }
}
