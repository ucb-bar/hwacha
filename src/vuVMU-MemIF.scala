package hwacha

import Chisel._
import Node._
import Constants._
import hardfloat._

class io_vmu_memif extends Bundle
{
  val vaq = Decoupled(new io_vpaq_bundle()).flip
  val vsdq = Decoupled(Bits(width = 65)).flip
  val vldq = Valid(new VLDQEnqBundle(65, LG_ENTRIES_VLDQ))
  val vldq_rtag = Decoupled(Bits(width = LG_ENTRIES_VLDQ)).flip

  val mem_req = new io_dmem_req()
  val mem_resp = new io_dmem_resp().flip

  val pending_replayq = Bool(OUTPUT)
}

class vuVMU_MemIF extends Module
{
  val io = new io_vmu_memif()

  val ex_pf_cmd = is_mcmd_pf(io.vaq.bits.cmd)
  val ex_load_cmd = is_mcmd_load(io.vaq.bits.cmd)
  val ex_store_cmd = is_mcmd_store(io.vaq.bits.cmd)
  val ex_amo_cmd = is_mcmd_amo(io.vaq.bits.cmd)

  val ex_pf_val = ex_pf_cmd && io.vaq.valid
  val ex_load_val = ex_load_cmd && io.vaq.valid && io.vldq_rtag.valid
  val ex_store_val = ex_store_cmd && io.vaq.valid && io.vsdq.valid
  val ex_amo_val = ex_amo_cmd && io.vaq.valid && io.vsdq.valid && io.vldq_rtag.valid

  val replaying_cmb = Bool()
  val replaying = Reg(next = replaying_cmb, init = Bool(false))
  replaying_cmb := replaying

  val replayq1 = Module(new Queue(new io_dmem_req_bundle, 1, flow = true))
  val replayq2 = Module(new Queue(new io_dmem_req_bundle, 1))
  val req_arb = Module(new Arbiter(new io_dmem_req_bundle, 2))

  req_arb.io.in(0) <> replayq1.io.deq
  req_arb.io.in(1).valid := !replaying_cmb && (ex_pf_val || ex_load_val || ex_store_val || ex_amo_val)
  req_arb.io.in(1).bits.cmd := io.vaq.bits.cmd
  req_arb.io.in(1).bits.typ := io.vaq.bits.typ
  req_arb.io.in(1).bits.addr := io.vaq.bits.addr.toUInt
  req_arb.io.in(1).bits.data := io.vsdq.bits // delayed one cycle in cpu
  req_arb.io.in(1).bits.tag := Cat(io.vldq_rtag.bits, io.vaq.bits.typ_float) // delayed one cycle in cpu

  io.vaq.ready :=
    !replaying_cmb && req_arb.io.in(1).ready && (
      ex_pf_cmd ||
      ex_load_cmd && io.vldq_rtag.valid ||
      ex_store_cmd && io.vsdq.valid ||
      ex_amo_cmd && io.vsdq.valid && io.vldq_rtag.valid
    )

  io.vsdq.ready :=
    !replaying_cmb && req_arb.io.in(1).ready && (
      ex_store_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vldq_rtag.valid
    )

  io.vldq_rtag.ready :=
    !replaying_cmb && req_arb.io.in(1).ready && (
      ex_load_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vsdq.valid
    )

  val s2_nack = io.mem_resp.bits.nack
  val s3_nack = Reg(next=s2_nack)

  val s0_req_fire = io.mem_req.fire()
  val s1_req_fire = Reg(next=s0_req_fire)
  val s2_req_fire = Reg(next=s1_req_fire)

  io.mem_req.bits.kill := s2_nack
  io.mem_req.bits.phys := Bool(true)
  io.mem_req <> req_arb.io.out

  // replay queues
  // replayq1 holds the older request
  // replayq2 holds the newer request (for the first nack)
  // we need to split the queues like this for the case where the older request
  // goes through but gets nacked, while the newer request stalls
  // if this happens, the newer request will go through before the older
  // request
  // we don't need to check replayq1.io.enq.ready and replayq2.io.enq.ready as
  // there will only be two requests going through at most

  // stash d$ request in stage 2 if nacked (older request)
  replayq1.io.enq.valid := Bool(false)
  replayq1.io.enq.bits.cmd := io.mem_resp.bits.cmd
  replayq1.io.enq.bits.typ := io.mem_resp.bits.typ
  replayq1.io.enq.bits.addr := io.mem_resp.bits.addr
  replayq1.io.enq.bits.data := io.mem_resp.bits.store_data
  replayq1.io.enq.bits.tag := io.mem_resp.bits.tag

  // stash d$ request in stage 1 if nacked (newer request)
  replayq2.io.enq.valid := s2_req_fire && s3_nack
  replayq2.io.enq.bits.data := io.mem_resp.bits.store_data
  replayq2.io.enq.bits <> io.mem_resp.bits
  replayq2.io.deq.ready := Bool(false)

  when (s2_nack) {
    replayq1.io.enq.valid := Bool(true)
    replaying_cmb := Bool(true)
  }

  // when replaying request got sunk into the d$
  when (s2_req_fire && Reg(next=Reg(next=replaying_cmb)) && !s2_nack) {
    // see if there's a stashed request in replayq2
    when (replayq2.io.deq.valid) {
      replayq1.io.enq.valid := Bool(true)
      replayq1.io.enq.bits.cmd := replayq2.io.deq.bits.cmd
      replayq1.io.enq.bits.typ := replayq2.io.deq.bits.typ
      replayq1.io.enq.bits.addr := replayq2.io.deq.bits.addr
      replayq1.io.enq.bits.data := replayq2.io.deq.bits.data
      replayq1.io.enq.bits.tag := replayq2.io.deq.bits.tag
      replayq2.io.deq.ready := Bool(true)
    } .otherwise {
      replaying_cmb := Bool(false)
    }
  }

  // being a little bit conservative here
  // doesn't depend on whether d$ request in stage 2 is nacked or not
  io.pending_replayq := s1_req_fire || s2_req_fire || replaying

  // a slightly faster version, however the following relies on replaying_cmb
  // io.pending_replayq := s1_req_fire || replaying_cmb

  // load data conversion
  val reg_mem_resp = Reg(next=io.mem_resp)
    
  val ldq_sp_bits = Bits(width=33)
  val ldq_dp_bits = Bits(width=65)
  
  val load_fp = reg_mem_resp.bits.tag(0)
  val load_fp_d = load_fp && reg_mem_resp.bits.typ === mtyp_D 
  val load_fp_w = load_fp && reg_mem_resp.bits.typ === mtyp_W

  val recode_sp = Module(new float32ToRecodedFloat32)
  recode_sp.io.in := reg_mem_resp.bits.data_subword(31,0)
  ldq_sp_bits := recode_sp.io.out
  
  val recode_dp = Module(new float64ToRecodedFloat64)
  recode_dp.io.in := reg_mem_resp.bits.data_subword
  ldq_dp_bits := recode_dp.io.out

  io.vldq.valid := reg_mem_resp.valid
  io.vldq.bits.data := MuxCase(
    Cat(Bits(0,1),reg_mem_resp.bits.data_subword(63,0)), Array(
	    (load_fp_d) -> ldq_dp_bits,
      (load_fp_w) -> Cat(Bits("hFFFFFFFF",32), ldq_sp_bits)
  ))
  io.vldq.bits.rtag := reg_mem_resp.bits.tag.toUInt >> UInt(1)
}
