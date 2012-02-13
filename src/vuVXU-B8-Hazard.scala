package hwacha

import Chisel._
import Node._
import Config._

class vuVXU_Banked8_Hazard extends Component
{
  val io = new io_vxu_hazard()

  val reg_ptr = Reg(resetVal = UFix(0,SZ_LGBANK))

  val next_ptr1_add = reg_ptr + UFix(1, SZ_LGBANK1)
  val next_ptr2_add = reg_ptr + UFix(2, SZ_LGBANK1)
  val next_ptr3_add = reg_ptr + UFix(3, SZ_LGBANK1)
  val next_ptr4_add = reg_ptr + UFix(4, SZ_LGBANK1)

  val next_ptr1_add_bcnt = next_ptr1_add - io.issue_to_hazard.bcnt
  val next_ptr2_add_bcnt = next_ptr2_add - io.issue_to_hazard.bcnt
  val next_ptr3_add_bcnt = next_ptr3_add - io.issue_to_hazard.bcnt
  val next_ptr4_add_bcnt = next_ptr4_add - io.issue_to_hazard.bcnt

  val next_ptr4_add_bcnt2 = next_ptr4_add_bcnt - io.issue_to_hazard.bcnt

  val next_ptr1 = Mux(
    next_ptr1_add < io.issue_to_hazard.bcnt, next_ptr1_add(SZ_LGBANK-1,0),
    next_ptr1_add_bcnt(SZ_LGBANK-1,0))

  val next_ptr2 = Mux(
    next_ptr2_add < io.issue_to_hazard.bcnt, next_ptr2_add(SZ_LGBANK-1,0),
    next_ptr2_add_bcnt(SZ_LGBANK-1,0))

  val next_ptr3 = Mux(
    next_ptr3_add < io.issue_to_hazard.bcnt, next_ptr3_add(SZ_LGBANK-1,0),
    next_ptr3_add_bcnt(SZ_LGBANK-1,0))

  val next_ptr4 = Mux(
    next_ptr4_add < io.issue_to_hazard.bcnt, next_ptr4_add(SZ_LGBANK-1,0), Mux(
    next_ptr4_add_bcnt < io.issue_to_hazard.bcnt, next_ptr4_add_bcnt(SZ_LGBANK-1,0),
    next_ptr4_add_bcnt2(SZ_LGBANK-1,0)))

  reg_ptr := next_ptr1

  val array_rport_val = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_rport_vau0 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_rport_vau1 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_rport_vau2 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_rport_vsu = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_rport_vgu = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }

  val next_rport_val = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_rport_vau0 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_rport_vau1 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_rport_vau2 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_rport_vsu = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_rport_vgu = GenArray(SZ_BANK){ Wire(){ Bool() } }

  for (i <- 0 until SZ_BANK)
  {
    array_rport_val(i) := next_rport_val(i)
    array_rport_vau0(i) := next_rport_vau0(i)
    array_rport_vau1(i) := next_rport_vau1(i)
    array_rport_vau2(i) := next_rport_vau2(i)
    array_rport_vsu(i) := next_rport_vsu(i)
    array_rport_vgu(i) := next_rport_vgu(i)
  }

  for (i <- 0 until SZ_BANK)
  {
    next_rport_val(i) <== array_rport_val(i)
    next_rport_vau0(i) <== array_rport_vau0(i)
    next_rport_vau1(i) <== array_rport_vau1(i)
    next_rport_vau2(i) <== array_rport_vau2(i)
    next_rport_vsu(i) <== array_rport_vsu(i)
    next_rport_vgu(i) <== array_rport_vgu(i)
  }

  when (io.fire.viu)
  {
    next_rport_val.write(next_ptr2, Bool(true))

    when (io.fire_fn.viu(RG_VIU_T) === Cat(ML,MR))
    {
      next_rport_val.write(next_ptr3, Bool(true))
    }
  }
  when (io.fire.vau0)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vau0.write(next_ptr2, Bool(true))

    next_rport_val.write(next_ptr3, Bool(true))
    next_rport_vau0.write(next_ptr3, Bool(true))
  }
  when (io.fire.vau1)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vau1.write(next_ptr2, Bool(true))

    next_rport_val.write(next_ptr3, Bool(true))
    next_rport_vau1.write(next_ptr3, Bool(true))

    when (io.fire_fn.vau1(2).toBool)
    {
      next_rport_val.write(next_ptr4, Bool(true))
      next_rport_vau1.write(next_ptr4, Bool(true))
    }
  }
  when (io.fire.vau2)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vau2.write(next_ptr2, Bool(true))
  }
  when (io.fire.amo || io.fire.utst)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vgu.write(next_ptr2, Bool(true))

    next_rport_val.write(next_ptr3, Bool(true))
    next_rport_vsu.write(next_ptr3, Bool(true))
  }
  when (io.fire.utld)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vgu.write(next_ptr2, Bool(true))
  }
  when (io.fire.vst)
  {
    next_rport_val.write(next_ptr2, Bool(true))
    next_rport_vsu.write(next_ptr2, Bool(true))
  }

  when (io.lane_to_hazard.rlast)
  {
    next_rport_val.write(reg_ptr, Bool(false))
    next_rport_vau0.write(reg_ptr, Bool(false))
    next_rport_vau1.write(reg_ptr, Bool(false))
    next_rport_vau2.write(reg_ptr, Bool(false))
    next_rport_vsu.write(reg_ptr, Bool(false))
    next_rport_vgu.write(reg_ptr, Bool(false))
  }

  // I had to change this structure to the following in order to cut the
  // critical path.  I'm precomputing all possible write pointers and then
  // muxing them in later.

  // this delay comes from seq/expand

  val DELAY = 2

  // tvec wptr calculation

  val tvec_viu_incr = UFix(INT_STAGES,SZ_LGBANK+1) + UFix(1, SZ_LGBANK) + UFix(DELAY, SZ_LGBANK)

  val tvec_viuwptr = new vuVXU_Pointer()

  tvec_viuwptr.io.ptr := reg_ptr
  tvec_viuwptr.io.incr := tvec_viu_incr
  tvec_viuwptr.io.bcnt <> io.issue_to_hazard.bcnt

  val tvec_viu_wptr1 = tvec_viuwptr.io.nptr

  val tvec_viu_wptr1_add = tvec_viu_wptr1 + UFix(1, SZ_LGBANK1)
  val tvec_viu_wptr1_add_sub = tvec_viu_wptr1_add - io.issue_to_hazard.bcnt

  val tvec_viu_wptr2 = Mux(
    tvec_viu_wptr1_add < io.issue_to_hazard.bcnt, tvec_viu_wptr1_add(SZ_LGBANK-1,0),
    tvec_viu_wptr1_add_sub(SZ_LGBANK-1,0))

  val tvec_viu_wptr = MuxCase(
    Bits(0,SZ_LGBANK), Array(
      (io.tvec_valid.viu && io.tvec_fn.viu(RG_VIU_T) === Cat(ML,MR)) -> tvec_viu_wptr2,
      io.tvec_valid.viu -> tvec_viu_wptr1
    ))

  // vt wptr calculation

  val vt_viu_incr = UFix(INT_STAGES,SZ_LGBANK+1) + UFix(1, SZ_LGBANK) + UFix(DELAY, SZ_LGBANK)
  val vt_vau0_incr = UFix(IMUL_STAGES,SZ_LGBANK+1) + UFix(2, SZ_LGBANK) + UFix(DELAY, SZ_LGBANK)
  val vt_vau1_incr = UFix(FMA_STAGES,SZ_LGBANK+1) + UFix(2, SZ_LGBANK) + UFix(DELAY, SZ_LGBANK)
  val vt_vau2_incr = UFix(FCONV_STAGES,SZ_LGBANK+1) + UFix(1, SZ_LGBANK) + UFix(DELAY, SZ_LGBANK)

  val vt_viuwptr = new vuVXU_Pointer()
  val vt_vau0wptr = new vuVXU_Pointer()
  val vt_vau1wptr = new vuVXU_Pointer()
  val vt_vau2wptr = new vuVXU_Pointer()

  vt_viuwptr.io.ptr := reg_ptr
  vt_viuwptr.io.incr := vt_viu_incr
  vt_viuwptr.io.bcnt <> io.issue_to_hazard.bcnt

  vt_vau0wptr.io.ptr := reg_ptr
  vt_vau0wptr.io.incr := vt_vau0_incr
  vt_vau0wptr.io.bcnt <> io.issue_to_hazard.bcnt

  vt_vau1wptr.io.ptr := reg_ptr
  vt_vau1wptr.io.incr := vt_vau1_incr
  vt_vau1wptr.io.bcnt <> io.issue_to_hazard.bcnt

  vt_vau2wptr.io.ptr := reg_ptr
  vt_vau2wptr.io.incr := vt_vau2_incr
  vt_vau2wptr.io.bcnt <> io.issue_to_hazard.bcnt

  val vt_viu_wptr1 = vt_viuwptr.io.nptr
  val vt_vau0_wptr = vt_vau0wptr.io.nptr
  val vt_vau1_wptr2 = vt_vau1wptr.io.nptr
  val vt_vau2_wptr = vt_vau2wptr.io.nptr

  val vt_viu_wptr1_add = vt_viu_wptr1 + UFix(1, SZ_LGBANK1)
  val vt_viu_wptr1_add_sub = vt_viu_wptr1_add - io.issue_to_hazard.bcnt

  val vt_vau1_wptr2_add = vt_vau1_wptr2 + UFix(1, SZ_LGBANK1)
  val vt_vau1_wptr2_add_sub = vt_vau1_wptr2_add - io.issue_to_hazard.bcnt

  val vt_viu_wptr2 = Mux(
    vt_viu_wptr1_add < io.issue_to_hazard.bcnt, vt_viu_wptr1_add(SZ_LGBANK-1,0),
    vt_viu_wptr1_add_sub(SZ_LGBANK-1,0))

  val vt_vau1_wptr3 = Mux(
    vt_vau1_wptr2_add < io.issue_to_hazard.bcnt, vt_vau1_wptr2_add(SZ_LGBANK-1,0),
    vt_vau1_wptr2_add_sub(SZ_LGBANK-1,0))

  val vt_viu_wptr = MuxCase(
    Bits(0,SZ_LGBANK), Array(
      (io.vt_valid.viu && io.vt_fn.viu(RG_VIU_T) === Cat(ML,MR)) -> vt_viu_wptr2,
      io.vt_valid.viu -> vt_viu_wptr1
    ))

  val vt_vau1_wptr = MuxCase(
    Bits(0,SZ_LGBANK), Array(
      (io.vt_valid.vau1 && io.vt_fn.vau1(2) === Bits(1,1)) -> vt_vau1_wptr3,
      io.vt_valid.vau1 -> vt_vau1_wptr2
    ))

  val vt_wptr = MuxCase(
    Bits(0,SZ_LGBANK), Array(
      io.vt_valid.viu -> vt_viu_wptr,
      io.vt_valid.vau0 -> vt_vau0_wptr,
      io.vt_valid.vau1 -> vt_vau1_wptr,
      io.vt_valid.vau2 -> vt_vau2_wptr
    ))

  // for the fire port
  // we can look at the issue port because we're firing at the same cycle

  val viu_wptr = Mux(io.tvec_valid.viu, tvec_viu_wptr, vt_viu_wptr)
  val vau0_wptr = vt_vau0_wptr
  val vau1_wptr = vt_vau1_wptr
  val vau2_wptr = vt_vau2_wptr

  val array_wport_val = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_head = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_vau0 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_vau1 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_vau2 = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_vlu = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_wport_vd = GenArray(SZ_BANK){ Reg(){ Bits(width = SZ_REGLEN) } }

  val next_wport_val = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_head = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_vau0 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_vau1 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_vau2 = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_vlu = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_wport_vd = GenArray(SZ_BANK){ Wire(){ Bits(width = SZ_REGLEN) } }

  for (i <- 0 until SZ_BANK)
  {
    array_wport_val(i) := next_wport_val(i)
    array_wport_head(i) := next_wport_head(i)
    array_wport_vau0(i) := next_wport_vau0(i)
    array_wport_vau1(i) := next_wport_vau1(i)
    array_wport_vau2(i) := next_wport_vau2(i)
    array_wport_vlu(i) := next_wport_vlu(i)
    array_wport_vd(i) := next_wport_vd(i)
  }

  for (i <- 0 until SZ_BANK)
  {
    next_wport_val(i) <== array_wport_val(i)
    next_wport_head(i) <== array_wport_head(i)
    next_wport_vau0(i) <== array_wport_vau0(i)
    next_wport_vau1(i) <== array_wport_vau1(i)
    next_wport_vau2(i) <== array_wport_vau2(i)
    next_wport_vlu(i) <== array_wport_vlu(i)
    next_wport_vd(i) <== array_wport_vd(i)
  }

  when (io.fire.viu)
  {
    next_wport_val.write(viu_wptr, Bool(true))
    next_wport_head.write(viu_wptr, Bool(true))
    next_wport_vd.write(viu_wptr, io.fire_regid_imm.vd)
  }
  when (io.fire.vau0)
  {
    next_wport_val.write(vau0_wptr, Bool(true))
    next_wport_head.write(vau0_wptr, Bool(true))
    next_wport_vau0.write(vau0_wptr, Bool(true))
    next_wport_vd.write(vau0_wptr, io.fire_regid_imm.vd)
  }
  when (io.fire.vau1)
  {
    next_wport_val.write(vau1_wptr, Bool(true))
    next_wport_head.write(vau1_wptr, Bool(true))
    next_wport_vau1.write(vau1_wptr, Bool(true))
    next_wport_vd.write(vau1_wptr, io.fire_regid_imm.vd)
  }
  when (io.fire.vau2)
  {
    next_wport_val.write(vau2_wptr, Bool(true))
    next_wport_head.write(vau2_wptr, Bool(true))
    next_wport_vau2.write(vau2_wptr, Bool(true))
    next_wport_vd.write(vau2_wptr, io.fire_regid_imm.vd)
  }
  when (io.fire.amo)
  {
    next_wport_val.write(next_ptr4, Bool(true))
    next_wport_head.write(next_ptr4, Bool(true))
    next_wport_vlu.write(next_ptr4, Bool(true))
    next_wport_vd.write(next_ptr4, io.fire_regid_imm.vd)
  }
  when (io.fire.utld)
  {
    next_wport_val.write(next_ptr3, Bool(true))
    next_wport_head.write(next_ptr3, Bool(true))
    next_wport_vlu.write(next_ptr3, Bool(true))
    next_wport_vd.write(next_ptr3, io.fire_regid_imm.vd)
  }
  when (io.fire.vld)
  {
    next_wport_val.write(next_ptr3, Bool(true))
    next_wport_head.write(next_ptr3, Bool(true))
    next_wport_vlu.write(next_ptr3, Bool(true))
    next_wport_vd.write(next_ptr3, io.fire_regid_imm.vd)
  }

  when (io.expand_to_hazard.wen)
  {
    next_wport_head.write(reg_ptr, Bool(false))
  }

  when (io.lane_to_hazard.wlast)
  {
    next_wport_val.write(reg_ptr, Bool(false))
    next_wport_vau0.write(reg_ptr, Bool(false))
    next_wport_vau1.write(reg_ptr, Bool(false))
    next_wport_vau2.write(reg_ptr, Bool(false))
    next_wport_vlu.write(reg_ptr, Bool(false))
  }

  val array_sport_val = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }

  val next_sport_val = GenArray(SZ_BANK){ Wire(){ Bool() } }

  for (i <- 0 until SZ_BANK)
  {
    array_sport_val(i) := next_sport_val(i)
  }

  for (i <- 0 until SZ_BANK)
  {
    next_sport_val(i) <== array_sport_val(i)
  }

  when (io.fire.viu || io.fire.vau0 || io.fire.vau1 || io.fire.vau2)
  {
    next_sport_val.write(next_ptr1, Bool(true))
  }
  when (io.fire.amo)
  {
    next_sport_val.write(next_ptr1, Bool(true))
    next_sport_val.write(next_ptr2, Bool(true))
    next_sport_val.write(next_ptr3, Bool(true))
  }
  when (io.fire.utld || io.fire.utst)
  {
    next_sport_val.write(next_ptr1, Bool(true))
    next_sport_val.write(next_ptr2, Bool(true))
  }
  when(io.fire.vld)
  {
    next_sport_val.write(next_ptr1, Bool(true))
    next_sport_val.write(next_ptr2, Bool(true))
  }
  when (io.fire.vst)
  {
    next_sport_val.write(next_ptr1, Bool(true))
  }
  
  when (io.seq_to_hazard.last)
  {
    next_sport_val.write(reg_ptr, Bool(false))
  }

  // hazard check logic for tvec/vt
  val shazard_vau0 = (array_rport_val.flatten() | array_rport_vau0.flatten()).orR() | (array_wport_val.flatten() | array_wport_vau0.flatten()).orR()
  val shazard_vau1 = (array_rport_val.flatten() | array_rport_vau1.flatten()).orR() | (array_wport_val.flatten() | array_wport_vau1.flatten()).orR()
  val shazard_vau2 = (array_rport_val.flatten() | array_rport_vau2.flatten()).orR() | (array_wport_val.flatten() | array_wport_vau2.flatten()).orR()
  val shazard_vgu = (array_rport_val.flatten() | array_rport_vgu.flatten()).orR()
  val shazard_vlu = (array_wport_val.flatten() | array_wport_vlu.flatten()).orR()
  val shazard_vsu = (array_rport_val.flatten() | array_rport_vsu.flatten()).orR()

  val seqhazard_1slot = array_sport_val.read(next_ptr1)
  val seqhazard_2slot = array_sport_val.read(next_ptr1) | array_sport_val.read(next_ptr2)
  val seqhazard_3slot = array_sport_val.read(next_ptr1) | array_sport_val.read(next_ptr2) | array_sport_val.read(next_ptr3)

  // hazard check logic for tvec
  val tvec_comp_vt =
    Cat(
      io.tvec_regid_imm.vt === array_wport_vd(7),
      io.tvec_regid_imm.vt === array_wport_vd(6),
      io.tvec_regid_imm.vt === array_wport_vd(5),
      io.tvec_regid_imm.vt === array_wport_vd(4),
      io.tvec_regid_imm.vt === array_wport_vd(3),
      io.tvec_regid_imm.vt === array_wport_vd(2),
      io.tvec_regid_imm.vt === array_wport_vd(1),
      io.tvec_regid_imm.vt === array_wport_vd(0)
    )

  val tvec_comp_vd =
    Cat(
      io.tvec_regid_imm.vd === array_wport_vd(7),
      io.tvec_regid_imm.vd === array_wport_vd(6),
      io.tvec_regid_imm.vd === array_wport_vd(5),
      io.tvec_regid_imm.vd === array_wport_vd(4),
      io.tvec_regid_imm.vd === array_wport_vd(3),
      io.tvec_regid_imm.vd === array_wport_vd(2),
      io.tvec_regid_imm.vd === array_wport_vd(1),
      io.tvec_regid_imm.vd === array_wport_vd(0)
    )

  val tvec_dhazard_vt = (array_wport_val.flatten() & array_wport_head.flatten() & tvec_comp_vt).orR()
  val tvec_dhazard_vd = (array_wport_val.flatten() & array_wport_head.flatten() & tvec_comp_vd).orR()

  val tvec_bhazard_r1w1 = array_rport_val.read(next_ptr2) | array_wport_val.read(tvec_viu_wptr)
  val tvec_bhazard_vld = array_wport_val.read(next_ptr2)
  val tvec_bhazard_vst = array_rport_val.read(next_ptr2)

  val tvec_stall =
    Cat(
      io.tvec_valid.viu & io.seq_to_hazard.stall.orR(),
      io.tvec_valid.vld & (io.seq_to_hazard.stall(RG_VAQ) | io.seq_to_hazard.stall(RG_VSDQ) | io.seq_to_hazard.stall(RG_UTAQ) | io.seq_to_hazard.stall(RG_UTLDQ) | io.seq_to_hazard.stall(RG_UTSDQ)),
      io.tvec_valid.vst & (io.seq_to_hazard.stall(RG_VAQ) | io.seq_to_hazard.stall(RG_VLDQ) | io.seq_to_hazard.stall(RG_UTAQ) | io.seq_to_hazard.stall(RG_UTLDQ) | io.seq_to_hazard.stall(RG_UTSDQ))
    )

  val tvec_dhazard =
    Cat(
      ~io.tvec_regid_imm.vt_zero & tvec_dhazard_vt & io.tvec_dhazard.vt,
      tvec_dhazard_vd & io.tvec_dhazard.vd
    )

  val tvec_shazard =
    Cat(
      shazard_vlu & io.tvec_shazard.vlu,
      shazard_vsu & io.tvec_shazard.vsu
    )

  val tvec_seqhazard =
    Cat(
      io.tvec_valid.viu & seqhazard_1slot,
      io.tvec_valid.vld & seqhazard_2slot,
      io.tvec_valid.vst & seqhazard_1slot
    )

  val tvec_bhazard =
    Cat(
      tvec_bhazard_r1w1 & io.tvec_bhazard.r1w1,
      tvec_bhazard_vld & io.tvec_bhazard.vld,
      tvec_bhazard_vst & io.tvec_bhazard.vst
    )

  io.no_pending_ldsd := !array_rport_vsu.flatten().orR() && !array_rport_vgu.flatten().orR() && !array_wport_vlu.flatten().orR()

  io.tvec_ready := io.tvec_regid_imm.vd_zero | !tvec_stall.orR() & !tvec_dhazard.orR() & !tvec_shazard.orR() & !tvec_seqhazard.orR() & !tvec_bhazard.orR()

  // hazard check logic for vt
  val vt_comp_vs =
    Cat(
      io.vt_regid_imm.vs === array_wport_vd(7),
      io.vt_regid_imm.vs === array_wport_vd(6),
      io.vt_regid_imm.vs === array_wport_vd(5),
      io.vt_regid_imm.vs === array_wport_vd(4),
      io.vt_regid_imm.vs === array_wport_vd(3),
      io.vt_regid_imm.vs === array_wport_vd(2),
      io.vt_regid_imm.vs === array_wport_vd(1),
      io.vt_regid_imm.vs === array_wport_vd(0)
    )

  val vt_comp_vt =
    Cat(
      io.vt_regid_imm.vt === array_wport_vd(7),
      io.vt_regid_imm.vt === array_wport_vd(6),
      io.vt_regid_imm.vt === array_wport_vd(5),
      io.vt_regid_imm.vt === array_wport_vd(4),
      io.vt_regid_imm.vt === array_wport_vd(3),
      io.vt_regid_imm.vt === array_wport_vd(2),
      io.vt_regid_imm.vt === array_wport_vd(1),
      io.vt_regid_imm.vt === array_wport_vd(0)
    )

  val vt_comp_vr =
    Cat(
      io.vt_regid_imm.vr === array_wport_vd(7),
      io.vt_regid_imm.vr === array_wport_vd(6),
      io.vt_regid_imm.vr === array_wport_vd(5),
      io.vt_regid_imm.vr === array_wport_vd(4),
      io.vt_regid_imm.vr === array_wport_vd(3),
      io.vt_regid_imm.vr === array_wport_vd(2),
      io.vt_regid_imm.vr === array_wport_vd(1),
      io.vt_regid_imm.vr === array_wport_vd(0)
    )

  val vt_comp_vd =
    Cat(
      io.vt_regid_imm.vd === array_wport_vd(7),
      io.vt_regid_imm.vd === array_wport_vd(6),
      io.vt_regid_imm.vd === array_wport_vd(5),
      io.vt_regid_imm.vd === array_wport_vd(4),
      io.vt_regid_imm.vd === array_wport_vd(3),
      io.vt_regid_imm.vd === array_wport_vd(2),
      io.vt_regid_imm.vd === array_wport_vd(1),
      io.vt_regid_imm.vd === array_wport_vd(0)
    )

  val vt_dhazard_vs = (array_wport_val.flatten() & array_wport_head.flatten() & vt_comp_vs).orR()
  val vt_dhazard_vt = (array_wport_val.flatten() & array_wport_head.flatten() & vt_comp_vt).orR()
  val vt_dhazard_vr = (array_wport_val.flatten() & array_wport_head.flatten() & vt_comp_vr).orR()
  val vt_dhazard_vd = (array_wport_val.flatten() & array_wport_head.flatten() & vt_comp_vd).orR()

  val vt_bhazard_r1w1 = array_rport_val.read(next_ptr2) | array_wport_val.read(vt_wptr)
  val vt_bhazard_r2w1 = array_rport_val.read(next_ptr2) | array_rport_val.read(next_ptr3) | array_wport_val.read(vt_wptr)
  val vt_bhazard_r3w1 = array_rport_val.read(next_ptr2) | array_rport_val.read(next_ptr3) | array_rport_val.read(next_ptr4) | array_wport_val.read(vt_wptr)
  val vt_bhazard_amo = array_rport_val.read(next_ptr2) | array_rport_val.read(next_ptr3) | array_wport_val.read(next_ptr4)
  val vt_bhazard_utld = array_rport_val.read(next_ptr2) | array_wport_val.read(next_ptr3)
  val vt_bhazard_utst = array_rport_val.read(next_ptr2) | array_rport_val.read(next_ptr3)

  val vt_stall =
    Cat(
      io.vt_valid.viu & io.seq_to_hazard.stall.orR(),
      io.vt_valid.vau0 & io.seq_to_hazard.stall.orR(),
      io.vt_valid.vau1 & io.seq_to_hazard.stall.orR(),
      io.vt_valid.vau2 & io.seq_to_hazard.stall.orR(),
      io.vt_valid.amo & (io.seq_to_hazard.stall(RG_VLDQ) | io.seq_to_hazard.stall(RG_VSDQ)),
      io.vt_valid.utld & (io.seq_to_hazard.stall(RG_VLDQ) | io.seq_to_hazard.stall(RG_VSDQ) | io.seq_to_hazard.stall(RG_UTSDQ)),
      io.vt_valid.utst & (io.seq_to_hazard.stall(RG_VLDQ) | io.seq_to_hazard.stall(RG_VSDQ) | io.seq_to_hazard.stall(RG_UTLDQ))
    )

  val vt_dhazard =
    Cat(
      ~io.vt_regid_imm.vs_zero & vt_dhazard_vs & io.vt_dhazard.vs,
      ~io.vt_regid_imm.vt_zero & vt_dhazard_vt & io.vt_dhazard.vt,
      ~io.vt_regid_imm.vr_zero & vt_dhazard_vr & io.vt_dhazard.vr,
      vt_dhazard_vd & io.vt_dhazard.vd
    )

  val vt_shazard =
    Cat(
      shazard_vau0 & io.vt_shazard.vau0,
      shazard_vau1 & io.vt_shazard.vau1,
      shazard_vau2 & io.vt_shazard.vau2,
      shazard_vgu & io.vt_shazard.vgu,
      shazard_vlu & io.vt_shazard.vlu,
      shazard_vsu & io.vt_shazard.vsu
    )

  val vt_seqhazard =
    Cat(
      io.vt_valid.viu & seqhazard_1slot,
      io.vt_valid.vau0 & seqhazard_1slot,
      io.vt_valid.vau1 & seqhazard_1slot,
      io.vt_valid.vau2 & seqhazard_1slot,
      io.vt_valid.amo & seqhazard_3slot,
      io.vt_valid.utld & seqhazard_2slot,
      io.vt_valid.utst & seqhazard_2slot
    )

  val vt_bhazard =
    Cat(
      vt_bhazard_r1w1 & io.vt_bhazard.r1w1,
      vt_bhazard_r2w1 & io.vt_bhazard.r2w1,
      vt_bhazard_r3w1 & io.vt_bhazard.r3w1,
      vt_bhazard_amo & io.vt_bhazard.amo,
      vt_bhazard_utld & io.vt_bhazard.utld,
      vt_bhazard_utst & io.vt_bhazard.utst
    )

  io.vt_ready := io.vt_regid_imm.vd_zero | !vt_stall.orR() & !vt_dhazard.orR() & !vt_shazard.orR() & !vt_seqhazard.orR() & !vt_bhazard.orR()
}
