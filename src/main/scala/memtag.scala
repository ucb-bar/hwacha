package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_cmd_type extends Bundle
{
  val size = Bits(width = 3)
  val float = Bool()
}

class io_vmu_req_tag(TAG_SIZE: Int) extends Bundle
{
	val major = Bits(width = TAG_SIZE) // VLDQ-supplied tag
	val minor = Bits(width = 2) // Offset of subword element
	override def clone = new io_vmu_req_tag(TAG_SIZE).asInstanceOf[this.type]
}

class MemTag(implicit conf: HwachaConfiguration) extends Module
{
	val io = new Bundle {
		val prec = Bits(INPUT, SZ_PREC)
		val typ = Decoupled(new io_vmu_cmd_type()).flip
		val tag = Decoupled(Bits(width = log2Up(conf.nvldq))).flip // VLDQ tag
		val out = Decoupled(new io_vmu_req_tag(log2Up(conf.nvldq)))
	}

	val major = Reg(Bits())
	val minor = Reg(init = UInt(0, 2))
	val size = Reg(Bool())

	val size_w = is_mtype_word(io.typ.bits.size)
	val size_hw = is_mtype_halfword(io.typ.bits.size)

	// Whether all subword elements corresponding to the last VLDQ entry have been handled
	val ent_finished = (minor === UInt(0))
	// Whether to start a new VLDQ entry due to datatype mismatch
	val ent_mismatch = !(io.typ.bits.float && (size_w || size_hw) && (size === size_hw))
	val ent_next = ent_finished || ent_mismatch

	io.typ.ready := io.out.ready && (!ent_mismatch || io.tag.valid)
	io.tag.ready := io.out.ready && (ent_next && io.typ.valid)

	val minor_next = MuxLookup(
		io.prec, Bits(0, 2),
		Array(
			(PREC_SINGLE) -> Cat(~minor(1), Bits(0, 1)),
			(PREC_HALF)   -> (minor + UInt(1))
		))

	io.out.valid := io.typ.valid && (io.tag.valid || !ent_next)
	io.out.bits.major := Mux(ent_next, io.tag.bits, major)
	io.out.bits.minor := Mux(ent_mismatch, Bits(0), minor)

	when (io.out.fire()) {
		major := io.out.bits.major
		minor := minor_next
		size := size_hw
	}

}
