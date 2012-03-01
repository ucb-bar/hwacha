package hwacha

import Chisel._
import Node._
import Constants._
import queues._

class io_ready_valid[T <: Data](view: List[String] = null)(data: => T) extends Bundle(view)
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_valid[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_arbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Bundle
{
  val in = Vec(n){ data.flip() }
  val out = data
  val chosen = Bits(log2up(n),OUTPUT)
}

class Arbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1)
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- 1 to n-1)
  {
    val actual = n-1-i
    dout = Mux(io.in(actual).valid, io.in(actual).bits, dout)
    choose = Mux(io.in(actual).valid, Bits(actual,log2up(n)), choose)
  }

  io.chosen := choose

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
  dout <> io.out.bits
}

class io_imem_req extends io_ready_valid()( { Bits(width = SZ_ADDR) } )
class io_imem_resp extends io_valid()( { Bits(width = SZ_INST) } )

class io_vxu_cmdq extends io_ready_valid()( { Bits(width = SZ_XCMD) } )
class io_vxu_immq extends io_ready_valid()( { Bits(width = SZ_XIMM) } )
class io_vxu_imm2q extends io_ready_valid()( { Bits(width = SZ_XIMM2) } )
class io_vxu_cntq extends io_ready_valid()( Bits(width = SZ_VLEN) )
class io_vxu_ackq extends io_ready_valid()( { Bits(width = SZ_XRESP) } )

class io_vec_cmdq extends io_ready_valid()( { Bits(width = SZ_VCMD) } )
class io_vec_ximm1q extends io_ready_valid()( { Bits(width = SZ_VIMM) } )
class io_vec_ximm2q extends io_ready_valid()( { Bits(width = SZ_VSTRIDE) } )
class io_vec_cntq() extends io_ready_valid()( Bits(width=SZ_VLEN) )
class io_vec_ackq extends io_ready_valid()( { Bits(width = SZ_VRESP) } )

class io_vvaq extends io_ready_valid()( { new io_vvaq_bundle() } )
class io_vpaq extends io_ready_valid()( { new io_vpaq_bundle() } )
class io_vldq extends io_ready_valid()( { Bits(width = SZ_DATA) } )
class io_vsdq extends io_ready_valid()( { Bits(width = SZ_DATA) } )

class io_cpu_exception extends Bundle 
{
  val supervisor_mode = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val addr = UFix(SZ_ADDR, OUTPUT)
}

class io_imul_req_bundle extends Bundle
{
  val fn = Bits(width = SZ_VAU0_FN)
  val in0 = Bits(width = SZ_XLEN)
  val in1 = Bits(width = SZ_XLEN)
}

class io_imul_req extends io_ready_valid()( { new io_imul_req_bundle() } )

class io_dmem_req_bundle extends Bundle
{
  val kill = Bool()
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val idx = Bits(width = PGIDX_BITS)
  val ppn = Bits(width = PPN_BITS)
  val data = Bits(width = 64)
  val tag = Bits(width = 10)
}

class io_dmem_resp_bundle extends Bundle
{
  val nack = Bool()
  val data = Bits(width = 64)
  val tag = Bits(width = 10)
  val typ = Bits(width = 3)
}

class io_dmem_req extends io_valid()({ new io_dmem_req_bundle() })
class io_dmem_resp extends io_valid()({ new io_dmem_resp_bundle() })

class io_irbUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UFix(width=ADDR_SIZE)
}

class io_qstall extends Bundle
{
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
}

class io_vxu_mem_check extends Bundle
{
  val checkcnt = Bool()
  val cnt = UFix(width = 4)
}

class io_vxu_mem_cmd extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
}

class io_vvaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UFix(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
  val idx = Bits(width = PGIDX_BITS)
  val vpn = Bits(width = VPN_BITS)
}

class io_vpaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UFix(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
  val idx = Bits(width = PGIDX_BITS)
  val ppn = Bits(width = PPN_BITS)
}

class io_vxu_to_vmu extends Bundle
{
  val qcnt = UFix(width = 5)
}

class io_vxu_irb_bundle extends Bundle
{
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
  val cnt_rtag = Bits(SZ_IRB_CNT, OUTPUT)
  val pc_next = Bits(SZ_ADDR, OUTPUT)
  val update_imm1 = Bool(OUTPUT)
}

class ioDTLB_CPU_req_bundle extends Bundle
{
  // lookup requests
  val kill  = Bool()
  val cmd  = Bits(width=4) // load/store/amo
  val asid = Bits(width=ASID_BITS)
  val vpn  = Bits(width=VPN_BITS+1)
}

class ioDTLB_CPU_req extends io_ready_valid()( { new ioDTLB_CPU_req_bundle() } )

class ioDTLB_CPU_resp extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = Bits(PPN_BITS, OUTPUT)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
}

