package hwacha

import Chisel._
import Node._
import Constants._

class ioDecoupled[T <: Data](view: List[String] = null)(data: => T) extends Bundle(view)
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class ioPipe[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_arbiter[T <: Data](n: Int)(data: => ioDecoupled[T]) extends Bundle
{
  val in = Vec(n){ data.flip }
  val out = data
  val chosen = Bits(log2up(n),OUTPUT)
}

class io_arbiterdpath[T <: Data](n: Int)(data: => ioDecoupled[T]) extends Bundle
{
  val in = Vec(n){ data.flip }
  val out = data
  val chosen = Bits(log2up(n),INPUT)
}

class ArbiterCtrl[T <: Data](n :Int)(data: => ioDecoupled[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1)
  {
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready
  }

  var choose = Bits(n-1)
  for (i <- 1 to n-1)
  {
    val actual = n-1-i
    choose = Mux(io.in(actual).valid, Bits(actual,log2up(n)), choose)
  }

  io.chosen := choose

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
}

class ArbiterDPath[T <: Data](n: Int)(data: => ioDecoupled[T]) extends Component
{
  val io = new io_arbiterdpath(n)(data)

  io.out.bits := io.in(io.chosen).bits
}

class Arbiter[T <: Data](n: Int)(data: => ioDecoupled[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  val ctrl = new ArbiterCtrl(n)(data)
  val dpath = new ArbiterDPath(n)(data)

  dpath.io.chosen := ctrl.io.chosen
  io.chosen := ctrl.io.chosen

  for (i <- 0 to n-1)
  {
    dpath.io.in(i).bits := io.in(i).bits
    io.in(i).ready := ctrl.io.in(i).ready
    ctrl.io.in(i).valid := io.in(i).valid
  }

  ctrl.io.out.ready := io.out.ready
  io.out.valid := ctrl.io.out.valid
  io.out.bits := dpath.io.out.bits
}

class RoundRobinArbiterCtrl[T <: Data](n :Int)(data: => ioDecoupled[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  // Round robin counter
  val cnt = Reg(resetVal = UFix(0, log2up(n)))

  if (isPow2(n))
  {
    cnt := cnt + UFix(1)
  }
  else
  {
    // Only need reset if n isn't a power of 2
    when (cnt === UFix(n-1)) { cnt := UFix(0) }
    .otherwise { cnt := cnt + UFix(1) }
  }

  val sel_vec = Vec(n) {Wire() {Bits(width=log2up(n))}}
  // val in_ready_vec = Vec(n) {Vec(n) {Wire() {Bool()}}}
  // make a sel value for every possible value of cnt
  for (i <- 0 to n-1)
  {
    // initialize sel value
    var sel = Bits(n-1-i)
    // in_ready_vec(i)(i) := io.out.ready
    for (j <- 1 to n-1)
    {
      val actual_sel = (i + (n-1-j)) % n
      val actual_ready = (i + j) % n
      val actual_ready_prev = (i + j - 1) % n
      sel = Mux(io.in(actual_sel).valid, Bits(actual_sel,log2up(n)), sel)
      // in_ready_vec(i)(actual_ready) := !io.in(actual_ready_prev).valid && in_ready_vec(i)(actual_ready_prev)
    }
    sel_vec(i) := sel
  }

  for (i <- 0 to n-1)
  {
    // only the chosen gets the true out ready signal
    // io.in(i).ready := in_ready_vec(sel_vec(cnt))(i)
    io.in(i).ready := Mux(UFix(i) === sel_vec(cnt), io.out.ready, Bool(false))
  }

  io.chosen := sel_vec(cnt)

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
}

class RoundRobinArbiter[T <: Data](n: Int)(data: => ioDecoupled[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  val ctrl = new RoundRobinArbiterCtrl(n)(data)
  val dpath = new ArbiterDPath(n)(data)

  dpath.io.chosen := ctrl.io.chosen
  io.chosen := ctrl.io.chosen

  for (i <- 0 to n-1)
  {
    io.in(i).ready := ctrl.io.in(i).ready
    ctrl.io.in(i).valid := io.in(i).valid
    dpath.io.in(i).bits := io.in(i).bits
  }

  ctrl.io.out.ready := io.out.ready
  io.out.valid := ctrl.io.out.valid
  io.out.bits := dpath.io.out.bits
}

class io_imem_req extends ioDecoupled()( { Bits(width = SZ_ADDR) } )
class io_imem_resp extends ioPipe()( { Bits(width = SZ_INST) } )

class io_vxu_cmdq extends ioDecoupled()( { Bits(width = SZ_XCMD) } )
class io_vxu_immq extends ioDecoupled()( { Bits(width = SZ_XIMM) } )
class io_vxu_imm2q extends ioDecoupled()( { Bits(width = SZ_XIMM2) } )
class io_vxu_cntq extends ioDecoupled()( Bits(width = SZ_VLEN) )
class io_vxu_numcntq extends ioDecoupled()( Bits(width = 1) )
class io_vxu_ackq extends ioDecoupled()( { Bits(width = SZ_XRESP) } )

class io_vec_cmdq extends ioDecoupled()( { Bits(width = SZ_VCMD) } )
class io_vec_ximm1q extends ioDecoupled()( { Bits(width = SZ_VIMM) } )
class io_vec_ximm2q extends ioDecoupled()( { Bits(width = SZ_VSTRIDE) } )
class io_vec_cntq() extends ioDecoupled()( Bits(width=SZ_VLEN) )
class io_vec_ackq extends ioDecoupled()( { Bits(width = SZ_VRESP) } )

class io_vvaq extends ioDecoupled()( { new io_vvaq_bundle() } )
class io_vpaq extends ioDecoupled()( { new io_vpaq_bundle() } )
class io_vldq extends ioDecoupled()( { Bits(width = SZ_DATA) } )
class io_vsdq extends ioDecoupled()( { Bits(width = SZ_DATA) } )

class io_update_num_cnt extends ioPipe()( { Bits(width=SZ_IRB_NUMCNT) } )

class io_cpu_exception extends Bundle 
{
  val supervisor_mode = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val addr = UFix(SZ_ADDR, OUTPUT)
}

class io_imul_req_bundle extends Bundle
{
  val fn = Bits(width = SZ_VAU0_FN)
  val in0 = Bits(width = SZ_XLEN)
  val in1 = Bits(width = SZ_XLEN)
}

class io_imul_req extends ioDecoupled()( { new io_imul_req_bundle() } )

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

class io_dmem_req extends ioPipe()({ new io_dmem_req_bundle() })
class io_dmem_resp extends ioPipe()({ new io_dmem_resp_bundle() })

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
  val qcnt = UFix(width = SZ_QCNT)
}

class io_vxu_irb_bundle extends Bundle
{
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
  val numCnt_rtag = Bits(SZ_IRB_CMD, OUTPUT)
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

class ioDTLB_CPU_req extends ioDecoupled()( { new ioDTLB_CPU_req_bundle() } )

class ioDTLB_CPU_resp extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = Bits(PPN_BITS, OUTPUT)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
}

