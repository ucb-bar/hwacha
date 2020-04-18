package hwacha

import scala.collection.mutable.LinkedHashSet
import freechips.rocketchip.system._
import freechips.rocketchip.config._

class VectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-vec")
class ScalarVectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-svec")

object HwachaTestSuites {
import freechips.rocketchip.system.DefaultTestSuites._
  val rv64uvNames = LinkedHashSet(
    "wakeup", "fence", "keepcfg",
    "vmca", "vmcs", "vssd", "vssw", "vssh", "vssb",
    "vlsd", "vlsw", "vlswu", "vlsh", "vlshu", "vlsb", "vlsbu",
    "vsad", "vsaw", "vsah", "vsab", "vlad", "vlaw", "vlawu", "vlah", "vlahu", "vlab", "vlabu",
    "vld", "vlw", "vlwu", "vlh", "vlhu", "vlb", "vlbu", "vlxd", "vlxw", "vlxwu", "vlxh", "vlxhu", "vlxb", "vlxbu",
    "vsd", "vsw", "vsh", "vsb", "vsxd", "vsxw", "vsxh", "vsxb",
    "eidx", "imul", "fcvt", "fcvt_hs", "cmp", "fcmp", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
    "overlap", "sched_sreg_xbar", "sched_fadd", "sched_waw", "sched_war", "pointer", "vcjal", "vfirst", "vfence",
    "vl_empty", "vs_empty", "vlx_empty", "vsx_empty", "vamo_empty", "eidx_empty") ++
    (rv32uaNames -- Set("lrsc")) ++ (rv64uaNames -- Set("lrsc"))
  val rv64uvBasic = new AssemblyTestSuite("rv64uv", rv64uvNames)(_)

  val rv64uiVecNames = rv32uiNames ++ rv64uiNames -- Set("simple", "auipc", "lui", "fence_i",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "jal", "jalr",
    "lb", "lbu", "lh", "lhu", "lw", "lwu", "ld", "sb", "sh", "sw", "sd")
  val rv64uiVec = new VectorAssemblyTestSuite("rv64ui", rv64uiVecNames)(_)
  val rv64uiScalarVec = new ScalarVectorAssemblyTestSuite("rv64ui", rv64uiVecNames)(_)

  val rv64umVec = new VectorAssemblyTestSuite("rv64um", rv64umNames)(_)
  val rv64umScalarVec = new ScalarVectorAssemblyTestSuite("rv64um", rv64umNames)(_)

  val rv64ufVecNames = rv64ufNames -- Set("ldst", "move")
  val rv64ufVec = new VectorAssemblyTestSuite("rv64uf", rv64ufVecNames)(_)
  val rv64udVec = new VectorAssemblyTestSuite("rv64ud", rv64ufVecNames)(_)

  val rv64ufScalarVecNames = rv64ufVecNames -- Set("fdiv", "fcmp") // unsupported by current scalar unit
  val rv64ufScalarVec = new ScalarVectorAssemblyTestSuite("rv64uf", rv64ufScalarVecNames)(_)
  val rv64udScalarVec = new ScalarVectorAssemblyTestSuite("rv64ud", rv64ufScalarVecNames)(_)

  val rv64uv = List(rv64ufScalarVec, rv64ufVec, rv64udScalarVec, rv64udVec, rv64uiScalarVec, rv64uiVec, rv64umScalarVec, rv64umVec, rv64uvBasic)

  val rv64svNames = LinkedHashSet(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
    val rv64svNamesV4 = rv64svNames -- Set(
    "illegal_inst", "illegal_vt_inst", "illegal_vt_regid", "ma_utld", "ma_utsd", "ma_vld", "ma_vsd", "ma_vt_inst", "privileged_inst")
  val rv64sv = new AssemblyTestSuite("rv64sv", rv64svNamesV4)(_)

  val hwachaBmarks = new BenchmarkTestSuite("hwacha", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet(
    "pb-spmv", "vec-daxpy", "vec-dgemm-opt", "vec-hsaxpy", "vec-hgemm-opt", "vec-hsgemm-opt", "vec-saxpy", "vec-sdaxpy", "vec-sdgemm-opt", "vec-sgemm-naive", "vec-sgemm-opt", "vec-vvadd"))

}
