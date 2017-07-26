package hwacha

import scala.collection.mutable.LinkedHashSet
import freechips.rocketchip.system._
import freechips.rocketchip.config._
import freechips.rocketchip.util.GeneratorApp

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
    "eidx", "imul", "fcvt", "fcvt_hs", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
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

object Generator extends GeneratorApp {
  override def addTestSuites {
  val rv64RegrTestNames = LinkedHashSet(
        "rv64ud-v-fcvt",
        "rv64ud-p-fdiv",
        "rv64ud-v-fadd",
        "rv64uf-v-fadd",
        "rv64um-v-mul",
        "rv64mi-p-breakpoint",
        "rv64uc-v-rvc",
        "rv64ud-v-structural",
        "rv64si-p-wfi",
        "rv64um-v-divw",
        "rv64ua-v-lrsc",
        "rv64ui-v-fence_i",
        "rv64ud-v-fcvt_w",
        "rv64uf-v-fmin",
        "rv64ui-v-sb",
        "rv64ua-v-amomax_d",
        "rv64ud-v-move",
        "rv64ud-v-fclass",
        "rv64ua-v-amoand_d",
        "rv64ua-v-amoxor_d",
        "rv64si-p-sbreak",
        "rv64ud-v-fmadd",
        "rv64uf-v-ldst",
        "rv64um-v-mulh",
        "rv64si-p-dirty")

  val rv32RegrTestNames = LinkedHashSet(
      "rv32mi-p-ma_addr",
      "rv32mi-p-csr",
      "rv32ui-p-sh",
      "rv32ui-p-lh",
      "rv32uc-p-rvc",
      "rv32mi-p-sbreak",
      "rv32ui-p-sll")

    import freechips.rocketchip.coreplex.RocketTilesKey
    import freechips.rocketchip.tile.XLen
    import DefaultTestSuites._
    val xlen = params(XLen)
    // TODO: for now only generate tests for the first core in the first coreplex
    val tileParams = params(RocketTilesKey).head
    val coreParams = tileParams.core
    val vm = coreParams.useVM
    val env = if (vm) List("p","v") else List("p")
    coreParams.fpu foreach { case cfg =>
      if (xlen == 32) {
        TestGeneration.addSuites(env.map(rv32ufNoDiv))
      } else {
        TestGeneration.addSuite(rv32udBenchmarks)
        TestGeneration.addSuites(env.map(rv64ufNoDiv))
        TestGeneration.addSuites(env.map(rv64udNoDiv))
        if (cfg.divSqrt) {
          TestGeneration.addSuites(env.map(rv64uf))
          TestGeneration.addSuites(env.map(rv64ud))
        }
      }
    }
    if (coreParams.useAtomics) {
      if (tileParams.dcache.flatMap(_.scratch).isEmpty)
        TestGeneration.addSuites(env.map(if (xlen == 64) rv64ua else rv32ua))
      else
        TestGeneration.addSuites(env.map(if (xlen == 64) rv64uaSansLRSC else rv32uaSansLRSC))
    }
    if (coreParams.useCompressed) TestGeneration.addSuites(env.map(if (xlen == 64) rv64uc else rv32uc))
    val (rvi, rvu) =
      if (xlen == 64) ((if (vm) rv64i else rv64pi), rv64u)
      else            ((if (vm) rv32i else rv32pi), rv32u)

    TestGeneration.addSuites(rvi.map(_("p")))
    TestGeneration.addSuites((if (vm) List("v") else List()).flatMap(env => rvu.map(_(env))))
    TestGeneration.addSuite(benchmarks)
    TestGeneration.addSuite(new RegressionTestSuite(if (xlen == 64) rv64RegrTestNames else rv32RegrTestNames))


    import HwachaTestSuites._
    TestGeneration.addSuites(rv64uv.map(_("p")))
    TestGeneration.addSuites(rv64uv.map(_("vp")))
    // no excep or vm in v4 yet
    //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
    TestGeneration.addSuite(rv64sv("p"))
    TestGeneration.addSuite(hwachaBmarks)
  }

  override def generateTestSuiteMakefrags {
    addTestSuites
    var frag = TestGeneration.generateMakefrag + "\nSRC_EXTENSION = $(base_dir)/hwacha/$(src_path)/*.scala" + "\nDISASM_EXTENSION = --extension=hwacha"
    writeOutputFile(td, s"$longName.d", frag)
  }

  val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags
  generateArtefacts
}
