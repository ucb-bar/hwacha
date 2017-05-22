package hwacha

import Chisel._
import config._
import scala.collection.mutable.LinkedHashSet
import uncore.agents._
import rocket._
import tile._
import coreplex._
import rocketchip._
import util._
import diplomacy._


class DefaultHwachaConfig extends Config((site, here, up) => {
    case HwachaIcacheKey => ICacheParams(
      nSets = 64,
      nWays = 1,
      rowBits = 1 * 64,
      nTLBEntries = 8,
      fetchBytes = 8, // Fetch one 8 byte instruction
      latency = 1
    )
    // Same as core's icache: NITLBEntries, NRAS, ECCCode, WordBits, Replacer

    case HwachaCommitLog => true

    // hwacha constants
    case HwachaNAddressRegs => 32
    case HwachaNScalarRegs => 64
    case HwachaNVectorRegs => 256
    case HwachaNPredRegs => 16
    case HwachaRegBits => math.max(log2Up(site(HwachaNVectorRegs)), log2Up(site(HwachaNScalarRegs)))
    case HwachaPredRegBits => log2Up(site(HwachaNPredRegs))
    case HwachaRegLen => 64
    case HwachaMaxVLen =>
      site(HwachaNBanks) * site(HwachaNSRAMRFEntries) *
        site(HwachaBankWidth) / site(HwachaRegLen)

    case HwachaNDTLB => 8
    case HwachaNPTLB => 2
    case HwachaLocalScalarFPU => false

    // Multi-lane constants
    case HwachaNLanes => 1

    // lane constants
    case HwachaBankWidth => 128
    case HwachaNBanks => 4
    case HwachaNSRAMRFEntries => 256
    case HwachaNFFRFEntries => 16
    case HwachaNFFRFReadPorts => 3
    case HwachaNPredRFEntries => 256
    case HwachaNPredRFReadPorts => 3
    case HwachaNOperandLatches => 6
    case HwachaNPredLatches => 4
    case HwachaWriteSelects => 2
    case HwachaRFAddrBits => math.max(log2Up(site(HwachaNSRAMRFEntries)), log2Up(site(HwachaNFFRFEntries)))
    case HwachaPRFAddrBits => log2Up(site(HwachaNPredRFEntries))

    case HwachaStagesALU => 1
    case HwachaStagesPLU => 0
    case HwachaStagesIMul => 3
    case HwachaStagesDFMA => 4
    case HwachaStagesSFMA => 3
    case HwachaStagesHFMA => 3
    case HwachaStagesFConv => 2
    case HwachaStagesFCmp => 1

    case HwachaNSeqEntries => 8

    case HwachaNVVAQEntries => 4
    case HwachaNVPAQEntries => 24
    case HwachaNVSDQEntries => 4
    case HwachaNVLDQEntries => 4
    case HwachaNVLTEntries => 64

    case HwachaNSMUEntries => 16
    case HwachaBuildVRU => true

    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(
        rocc = {
        import HwachaTestSuites._
        TestGeneration.addSuites(rv64uv.map(_("p")))
        TestGeneration.addSuites(rv64uv.map(_("vp")))
        // no excep or vm in v4 yet
        //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
        TestGeneration.addSuite(rv64sv("p"))
        TestGeneration.addSuite(hwachaBmarks)
        TestGeneration.addVariable("SRC_EXTENSION", "$(base_dir)/hwacha/$(src_path)/*.scala")
        TestGeneration.addVariable("DISASM_EXTENSION", "--extension=hwacha")
        Seq(RoCCParams(
          opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
          generator = (p: Parameters) => {
            LazyModule(new Hwacha()(p))},
          nPTWPorts = 2 + site(HwachaNLanes), // icache + vru + vmus
          useFPU = true))
    })}
    // Set TL network to 128bits wide
    case L1toL2Config => TLBusConfig(beatBytes = 16)

    case HwachaConfPrec => true
    case HwachaVRUMaxOutstandingPrefetches => 20
    case HwachaVRUEarlyIgnore => 1
    case HwachaVRUMaxRunaheadBytes => 16777216
    case HwachaCMDQLen => 32
    case HwachaVSETVLCompress => true
  }
)

class VectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-vec")
class ScalarVectorAssemblyTestSuite(prefix: String, names: LinkedHashSet[String])(env: String) extends AssemblyTestSuite(prefix, names)(env + "-svec")

object HwachaTestSuites {
import rocketchip.DefaultTestSuites._
  val rv64uvNames = LinkedHashSet(
    "wakeup", "fence", "keepcfg",
    "vmca", "vmcs", "vssd", "vssw", "vssh", "vssb",
    "vlsd", "vlsw", "vlswu", "vlsh", "vlshu", "vlsb", "vlsbu",
    "vsad", "vsaw", "vsah", "vsab", "vlad", "vlaw", "vlawu", "vlah", "vlahu", "vlab", "vlabu",
    "vld", "vlw", "vlwu", "vlh", "vlhu", "vlb", "vlbu", "vlxd", "vlxw", "vlxwu", "vlxh", "vlxhu", "vlxb", "vlxbu",
    "vsd", "vsw", "vsh", "vsb", "vsxd", "vsxw", "vsxh", "vsxb",
    "eidx", "imul", "fcvt", "vvadd_d", "vvadd_w", "vvadd_fd", "vvadd_fw", "vvmul_d",
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
