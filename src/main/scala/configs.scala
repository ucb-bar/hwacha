package hwacha

import Chisel._
import freechips.rocketchip._
import freechips.rocketchip.system._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._


class DefaultHwachaConfig extends Config((site, here, up) => {
    case HwachaIcacheKey => ICacheParams(
      nSets = 64,
      nWays = 1,
      rowBits = 1 * 64,
      nTLBWays = 8,
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
    case HwachaNPTLB => 4
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
    case HwachaNVMTEntries => 64

    case HwachaNSMUEntries => 16
    case HwachaBuildVRU => true

    case BuildRoCC => up(BuildRoCC) ++ Seq(
      (p: Parameters) => {
         val hwacha = LazyModule.apply(new Hwacha()(p))
         hwacha
      }
    )
    // Set TL network to 128bits wide
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)

    case HwachaConfPrec => false
    case HwachaVRUMaxOutstandingPrefetches => 20
    case HwachaVRUEarlyIgnore => 1
    case HwachaVRUMaxRunaheadBytes => 16777216
    case HwachaCMDQLen => 32
    case HwachaVSETVLCompress => true
  }
)
