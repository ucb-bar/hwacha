package hwacha

import Chisel._
import cde.{Config, Parameters, Knob}
import uncore._
import rocket._

class DefaultHwachaConfig extends Config (
  (pname,site,here) => pname match {
    case "HwI" => {
      case NSets => 64
      case NWays => 2
      case RowBits => 2 * site(CoreInstBits)
      case NTLBEntries => 8
      case CacheIdBits => 0
    }:PartialFunction[Any, Any]
    case FetchWidth => 1
    case CoreInstBits => if (site(CoreName) == "Hwacha") 64 else 32
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
    case HwachaCacheBlockOffsetBits => site(CacheBlockOffsetBits)
    case HwachaLocalScalarFPU => false

    // Multi-lane constants
    case HwachaNLanes => 1

    // lane constants
    case HwachaBankWidth => 128
    case HwachaNBanks => 4
    case HwachaNSRAMRFEntries => Knob("HWACHA_NSRAMRF_ENTRIES")
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
    case HwachaStagesHFMA => 2
    case HwachaStagesFConv => 2
    case HwachaStagesFCmp => 1

    case HwachaNSeqEntries => 8

    case HwachaNVVAQEntries => 4
    case HwachaNVPAQEntries => 24
    case HwachaNVSDQEntries => 4
    case HwachaNVLDQEntries => 4
    case HwachaNVLTEntries => 64

    case HwachaNSMUEntries => 2
    case HwachaBuildVRU => Knob("HWACHA_BUILD_VRU")

    // +2 comes from the overhead of tagging for the arbitration
    case RoccMaxTaggedMemXacts =>
      math.max(site(HwachaNVLTEntries), site(HwachaNSMUEntries))
    case BuildRoCC =>
      Seq(RoccParameters(
        opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
        generator = (p: Parameters) =>
          (Module(new Hwacha()(p.alterPartial({ case CoreName => "Hwacha" })))),
        nMemChannels = site(HwachaNLanes),
        useFPU = true))

    case HwachaConfPrec => true
    case HwachaVRUThrottle => Knob("HWACHA_VRU_THROTTLE")
    case HwachaVRUEarlyIgnore => Knob("HWACHA_VRU_EARLY_IGNORE")
    case HwachaVRUDistThrottle => Knob("HWACHA_VRU_DIST_THROTTLE")
    case HwachaCMDQLen => Knob("HWACHA_CMDQ_LEN")
    case HwachaVSETVLCompress => Knob("HWACHA_VSETVL_COMPRESS")
  },
  knobValues = {
    case "HWACHA_NSRAMRF_ENTRIES" => 256
    case "HWACHA_BUILD_VRU" => true
    case "HWACHA_VRU_THROTTLE" => 20 // try 8, 16, 20, 32, 64
    case "HWACHA_VRU_EARLY_IGNORE" => 2 // try 0, 1, 2, 3, 4
    case "HWACHA_VRU_DIST_THROTTLE" => 128 // try 8, 16, 32, 64, 128, 256, 512, 2048
    case "HWACHA_CMDQ_LEN" => 32 // try 8, 16, 32, 64, 128
    case "HWACHA_VSETVL_COMPRESS" => true
  })
