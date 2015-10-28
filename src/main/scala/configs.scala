package hwacha

import Chisel._
import cde.{Config, Parameters, Knob}
import uncore._
import rocket._

class DefaultHwachaConfig extends Config (
  (pname,site,here) => pname match {
    case NSets => site(CacheName) match { case "HwI" => 64 }
    case NWays => site(CacheName) match { case "HwI" => 1 }
    case RowBits => site(CacheName) match { case "HwI" => 2*site(CoreInstBits) }
    case NTLBEntries => site(CacheName) match { case "HwI" => 8 }
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
    case HwachaStagesFMA => 3
    case HwachaStagesFConv => 2
    case HwachaStagesFCmp => 1

    case HwachaNVVAQEntries => 4
    case HwachaNVPAQEntries => 24
    case HwachaNVPFQEntries => 8
    case HwachaNVSDQEntries => 16
    case HwachaNVLDQEntries => 16
    case HwachaNVLTEntries => 32

    case HwachaNSMUEntries => 2

    // +2 comes from the overhead of tagging for the arbitration
    case RoccMaxTaggedMemXacts =>
      math.max(site(HwachaNVLTEntries), site(HwachaNSMUEntries))
    case RoccNMemChannels => site(HwachaNLanes)
    case BuildRoCC => Some((p: Parameters) => 
      Module(new Hwacha()(p.alterPartial({ case CoreName => "Hwacha" }))))
  }
) 
{
  override val knobValues:Any=>Any = {
    case "HWACHA_NSRAMRF_ENTRIES" => 256
  }
}
