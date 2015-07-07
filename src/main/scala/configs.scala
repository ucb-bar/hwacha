package hwacha

import Chisel._
import uncore._
import rocket._

class DefaultHwachaConfig extends ChiselConfig (
  (pname,site,here) => pname match {
    case NSets => site(CacheName) match { case "HwI" => 64 }
    case NWays => site(CacheName) match { case "HwI" => 1 }
    case RowBits => site(CacheName) match { case "HwI" => 4*site(CoreInstBits) }
    case NTLBEntries => site(CacheName) match { case "HwI" => 8 }
    case FetchWidth => 1
    case NBTBEntries => if(site(CoreName) == "Hwacha") 8 else 62
    //Same as core's icache: NITLBEntries, NRAS, ECCCode, WordBits, Replacer
    case HwachaNBanks => 8
    case HwachaNRegPerBank => 256
    case HwachaNDTLB => 8
    case HwachaNPTLB => 2
    case HwachaCacheBlockOffsetBits => site(CacheBlockOffsetBits)
    case HwachaNVectorLoadMetaBufferEntries => 16
    // +2 comes from the overhead of tagging for the arbitration
    case RoCCMaxTaggedMemXacts => site(HwachaNVectorLoadMetaBufferEntries)
    case BuildRoCC => Some(() => (Module(new Hwacha, { case CoreName => "Hwacha" })))
  }
)
