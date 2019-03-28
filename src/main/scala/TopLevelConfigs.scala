// See LICENSE for license details.

package hwacha

import freechips.rocketchip._
import freechips.rocketchip.system._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket._
import hwacha._
import freechips.rocketchip.config._

class HwachaConfig extends Config(new DefaultHwachaConfig ++ new DefaultConfig)

class EOS24Config extends Config(new WithNBanks(4) ++ new HwachaConfig)

class WithNLanes(n: Int) extends Config((site, here, up) => {
  case HwachaNLanes => n
})

class With32BtbEntires extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(btb = r.btb.map(_.copy(nEntries = 32)))
  }
})

class Process28nmConfig extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(fpu = r.core.fpu.map(_.copy(sfmaLatency = 3, dfmaLatency = 4))))
  }
})

class WithoutConfPrec extends Config((site, here, up) => {
    case HwachaConfPrec => false
})

class WithSmallPredRF extends Config((site, here, up) => {
    case HwachaNPredRFEntries => 128
})

class ISCA2016Config extends Config(
  new Process28nmConfig ++
  new WithNBanks(4) ++
  new With32BtbEntires ++ new HwachaConfig)
class FastISCA2016Config extends Config(new WithoutTLMonitors ++ new ISCA2016Config)

class ISCA2016L2Config extends Config(new WithNLanes(2) ++ new ISCA2016Config)
class ISCA2016L4Config extends Config(new WithNLanes(4) ++ new ISCA2016Config)

class ISCA2016HOVB4Config extends Config(new WithNBanks(2) ++ new ISCA2016Config)
class ISCA2016HOVB8Config extends Config(new ISCA2016Config)
class ISCA2016LOVB4Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB4Config)
class ISCA2016LOVB8Config extends Config(new WithoutConfPrec ++ new ISCA2016HOVB8Config)

class ISCA2016HOVL2B4Config extends Config(new WithNLanes(2) ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL2B8Config extends Config(new WithNLanes(2) ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL2B4Config extends Config(new WithNLanes(2) ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL2B8Config extends Config(new WithNLanes(2) ++ new ISCA2016LOVB8Config)

class ISCA2016HOVL4B4Config extends Config(new WithNLanes(4) ++ new ISCA2016HOVB4Config)
class ISCA2016HOVL4B8Config extends Config(new WithNLanes(4) ++ new ISCA2016HOVB8Config)
class ISCA2016LOVL4B4Config extends Config(new WithNLanes(4) ++ new ISCA2016LOVB4Config)
class ISCA2016LOVL4B8Config extends Config(new WithNLanes(4) ++ new ISCA2016LOVB8Config)

class DualCoreISCA2016L2Config extends Config(new WithNBigCores(2) ++ new WithNLanes(2) ++ new ISCA2016Config)

class HurricaneSimilarConfig extends Config(new WithNLanes(2) ++ new WithNMemoryChannels(8) ++ new WithNBanks(1) ++ new ISCA2016Config)
