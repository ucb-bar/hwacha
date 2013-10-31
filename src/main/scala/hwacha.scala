package hwacha

import Chisel._
import rocket._
import uncore._

case class HwachaConfiguration(nbanks: Int, nreg_per_bank: Int)
{
  val nreg_total = nbanks * nreg_per_bank
}

class Hwacha(hc: HwachaConfiguration, rc: RocketConfiguration) extends RoCC(rc) {
  // Make Hwacha here
}
