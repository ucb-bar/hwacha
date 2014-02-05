package hwacha

import Chisel._
import Node._
import Constants._

class CntBundle extends Bundle
{
  val cnt = Bits(width = SZ_BCNT)
}

class BankUopRead extends CntBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val oplen = Bits(width = SZ_BOPL)
  val rblen = Vec.fill(SZ_BRPORT){Bool()}
}

class BankUopWrite extends CntBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val sel = Bits(width = SZ_BWPORT)
}

class BankUopVIU extends CntBundle
{
  val fn = Bits(width = SZ_VIU_FN)
  val utidx = Bits(width = SZ_VLEN)
  val imm = Bits(width = SZ_DATA)
}

class BankUopIO extends Bundle
{
  val read = Valid(new BankUopRead)
  val write = Valid(new BankUopWrite)
  val viu = Valid(new BankUopVIU)
}

class LfuncUopVAU0 extends CntBundle
{
  val fn = Bits(width = SZ_VAU0_FN)
}

class LfuncUopVAU1 extends CntBundle
{
  val fn = Bits(width = SZ_VAU1_FN)
}

class LfuncUopVAU2 extends CntBundle
{
  val fn = Bits(width = SZ_VAU2_FN)
}

class LfuncUopVGU extends CntBundle
{
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val utmemop = Bool()
}

class LfuncUopVLU extends CntBundle

class LfuncUopVSU extends CntBundle
{
  val mem = new io_vxu_mem_cmd()
}

class LfuncUopIO extends Bundle
{
  val vau0 = Valid(new LfuncUopVAU0)
  val vau1 = Valid(new LfuncUopVAU1)
  val vau2 = Valid(new LfuncUopVAU2)
  val vgu = Valid(new LfuncUopVGU)
  val vlu = Valid(new LfuncUopVLU)
  val vsu = Valid(new LfuncUopVSU)
}

class LaneUopIO extends Bundle
{
  val read = Valid(new BankUopRead)
  val write = Valid(new BankUopWrite)
  val viu = Valid(new BankUopVIU)
  val vau0 = Valid(new LfuncUopVAU0)
  val vau1 = Valid(new LfuncUopVAU1)
  val vau2 = Valid(new LfuncUopVAU2)
  val vgu = Valid(new LfuncUopVGU)
  val vlu = Valid(new LfuncUopVLU)
  val vsu = Valid(new LfuncUopVSU)
}
