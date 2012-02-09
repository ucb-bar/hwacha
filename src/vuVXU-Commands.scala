package riscvVector

import Chisel._
import Node._

object Commands
{
  val CMD_X          = Bits(0,8)

  val CMD_VVCFGIVL   = Bits("b00_0000_00",8)
  val CMD_VSETVL     = Bits("b00_0000_10",8)
  val CMD_VF         = Bits("b00_0000_11",8)

  val CMD_FENCE_L_V  = Bits("b00_001_100",8)
  val CMD_FENCE_G_V  = Bits("b00_001_101",8)
  val CMD_FENCE_L_CV = Bits("b00_001_110",8)
  val CMD_FENCE_G_CV = Bits("b00_001_111",8)

  val CMD_LDWB       = Bits("b00_010_000",8)
  val CMD_STAC       = Bits("b00_010_001",8)

  val CMD_VMVV       = Bits("b01_000_000",8)
  val CMD_VMSV       = Bits("b01_001_000",8)
  val CMD_VMST       = Bits("b01_010_000",8)
  val CMD_VMTS       = Bits("b01_011_000",8)
  val CMD_VFMVV      = Bits("b01_000_001",8)
  val CMD_VFMSV      = Bits("b01_001_001",8)
  val CMD_VFMST      = Bits("b01_010_001",8)
  val CMD_VFMTS      = Bits("b01_011_001",8)

  val CMD_VLD        = Bits("b1_00_0_0_0_11",8)
  val CMD_VLW        = Bits("b1_00_0_0_0_10",8)
  val CMD_VLWU       = Bits("b1_00_0_0_1_10",8)
  val CMD_VLH        = Bits("b1_00_0_0_0_01",8)
  val CMD_VLHU       = Bits("b1_00_0_0_1_01",8)
  val CMD_VLB        = Bits("b1_00_0_0_0_00",8)
  val CMD_VLBU       = Bits("b1_00_0_0_1_00",8)
  val CMD_VSD        = Bits("b1_00_1_0_0_11",8)
  val CMD_VSW        = Bits("b1_00_1_0_0_10",8)
  val CMD_VSH        = Bits("b1_00_1_0_0_01",8)
  val CMD_VSB        = Bits("b1_00_1_0_0_00",8)

  val CMD_VFLD       = Bits("b1_00_0_1_0_11",8)
  val CMD_VFLW       = Bits("b1_00_0_1_0_10",8)
  val CMD_VFSD       = Bits("b1_00_1_1_0_11",8)
  val CMD_VFSW       = Bits("b1_00_1_1_0_10",8)

  val CMD_VLSTD      = Bits("b1_01_0_0_0_11",8)
  val CMD_VLSTW      = Bits("b1_01_0_0_0_10",8)
  val CMD_VLSTWU     = Bits("b1_01_0_0_1_10",8)
  val CMD_VLSTH      = Bits("b1_01_0_0_0_01",8)
  val CMD_VLSTHU     = Bits("b1_01_0_0_1_01",8)
  val CMD_VLSTB      = Bits("b1_01_0_0_0_00",8)
  val CMD_VLSTBU     = Bits("b1_01_0_0_1_00",8)
  val CMD_VSSTD      = Bits("b1_01_1_0_0_11",8)
  val CMD_VSSTW      = Bits("b1_01_1_0_0_10",8)
  val CMD_VSSTH      = Bits("b1_01_1_0_0_01",8)
  val CMD_VSSTB      = Bits("b1_01_1_0_0_00",8)

  val CMD_VFLSTD     = Bits("b1_01_0_1_0_11",8)
  val CMD_VFLSTW     = Bits("b1_01_0_1_0_10",8)
  val CMD_VFSSTD     = Bits("b1_01_1_1_0_11",8)
  val CMD_VFSSTW     = Bits("b1_01_1_1_0_10",8)

  val CMD_VLXD       = Bits("b1_10_0_0_0_11",8)
  val CMD_VLXW       = Bits("b1_10_0_0_0_10",8)
  val CMD_VLXWU      = Bits("b1_10_0_0_1_10",8)
  val CMD_VLXH       = Bits("b1_10_0_0_0_01",8)
  val CMD_VLXHU      = Bits("b1_10_0_0_1_01",8)
  val CMD_VLXB       = Bits("b1_10_0_0_0_00",8)
  val CMD_VLXBU      = Bits("b1_10_0_0_1_00",8)
  val CMD_VSXD       = Bits("b1_10_1_0_0_11",8)
  val CMD_VSXW       = Bits("b1_10_1_0_0_10",8)
  val CMD_VSXH       = Bits("b1_10_1_0_0_01",8)
  val CMD_VSXB       = Bits("b1_10_1_0_0_00",8)

  val CMD_VFLXD      = Bits("b1_10_0_1_0_11",8)
  val CMD_VFLXW      = Bits("b1_10_0_1_0_10",8)
  val CMD_VFSXD      = Bits("b1_10_1_1_0_11",8)
  val CMD_VFSXW      = Bits("b1_10_1_1_0_10",8)

  val CMD_VAMOADDD   = Bits("b1_11_000_11",8)
  val CMD_VAMOSWAPD  = Bits("b1_11_001_11",8)
  val CMD_VAMOANDD   = Bits("b1_11_010_11",8)
  val CMD_VAMOORD    = Bits("b1_11_011_11",8)
  val CMD_VAMOMIND   = Bits("b1_11_100_11",8)
  val CMD_VAMOMAXD   = Bits("b1_11_101_11",8)
  val CMD_VAMOMINUD  = Bits("b1_11_110_11",8)
  val CMD_VAMOMAXUD  = Bits("b1_11_111_11",8)

  val CMD_VAMOADDW   = Bits("b1_11_000_10",8)
  val CMD_VAMOSWAPW  = Bits("b1_11_001_10",8)
  val CMD_VAMOANDW   = Bits("b1_11_010_10",8)
  val CMD_VAMOORW    = Bits("b1_11_011_10",8)
  val CMD_VAMOMINW   = Bits("b1_11_100_10",8)
  val CMD_VAMOMAXW   = Bits("b1_11_101_10",8)
  val CMD_VAMOMINUW  = Bits("b1_11_110_10",8)
  val CMD_VAMOMAXUW  = Bits("b1_11_111_10",8)
}
