package hwacha

import Chisel._
import Node._
import hardfloat._
import Constants._

object top_main
{
  def main(args: Array[String]) =
  {
    val boot_args = args ++ Array("--target-dir", "generated-src")
    //chiselMain(boot_args, () => new vuVXU())
    chiselMain(boot_args, () => new vu())
    //chiselMain(boot_args, () => new vuVXU_Banked8_Expand)
    //chiselMain(boot_args, () => new vuVXU_Banked8_Fire)
    //chiselMain(boot_args, () => new vuVXU_Banked8_Seq)
    //chiselMain(boot_args, () => new vuVXU_Banked8_Hazard)
    //chiselMain(boot_args, () => new vuVXU_Banked8_FU_fma())
    //chiselMain(boot_args, () => new vuVXU_Banked8_FU_imul())
    //chiselMain(boot_args, () => new vuVXU_Banked8_FU_conv())
    //chiselMain(boot_args, () => new vuVXU_Banked8_FU_alu())
    //chiselMain(boot_args, () => new vuVXU_Banked8_Bank())
    //chiselMain(boot_args, () => new vuVXU_Banked8_Lane())
    //chiselMain(boot_args, () => new vuVXU_Issue())
    //chiselMain(boot_args, () => new queue_reorder_qcnt(65, 256, 9))
  } 
}
