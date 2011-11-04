package riscvVector {
    import Chisel._
    import Node._

    /*
    class TopIo extends Bundle {
        val io = Bool('input); // Here temporarily just so this will compile
    }

    class Top extends Component {
        override val io = new TopIo();
        val vuVMU_BHWDsel = new vuVMU_BHWD_sel();
        val vuVMU_Ctrl_ut_issue = new vuVMU_Ctrl_ut_issue();
    }
    */

    object top_main {
        def main(args: Array[String]) = {
            val boot_args = args ++ Array("--target-dir", "generated-src");
            chiselMain(
                boot_args,
                () => new vuVMU_Ctrl_ut_store() //vuVMU_Ctrl_ut_issue()
            );
    } 
}
}  
