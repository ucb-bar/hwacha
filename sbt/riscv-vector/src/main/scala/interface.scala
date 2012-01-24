package riscvVector {
  import Chisel._
  import Node._

  object Interface {
      val VLENMAX_SZ  = 11;

      // vcmdq
      val VCMD_SZ      = 20;

      val VCMD_CMD_SZ  = 8;
      val VCMD_VD_SZ   = 6;
      val VCMD_VS_SZ   = 6;
      
      val VCMD_CMCODE  = (19,12);
      val VCMD_VD      = (11,6);
      val VCMD_VS      = (5,0);
      
      // vimmq
      val VIMM_SZ      = 64;
    /*
      val VIMM_DATA     63:0
    */
      // vstrideq
      val VSTRIDE_SZ   = 32;
    /*
      val VSTRIDE_DATA  31:0
    */
      // vrespq
      val VRESP_SZ     = 32;

      // xcmdq
      val XCMD_SZ      = 20;
    /*
      val XCMD_CMCODE   19:12
      val XCMD_VD       11:6
      val XCMD_VS       5:0
    */
      val XCMD_CMD_SZ  = 8;
      val XCMD_VD_SZ   = 6;
      val XCMD_VS_SZ   = 6;

      // ximmq
      val XIMM_SZ      = 64;
    /*
      val XIMM_DATA     63:0
    */
      // xrespq
      val XRESP_SZ     = 1;

      // xfcmdq

      val XFCMD_SZ     = 68;
    /*
      val XFCMD_OP      67:64
      val XFCMD_RS      63:32
      val XFCMD_RT      31:0
    */
      val XFCMD_OP_SZ  = 4;
      val XFCMD_RS_SZ  = 32;
      val XFCMD_RT_SZ  = 32;

      // xfrespq
      val XFRESP_SZ    = 32;

      // xf interface
      val XFOP_FADD     = UFix(0, 4);
      val XFOP_FSUB     = UFix(1, 4);
      val XFOP_IDIV     = UFix(2, 4);
      val XFOP_IREM     = UFix(3, 4);
      val XFOP_IDIVU    = UFix(4, 4);
      val XFOP_IREMU    = UFix(5, 4);
      val XFOP_FDIV     = UFix(6, 4);
      val XFOP_IMUL     = UFix(7, 4);
      val XFOP_FMUL     = UFix(8, 4);
      val XFOP_FSQRT    = UFix(9, 4);
      val XFOP_CVTSW    = UFix(10, 4);
      val XFOP_CVTWS    = UFix(11, 4);
      val XFOP_CEIL     = UFix(12, 4);
      val XFOP_FLOOR    = UFix(13, 4);
      val XFOP_ROUND    = UFix(14, 4);
      val XFOP_TRUNC    = UFix(15, 4);

      // vmcmdq
      val VMCMD_SZ     = 19;
      
      val VMCMD_CMDCODE = (18,11);
      val VMCMD_VLEN_M1 = (10, 0);
      
      val VMCMD_CMD_SZ = 8;
      val VMCMD_VLEN_SZ = VLENMAX_SZ;

      // vimmq
      val VMIMM_SZ       = 32;
    /*
      val VMIMM_DATA      31:0
    */
      // vmstrideq
      val VMSTRIDE_SZ    = 32;
    /*
      val VMSTRIDE_DATA   31:0
    */
      // vmresp
      val VMRESP_SZ      = 1;

      val VM_STCMD_SZ = 4+VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ;
      val VM_ISCMD_SZ = VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ;
      val VM_WBCMD_SZ = 4+VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ;

      // utmcmdq
      val UTMCMD_SZ      = 19;
      val UTMCMD_CMDCODE  = (18,11);
      val UTMCMD_VLEN_M1  = (10,0);
      val UTMCMD_CMD_SZ  = 8;
      val UTMCMD_VLEN_SZ = VLENMAX_SZ;

      // utmimmq
      val UTMIMM_SZ      = 32;
    /*
      val UTMIMM_DATA     31:0
    */
      // utmrespq
      val UTMRESP_SZ     = 1;

      val UT_ISCMD_SZ = UTMIMM_SZ+UTMCMD_VLEN_SZ;
      val UT_WBCMD_SZ = 5+UTMCMD_VLEN_SZ; // add amo
      val UT_STCMD_SZ = 6+UTMIMM_SZ+UTMCMD_VLEN_SZ; // add amo
    /*
      val DEF_VXU_CMDQ [`XCMD_SZ-1:0]
      val DEF_VXU_IMMQ [`XIMM_SZ-1:0]
      val DEF_VXU_ACKQ [`XRESP_SZ-1:0]
      val DEF_VMU_VCMDQ [`VMCMD_SZ-1:0]
      val DEF_VMU_VBASEQ [`VMIMM_SZ-1:0]
      val DEF_VMU_VSTRIDEQ [`VMSTRIDE_SZ-1:0]
      val DEF_VMU_VACKQ [`VMRESP_SZ-1:0]
      val DEF_VMU_UTCMDQ [`UTMCMD_SZ-1:0]
      val DEF_VMU_UTIMMQ [`UTMIMM_SZ-1:0]
      val DEF_VMU_UTACKQ [`UTMRESP_SZ-1:0]
    */
  }

}
