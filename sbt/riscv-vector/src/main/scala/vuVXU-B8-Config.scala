package riscvVector
{

import Chisel._;
import Node._;
import scala.collection.mutable.ArrayBuffer;
import scala.math._;

object Config
{
  val INT_STAGES   = 2
  val IMUL_STAGES  = 3
  val FMA_STAGES   = 5
  val FCONV_STAGES = 3

  val SHIFT_BUF_READ = 3
  val SHIFT_BUF_WRITE = FMA_STAGES + 4

  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b0", 1)
  val RX = Bits("b0", 1)
  val RF = Bits("b1", 1)

  val I_ = UFix(0, 2)
  val II = UFix(0, 2)
  val IB = UFix(1, 2)
  val IL = UFix(2, 2)

  val DW__ = Bits("b0", 1)
  val DW32 = Bits("b0", 1)
  val DW64 = Bits("b1", 1)

  val FP_ = Bits("b0", 1)
  val FPS = Bits("b0", 1)
  val FPD = Bits("b1", 1)

  val VIU_X     = Bits(0, 5)
  val VIU_ADD   = Bits(1, 5)
  val VIU_SLL   = Bits(2, 5)
  val VIU_SLT   = Bits(3, 5)
  val VIU_SLTU  = Bits(4, 5)
  val VIU_XOR   = Bits(5, 5)
  val VIU_SRL   = Bits(6, 5)
  val VIU_SRA   = Bits(7, 5)
  val VIU_OR    = Bits(8, 5)
  val VIU_AND   = Bits(9, 5)
  val VIU_SUB   = Bits(10, 5)
  val VIU_IDX   = Bits(11, 5)
  val VIU_MOV   = Bits(12, 5)
  val VIU_FSJ   = Bits(13, 5)
  val VIU_FSJN  = Bits(14, 5)
  val VIU_FSJX  = Bits(15, 5)
  val VIU_FEQ   = Bits(16, 5)
  val VIU_FLT   = Bits(17, 5)
  val VIU_FLE   = Bits(18, 5)
  val VIU_FMIN  = Bits(19, 5)
  val VIU_FMAX  = Bits(20, 5)
  val VIU_MOVZ  = Bits(21, 5)
  val VIU_MOVN  = Bits(22, 5)

  // in the decode table
  val VAU0_X     = Bits(0, 2)
  val VAU0_M     = Bits(0, 2)
  val VAU0_MH    = Bits(1, 2)
  val VAU0_MHU   = Bits(2, 2)
  val VAU0_MHSU  = Bits(3, 2)

  // acutal ops
  val VAU0_32    = Cat(DW32,VAU0_M)
  val VAU0_32H   = Cat(DW32,VAU0_MH)
  val VAU0_32HU  = Cat(DW32,VAU0_MHU)
  val VAU0_32HSU = Cat(DW32,VAU0_MHSU)
  val VAU0_64    = Cat(DW64,VAU0_M)
  val VAU0_64H   = Cat(DW64,VAU0_MH)
  val VAU0_64HU  = Cat(DW64,VAU0_MHU)
  val VAU0_64HSU = Cat(DW64,VAU0_MHSU)

  val VAU1_X     = UFix(0, 3)
  val VAU1_ADD   = UFix(0, 3)
  val VAU1_SUB   = UFix(1, 3)
  val VAU1_MUL   = UFix(2, 3)
  val VAU1_MADD  = UFix(4, 3)
  val VAU1_MSUB  = UFix(5, 3)
  val VAU1_NMSUB = UFix(6, 3)
  val VAU1_NMADD = UFix(7, 3)

  val VAU2_X     = Bits("b0000", 4)
  val VAU2_CLTF  = Bits("b0000", 4)
  val VAU2_CLUTF = Bits("b0001", 4)
  val VAU2_CWTF  = Bits("b0010", 4)
  val VAU2_CWUTF = Bits("b0011", 4)
  val VAU2_MXTF  = Bits("b0100", 4)
  val VAU2_CFTL  = Bits("b1000", 4)
  val VAU2_CFTLU = Bits("b1001", 4)
  val VAU2_CFTW  = Bits("b1010", 4)
  val VAU2_CFTWU = Bits("b1011", 4)
  val VAU2_MFTX  = Bits("b1100", 4)
  val VAU2_CDTS  = Bits("b1110", 4)
  val VAU2_CSTD  = Bits("b1111", 4)

  val SZ_ADDR = 32
  val SZ_INST = 32
  val SZ_DATA = 65
  val SZ_EXC = 5
  val SZ_XLEN = 64
  val SZ_FLEN = 65

  val DEF_ADDR = SZ_ADDR
  val DEF_INST = SZ_INST
  val DEF_DATA = SZ_DATA // data width
  val DEF_EXC  = SZ_EXC
  val DEF_XLEN = SZ_XLEN
  val DEF_FLEN = SZ_FLEN

  val SZ_STALL = 5

  val RG_VLDQ = 4
  val RG_VSDQ = 3
  val RG_UTAQ = 2
  val RG_UTLDQ = 1
  val RG_UTSDQ = 0

  val DEF_STALL = SZ_STALL

  val SZ_VIU_FN  = 11
  val SZ_VAU0_FN = 3
  val SZ_VAU1_FN = 6
  val SZ_VAU2_FN = 7

  val RG_VIU_T  = (10,7);
  val RG_VIU_T0 = (10,9);
  val RG_VIU_T1 = (8,7);
  val RG_VIU_DW = 6 
  val RG_VIU_FP = 5
  val RG_VIU_FN = (4,0);

  val RG_VAU1_FP = 5
  val RG_VAU1_RM = (4,3);
  val RG_VAU1_FN = (2,0);
  val RG_VAU2_FP = 6
  val RG_VAU2_RM = (5,4);
  val RG_VAU2_FN = (3,0);


  val DEF_VIU_FN  = SZ_VIU_FN
  val DEF_VAU0_FN = SZ_VAU0_FN
  val DEF_VAU1_FN = SZ_VAU1_FN
  val DEF_VAU2_FN = SZ_VAU2_FN

  val SZ_VLEN = 11
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val DEF_VLEN   = SZ_VLEN
  val DEF_REGLEN = SZ_REGLEN
  val DEF_REGCNT = SZ_REGCNT

  val SZ_BANK = 8
  val SZ_LGBANK = 3
  val SZ_LGBANK1 = 4
  val SZ_BVLEN = 3
  val SZ_BREGLEN = 8
  val SZ_BOPL = 2
  val SZ_BRPORT = SZ_BANK
  val SZ_BWPORT = 3

  val DEF_BANK    = SZ_BANK
  val DEF_BPTR    = SZ_LGBANK
  val DEF_BPTR1   = SZ_LGBANK+1
  val DEF_BPTR2   = SZ_LGBANK+2
  val DEF_BCNT    = SZ_LGBANK+1
  val DEF_BVLEN   = SZ_BVLEN
  val DEF_BREGLEN = SZ_BREGLEN
  val DEF_BOPL    = SZ_BOPL
  val DEF_BRPORT  = SZ_BRPORT
  val DEF_BWPORT  = SZ_BWPORT

  val VLENMAX_SZ     = 11
  val VCMD_SZ        = 20
  val VCMD_CMD_SZ    = 8
  val VCMD_VD_SZ     = 6
  val VCMD_VS_SZ     = 6
  val VIMM_SZ        = 64
  val VSTRIDE_SZ     = 32
  val VRESP_SZ       = 32
  val XCMD_SZ        = 20
  val XCMD_CMD_SZ    = 8
  val XCMD_VD_SZ     = 6
  val XCMD_VS_SZ     = 6
  val XIMM_SZ        = 64
  val XRESP_SZ       = 1
  val XFCMD_SZ       = 68
  val XFCMD_OP_SZ    = 4
  val XFCMD_RS_SZ    = 32
  val XFCMD_RT_SZ    = 32
  val XFRESP_SZ      = 32
  val VMCMD_SZ       = 19
  val VMCMD_CMD_SZ   = 8
  val VMCMD_VLEN_SZ  = VLENMAX_SZ
  val VMIMM_SZ       = 32
  val VMSTRIDE_SZ    = 32
  val VMRESP_SZ      = 1
  val VM_STCMD_SZ    = 4+VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ
  val VM_ISCMD_SZ    = VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ
  val VM_WBCMD_SZ    = 4+VMCMD_VLEN_SZ+VMIMM_SZ+VMSTRIDE_SZ
  val UTMCMD_SZ      = 19
  val UTMCMD_CMD_SZ  = 8
  val UTMCMD_VLEN_SZ = VLENMAX_SZ
  val UTMIMM_SZ      = 32
  val UTMRESP_SZ     = 1

  val XCMD_CMCODE  = (19,12);
  val XCMD_VD      = (11,6);
  val XCMD_VS      = (5,0);

  val DEF_VXU_CMDQ = XCMD_SZ
  val DEF_VXU_IMMQ = XIMM_SZ
  val DEF_VXU_ACKQ = XRESP_SZ
  val DEF_VMU_VCMDQ = VMCMD_SZ
  val DEF_VMU_VBASEQ = VMIMM_SZ
  val DEF_VMU_VSTRIDEQ = VMSTRIDE_SZ
  val DEF_VMU_VACKQ = VMRESP_SZ
  val DEF_VMU_UTCMDQ = UTMCMD_SZ
  val DEF_VMU_UTIMMQ = UTMIMM_SZ
  val DEF_VMU_UTACKQ = UTMRESP_SZ
}

object Match
{
  def apply(x: Bits, IOs: Bits*) =
  {
    val ioList = IOs.toList;
    var offset = 0;
    for (io <- IOs.toList.reverse)
    {
      io := x(offset+io.width-1, offset)
      offset += io.width;
    }
  }
}

object Reverse
{
  def apply(in: Bits): Bits =
  {
    var res = in(0);
    for(i <- 1 until 64)
      res = Cat(res, in(i));
    res
  }
}

object ShiftRegister
{
  def apply(n: Int, width: Int, valid: Bool, base: Bits): Bits =
  {
    if (n == 0)
    {
      val res = Reg() { Bits(width = width) };
      when (valid)
      {
        res <== base
      }
      res
    }
    else
    {
      Reg(apply(n-1, width, valid, base));
    }
  }
}

object log2up
{
  def apply(in: Int) = if (in == 1) 1 else ceil(log(in)/log(2)).toInt
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    val out = Bits(1, width)
    (out << in)(width-1,0);
  }
}

class Mux1H(n: Int, w: Int) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in = Vec(n) { Bits(width = w, dir = INPUT) }
    val out = Bits(width = w, dir = OUTPUT)
  }

  if (n > 1) {
    var out = io.in(0) & Fill(w, io.sel(0))
    for (i <- 1 to n-1)
      out = out | (io.in(i) & Fill(w, io.sel(i)))
    io.out := out
  } else {
    io.out := io.in(0)
  }
}

object GenArray{
  def apply[T <: Data](n: Int)(gen: => T): GenArray[T] = 
  {
    val res = new GenArray[T];
    for(i <- 0 until n)
      res += gen;
    res.width = res(0).width;
    if (res.width == -1) throw new Exception();
    res
  }
}
object GenBuf{
  def apply[T <: Data](n: Int)(gen: => GenArray[T]): ArrayBuffer[GenArray[T]] = 
    {
      val res = new ArrayBuffer[GenArray[T]];
      for(i <- 0 until n)
	res += gen;
      res
    }
}

class GenArray[T <: Data] extends ArrayBuffer[T] {
  var width = 0;

  def write(addr: UFix, data: T) = {
    if(data.isInstanceOf[Node]){

      val onehot = UFixToOH(addr, length);
      for(i <- 0 until length){
	conds.push(conds.top && onehot(i).toBool);
	this(i).comp procAssign data.toNode;
	conds.pop;
      }
    }
  }

  def write(addr: Bits, data: T): Unit = {
    write(addr.toUFix, data);
  }

  def read(addr: UFix): T = {
    val mux1h = new Mux1H(length, width);
    val onehot = UFixToOH(addr, length);
    for(i <- 0 until length){
      mux1h.io.sel(i) := onehot(i).toBool;
      mux1h.io.in(i)  assign this(i);
    }
    val res = this(0).clone;
    res.setIsCellIO;
    res assign mux1h.io.out;
    res
  }

  def flatten(): Bits = {
    var res: Bits = null;
    for(i <- 0 until length)
      res = Cat(this(i), res)
    res
  }
}
  
}
