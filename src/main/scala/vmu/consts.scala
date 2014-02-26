package hwacha
package vmu

import Chisel._
import uncore.constants.MemoryOpConstants._
import uncore.constants.AddressConstants._

trait VMUConstants extends LaneConstants
{
  val SZ_QCNT = SZ_LGBANK1
  val SZ_VMU_ADDR = math.max(PADDR_BITS, VADDR_BITS)
  val SZ_VMU_DATA = 64
  val SZ_VMU_OP = 1 + M_SZ

  val VM_X = Bits.DC(SZ_VMU_OP)

  val VM_VLD = (Bool(true)  ## M_XRD)
  val VM_VST = (Bool(true)  ## M_XWR)
  val VM_ULD = (Bool(false) ## M_XRD)
  val VM_UST = (Bool(false) ## M_XWR)

  val VM_AMO_SWAP = (Bool(false) ## M_XA_SWAP)
  val VM_AMO_ADD  = (Bool(false) ## M_XA_ADD)
  val VM_AMO_XOR  = (Bool(false) ## M_XA_XOR)
  val VM_AMO_OR   = (Bool(false) ## M_XA_OR)
  val VM_AMO_AND  = (Bool(false) ## M_XA_AND)
  val VM_AMO_MIN  = (Bool(false) ## M_XA_MIN)
  val VM_AMO_MAX  = (Bool(false) ## M_XA_MAX)
  val VM_AMO_MINU = (Bool(false) ## M_XA_MINU)
  val VM_AMO_MAXU = (Bool(false) ## M_XA_MAXU)

  def vmu_op_tvec(op: Bits) = op(M_SZ)
  def vmu_op_mcmd(op: Bits) = op(M_SZ-1, 0)

  def is_mcmd_load(cmd: Bits) = (cmd === M_XRD)
  def is_mcmd_store(cmd: Bits) = (cmd === M_XWR)
  def is_mcmd_amo(cmd: Bits) = isAMO(cmd)
  def is_mcmd_pfr(cmd: Bits) = (cmd === M_PFR)
  def is_mcmd_pfw(cmd: Bits) = (cmd === M_PFW)
  def is_mcmd_pf(cmd: Bits) = (is_mcmd_pfr(cmd) || is_mcmd_pfw(cmd))

  def is_mtype_byte(typ: Bits) = (typ === MT_B || typ === MT_BU)
  def is_mtype_halfword(typ: Bits) = (typ === MT_H || typ === MT_HU)
  def is_mtype_word(typ: Bits) = (typ === MT_W || typ === MT_WU)
  def is_mtype_doubleword(typ: Bits) = (typ === MT_D)
}

// TODO: Remove after integration
object Constants extends
  MachineConstants with
  VectorCommandQueueConstants with
  LaneConstants with
  VMUConstants

