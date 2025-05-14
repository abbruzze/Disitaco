package ucesoft.disitaco.cpu

import java.util

object Registers:
  inline final val F_CARRY      = 1 << 0
  inline final val F_PARITY     = 1 << 2
  inline final val F_AUX_CARRY  = 1 << 4
  inline final val F_ZERO       = 1 << 6
  inline final val F_SIGN       = 1 << 7
  inline final val F_TRAP       = 1 << 8
  inline final val F_INT        = 1 << 9
  inline final val F_DIRECTION  = 1 << 10
  inline final val F_OVERFLOW   = 1 << 11

  inline val REP = 0
  inline val REPN = 1
/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/02/2025 16:03  
 */
class Registers:
  import Registers.*
  // REG
  private inline val AX = 0
  private inline val BX = 1
  private inline val CX = 2
  private inline val DX = 3
  private inline val SP = 4
  private inline val BP = 5
  private inline val SI = 6
  private inline val DI = 7
  private inline val IP = 8
  
  private inline val CS = 0
  private inline val DS = 1
  private inline val SS = 2
  private inline val ES = 3

  private inline final val F_MASK = F_CARRY | F_PARITY | F_AUX_CARRY | F_ZERO | F_SIGN | F_TRAP | F_INT | F_DIRECTION | F_OVERFLOW
  private inline final val F_DEFAULT_MASK = 0xF002

  private final val regs = Array.ofDim[Int](9)
  private final val segments = Array.ofDim[Int](4)
  private var flags = 0
  private var undefinedFlagMask = 0xFFFF

  private var segmentOverrideReg = -1
  private var repetition = -1
  private var repetitionIP = 0
  
  private inline def low(r:Int): Int = regs(r) & 0xFF
  private inline def hi(r:Int): Int = (regs(r) >> 8) & 0xFF
  private inline def low(r:Int,value:Int): Unit = regs(r) = (regs(r) & 0xFF00) | (value & 0xFF)
  private inline def hi(r:Int,value:Int): Unit = regs(r) = (regs(r) & 0x00FF) | (value & 0xFF) << 8
  private def address(segment:Int,offset:Int,ignoreSegmentOverride:Boolean = false): Int =
    val seg = if segmentOverrideReg != -1 && !ignoreSegmentOverride then
      segmentOverrideReg match
        case 0 => segments(ES)
        case 1 => segments(CS)
        case 2 => segments(SS)
        case 3 => segments(DS)
    else
      segment
    (seg & 0xFFFF) << 16 | (offset & 0xFFFF)

  final def reset(): Unit =
    util.Arrays.fill(regs,0)
    util.Arrays.fill(segments, 0)
    segments(CS) = 0xF000
    regs(IP) = 0xFFF0
    flags = 0
    segmentOverrideReg = -1
    repetition = -1
    undefinedFlagMask = 0xFFFF
    
  final def undefinedFlags_=(mask:Int): Unit = undefinedFlagMask = mask ^ 0xFFFF
  final def undefinedFlags: Int = undefinedFlagMask
  final def clearUndefinedFlags(): Unit = undefinedFlagMask = 0xFFFF

  final def setSegmentOverride(soIndex:Int): Unit = segmentOverrideReg = soIndex & 3
  final def clearSegmentOverride(): Unit = segmentOverrideReg = -1
  final def isSegmentOverride: Boolean = segmentOverrideReg != -1

  final def setRep(): Unit = 
    repetition = REP
    repetitionIP = regs(IP) - 1
  final def setRepN(): Unit = 
    repetition = REPN
    repetitionIP = regs(IP) - 1
  final def clearRep(): Unit = repetition = -1
  final def isRep: Boolean = repetition != -1
  final def getRep: Int = repetition
  final def getRepIP: Int = repetitionIP
  
  final def al : Int = low(AX)
  final def al_=(value:Int): Unit = low(AX,value)
  final def ah : Int = hi(AX)
  final def ah_=(value:Int): Unit = hi(AX,value)
  final def ax: Int = regs(AX)
  final def ax_=(value:Int): Unit = regs(AX) = value & 0xFFFF

  final def bl: Int = low(BX)
  final def bl_=(value:Int): Unit = low(BX,value)
  final def bh: Int = hi(BX)
  final def bh_=(value:Int): Unit = hi(BX,value)
  final def bx: Int = regs(BX)
  final def bx_=(value: Int): Unit = regs(BX) = value & 0xFFFF

  final def cl: Int = low(CX)
  final def cl_=(value:Int): Unit = low(CX,value)
  final def ch: Int = hi(CX)
  final def ch_=(value:Int): Unit = hi(CX,value)
  final def cx: Int = regs(CX)
  final def cx_=(value: Int): Unit = regs(CX) = value & 0xFFFF


  final def dl: Int = low(DX)
  final def dl_=(value:Int): Unit = low(DX,value)
  final def dh: Int = hi(DX)
  final def dh_=(value:Int): Unit = hi(DX,value)
  final def dx: Int = regs(DX)
  final def dx_=(value: Int): Unit = regs(DX) = value & 0xFFFF

  final def sp: Int = regs(SP)
  final def sp_=(value: Int): Unit = regs(SP) = value & 0xFFFF

  final def bp: Int = regs(BP)
  final def bp_=(value:Int): Unit = regs(BP) = value & 0xFFFF

  final def si: Int = regs(SI)
  final def si_=(value: Int): Unit = regs(SI) = value & 0xFFFF

  final def di: Int = regs(DI)
  final def di_=(value: Int): Unit = regs(DI) = value & 0xFFFF

  final def ip: Int = regs(IP)
  final def ip_=(value: Int): Unit = regs(IP) = value & 0xFFFF
  final def incIP(value:Int): Unit = regs(IP) = (regs(IP) + value) & 0xFFFF

  final def cs: Int = segments(CS)
  final def cs(offset:Int,ignoreSegmentOverride:Boolean = false): Int = address(segments(CS),offset,ignoreSegmentOverride)
  final def cs_=(value:Int): Unit = segments(CS) = value & 0xFFFF

  final def ds: Int = segments(DS)
  final def ds(offset: Int): Int = address(segments(DS), offset)
  final def ds_=(value: Int): Unit = segments(DS) = value & 0xFFFF

  final def es: Int = segments(ES)
  final def es(offset: Int,ignoreSegmentOverride:Boolean = false): Int = address(segments(ES), offset,ignoreSegmentOverride)
  final def es_=(value: Int): Unit = segments(ES) = value & 0xFFFF

  final def ss: Int = segments(SS)
  final def ss(offset: Int): Int = address(segments(SS), offset)
  final def ss_=(value: Int): Unit = segments(SS) = value & 0xFFFF
  
  final def absoluteIP: Int = ((segments(CS) << 4) + regs(IP)) & 0xF_FFFF

  final def setFlags(bits:Int,sets:Int): Unit =
    flags &= ~bits
    flags |= sets
  final def setFlags(sets:Int): Unit = flags |= sets
  final def setFlag(flag:Int,set:Boolean): Unit =
    if set then flags |= flag else flags &= ~flag
  final def clearFlags(clears:Int): Unit = flags &= ~clears
  final def isFlags(fs:Int): Boolean = (flags & fs) == fs
  final def getFlagWord: Int = (flags & F_MASK) | F_DEFAULT_MASK
  final def setFlagWord(word:Int): Unit = flags = word

  final def getSeg(index:Int): Int =
    index & 3 match
      case 0 => segments(ES)
      case 1 => segments(CS)
      case 2 => segments(SS)
      case 3 => segments(DS)
  final def setSeg(index:Int,value:Int): Unit =
    index & 3 match
      case 0 => segments(ES) = value & 0xFFFF
      case 1 => segments(CS) = value & 0xFFFF
      case 2 => segments(SS) = value & 0xFFFF
      case 3 => segments(DS) = value & 0xFFFF

  final def get8(index:Int): Int =
    index & 7 match
      case 0 => low(AX)
      case 1 => low(CX)
      case 2 => low(DX)
      case 3 => low(BX)
      case 4 => hi(AX)
      case 5 => hi(CX)
      case 6 => hi(DX)
      case 7 => hi(BX)
  final def set8(index:Int,value:Int): Unit =
    index & 7 match
      case 0 => low(AX,value)
      case 1 => low(CX,value)
      case 2 => low(DX,value)
      case 3 => low(BX,value)
      case 4 => hi(AX,value)
      case 5 => hi(CX,value)
      case 6 => hi(DX,value)
      case 7 => hi(BX,value)
  final def get16(index:Int): Int =
    index & 7 match
      case 0 => ax
      case 1 => cx
      case 2 => dx
      case 3 => bx
      case 4 => sp
      case 5 => bp
      case 6 => si
      case 7 => di
  final def set16(index:Int,value:Int): Unit =
    index & 7 match
      case 0 => ax = value
      case 1 => cx = value
      case 2 => dx = value
      case 3 => bx = value
      case 4 => sp = value
      case 5 => bp = value
      case 6 => si = value
      case 7 => di = value

  final def push(word:Int,mem:Memory): Unit =
    regs(SP) = (regs(SP) - 2) & 0xFFFF
    mem.writeWord(address(segments(SS),regs(SP),ignoreSegmentOverride = true),word)
  final def pop(mem:Memory): Int =
    val popped = mem.readWord(address(segments(SS),regs(SP),ignoreSegmentOverride = true))
    regs(SP) = (regs(SP) + 2) & 0xFFFF
    popped

  final def flagsToString(onlySet:Boolean): String = flagsToString(flags,onlySet)
  final def flagsToString(f:Int,onlySet:Boolean = false): String =
    val sb = new StringBuilder()
    var b = 15
    while b >= 0 do
      val c = 1 << b match
        case F_OVERFLOW => if (f & F_OVERFLOW) != 0 then "O" else if onlySet then "_" else "o"
        case F_DIRECTION => if (f & F_DIRECTION) != 0 then "D" else if onlySet then "_" else "d"
        case F_INT => if (f & F_INT) != 0 then "I" else if onlySet then "_" else "i"
        case F_TRAP => if (f & F_TRAP) != 0 then "T" else if onlySet then "_" else "t"
        case F_SIGN => if (f & F_SIGN) != 0 then "S" else if onlySet then "_" else "s"
        case F_ZERO => if (f & F_ZERO) != 0 then "Z" else if onlySet then "_" else "z"
        case F_AUX_CARRY => if (f & F_AUX_CARRY) != 0 then "A" else if onlySet then "_" else "a"
        case F_PARITY => if (f & F_PARITY) != 0 then "P" else if onlySet then "_" else "p"
        case F_CARRY => if (f & F_CARRY) != 0 then "C" else if onlySet then "_" else "c"
        case _ => "_"
      sb.append(c)
      b -= 1
    sb.toString()
    
  override def toString: String =
    val sb = new StringBuilder()
    for s <- 0 to 3 do 
      val seg = s match
        case 0 => "CS"
        case 1 => "DS"
        case 2 => "SS"
        case 3 => "ES"
      sb.append("%s=%04X ".format(seg,segments(s)))
    sb.append("\n")
    for r <- 0 to 7 do
      val reg = r match
        case 0 => "AX"
        case 1 => "BX"
        case 2 => "CX"
        case 3 => "DX"
        case 4 => "SP"
        case 5 => "BP"
        case 6 => "SI"
        case 7 => "DI"
      sb.append("%s=%04X ".format(reg, regs(r)))
      if r == 3 then sb.append("\n")
    sb.append("\n")
    sb.append("IP=%04X FLAGS=%s\n".format(regs(IP),flagsToString(false)))
    sb.toString