package ucesoft.disitaco.cpu

import ucesoft.disitaco.cpu.Instruction.Disassembled
import ucesoft.disitaco.cpu.i8088.ESCHandler

import scala.compiletime.uninitialized

object Instruction:
  case class Disassembled(address:Int,bytes:List[Int],mnemonic:InstructionCode,op1:Option[String] = None,op2:Option[String] = None,rep:Option[InstructionCode] = None,repSeg:Option[String] = None,far:Boolean = false):
    val size : Int = bytes.length

    def getDisassembled: String =
      val sb = new StringBuilder()
      sb.append("%05X ".format(address))
      val bs = bytes.map(b => "%02X".format(b)).mkString
      sb.append(bs)
      sb.append(" " * (18 - bs.length))
      rep match
        case Some(r) =>
          sb.append(r)
          sb.append(" ")
          repSeg match
            case Some(seg) =>
              sb.append(seg)
            case None =>
        case None =>
      sb.append(mnemonic)
      if far then
        sb.append(" FAR")
      op1 match
        case Some(o1) =>
          sb.append(s" $o1")
          op2 match
            case Some(o2) =>
              sb.append(s",$o2")
            case None =>
        case None =>
      sb.toString()

/**
 * @author Alessandro Abbruzzetti
 *         Created on 21/02/2025 13:45  
 */
abstract class Instruction(val opcode:Int,val instr:InstructionCode):
  protected class DisInfo(var address:Int,val segmentOverrideIndex:Int,noMRM: Boolean = false):
    private val initialAddress = address
    private val bytes = new collection.mutable.ListBuffer[Int]
    var mrmMode = 0
    var mrmReg = 0
    var mrmRM = 0

    bytes += opcode
    address += 1
    if !noMRM then readMRM()

    def size: Int = address - initialAddress + 1 // + 1 for the opcode
    def getBytes: List[Int] = bytes.toList

    private def readMRM(): Unit =
      val mrm = mem.readByte(address,abs = true)
      bytes += mrm
      mrmMode = (mrm >> 6) & 3
      mrmReg = (mrm >> 3) & 7
      mrmRM = mrm & 7
      address += 1
    def readByte: Int =
      val b = mem.readByte(address,abs = true)
      bytes += b
      address += 1
      b
    def readWord: Int =
      val w = mem.readWord(address,abs = true)
      bytes += w & 0xFF
      bytes += w >> 8
      address += 2
      w
  end DisInfo

  val segmentOverrideIndex = -1
  val repIndex = -1
  val canGenerateSoftwareInterrupt = false

  private val segmentOverrideInstruction = instr == InstructionCode.SEG_CS || instr == InstructionCode.SEG_DS || instr == InstructionCode.SEG_ES || instr == InstructionCode.SEG_SS
  private val repInstruction = instr == InstructionCode.REP || instr == InstructionCode.REPNE
  private val lockInstruction = instr == InstructionCode.LOCK
  protected var regs : Registers = uninitialized
  protected var mem : Memory = uninitialized
  protected var io : InOut = uninitialized
  private final val PARITY = fillPARITY()

  private var mrmMode = 0
  private var mrmReg = 0
  private var mrmRM = 0
  private var mrmLoaded = false

  private var effectiveAddress = 0

  private var wordAccessOnOddAddressPenalty = false

  // handlers
  protected var escHandler : ESCHandler = uninitialized

  private def fillPARITY(): Array[Boolean] =
    val par = Array.ofDim[Boolean](256)
    for b <- 0 to 255 do
      val p1 = (b >> 4) ^ b
      val p2 = (p1 >> 2) ^ p1
      val p3 = (p2 >> 1) ^ p2
      par(b) = (p3 & 1) == 0
    par

  def setESCHandler(escHandler:ESCHandler): Unit =
    this.escHandler = escHandler

  def init(regs:Registers,mem:Memory,io:InOut): Unit =
    this.regs = regs
    this.mem = mem
    this.io = io
  def reset(loadMRM:Boolean): Unit =
    mrmLoaded = loadMRM
    if loadMRM then
      loadMRMByte()
    wordAccessOnOddAddressPenalty = false

  final def isPrefix: Boolean = segmentOverrideInstruction || repInstruction || lockInstruction
  final def isSegmentOverrideInstruction: Boolean = segmentOverrideInstruction
  final def isRepInstruction: Boolean = repInstruction

  protected def isRegMode: Boolean = mrmMode == 3

  final def execute(): Int =
    val cycles = process() + getDefaultCycles
    if !isPrefix && !hasBeenRepeated then
      regs.clearSegmentOverride()
      regs.clearRep()
    cycles
    
  def softwareInterrupt: Int = -1

  def hasBeenRepeated: Boolean = false
  // ===== Abstract methods =======================
  
  protected def process(): Int

  def disassemble(address:Int,segmentOverridePrefix:Int,repPrefix:Int,prefixList:List[Int]): Disassembled

  // =============== Disa =========================
  protected def disSeg(prefixIndex:Int): String =
    prefixIndex & 3 match
      case 0 => "ES"
      case 1 => "CS"
      case 2 => "SS"
      case 3 => "DS"
  protected def disSeg(disInfo:DisInfo): String = disSeg(disInfo.mrmReg)
  protected def disMrmReg(dis: Disassembled,opcode:Int): Int = (dis.bytes(dis.bytes.indexOf(opcode) + 1) >> 3) & 7
  protected def disEA8(disInfo:DisInfo): String = disEffectiveAddress(disInfo,size = 8)
  protected def disEA16(disInfo:DisInfo): String = disEffectiveAddress(disInfo,size = 16)
  protected def disReg8(disInfo: DisInfo): String = disReg(disInfo.mrmReg,size = 8)
  protected def disReg16(disInfo: DisInfo): String = disReg(disInfo.mrmReg,size = 16)
  protected def disImm8(disInfo:DisInfo): String = format(disInfo.readByte,size = 8,noSignExt = true)
  protected def disImm16(disInfo:DisInfo): String = format(disInfo.readWord,size = 8,noSignExt = true)
  protected def disImm(address:Int,segmentOverridePrefix:Int,prefixList:List[Int],op1:String,size:Int): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix,noMRM = true)
    val op2 = format(if size == 8 then dis.readByte else dis.readWord, size = size,noSignExt = true)
    Disassembled(address, prefixList ++ dis.getBytes, instr, Some(op1), Some(op2))
  protected def disImm(address: Int, segmentOverridePrefix: Int, prefixList: List[Int],size: Int,add:Int = 0): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix, noMRM = true)
    val op1 = format(if size == 8 then add + dis.readByte.toByte + prefixList.size else add + dis.readWord.toShort + prefixList.size, size = 20, noSignExt = true)
    Disassembled(address, prefixList ++ dis.getBytes, instr, Some(op1))
  protected def disNoOp(address: Int, segmentOverridePrefix: Int, prefixList: List[Int]): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix, noMRM = true)
    Disassembled(address, prefixList ++ dis.getBytes, instr)
  protected def dis1Op(address:Int,segmentOverridePrefix:Int,prefixList:List[Int],op: DisInfo => String,mrm:Boolean = false): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix,noMRM = !mrm)
    val o1 = op(dis)
    Disassembled(address, prefixList ++ dis.getBytes, instr,Some(o1))
  protected def dis2Ops(address:Int,segmentOverridePrefix:Int,prefixList:List[Int],op1:DisInfo => String,op2:DisInfo => String): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix)
    val o1 = op1(dis)
    val o2 = op2(dis)
    Disassembled(address, prefixList ++ dis.getBytes, instr, Some(o1), Some(o2))
  protected def disFarImm(address: Int, segmentOverridePrefix: Int, prefixList: List[Int]): Disassembled =
    val dis = new DisInfo(address, segmentOverridePrefix, noMRM = true)
    val ofs = dis.readWord
    val seg = dis.readWord
    Disassembled(address, prefixList ++ dis.getBytes, instr, Some(s"${format(seg,size = 16,noSignExt = true)}:${format(ofs,size = 16,noSignExt = true)}"))

  inline private def format(i:Int,size:Int,noSignExt:Boolean = false):String =
    size match
      case 8 =>
        if noSignExt then
          "%02Xh".format(i)
        else
          val byte = i.toByte
          if byte < 0 then "-%02Xh".format(-byte) else "+%02Xh".format(byte)
      case 16 =>
        val word = i.toShort
        if noSignExt then
          "%04Xh".format(word)
        else if word < 0 then "-%04Xh".format(-word) else "+%04Xh".format(word)
      case 20 =>
        "%05Xh".format(i)

  private def disEffectiveAddress(disInfo: DisInfo,size:Int): String =
    val segOverride = if disInfo.segmentOverrideIndex == -1 then ""
    else
      disInfo.segmentOverrideIndex match
        case 0 => "ES:"
        case 1 => "CS:"
        case 2 => "SS:"
        case 3 => "DS:"
    val sizeHint = if size == 8 then "BYTE PTR " else "WORD PTR "
    disInfo.mrmMode match
      case 0 => // No Displacement
        disInfo.mrmRM match
          case 0 => // (BX) + (SI)
            s"$segOverride$sizeHint[BX+SI]"
          case 1 => // (BX) + (DI)
            s"$segOverride$sizeHint[BX+DI]"
          case 2 => // (BP) + (SI)
            s"$segOverride$sizeHint[BP+SI]"
          case 3 => // (BP) + (DI)
            s"$segOverride$sizeHint[BP+DI]"
          case 4 => // (SI)
            s"$segOverride$sizeHint[SI]"
          case 5 => // (DI)
            s"$segOverride$sizeHint[DI]"
          case 6 => // 16 bit displacement
            s"$segOverride[${format(disInfo.readWord,size = 16,noSignExt = true)}]"
          case 7 => // (BX)
            s"$segOverride$sizeHint[BX]"
      case 1 => // 8-bit displacement
        val disp8 = format(disInfo.readByte,size = 8)
        disInfo.mrmRM match
          case 0 => // (BX) + (SI) + disp8
            s"$segOverride$sizeHint[BX+SI$disp8]"
          case 1 => // (BX) + (DI) + disp8
            s"$segOverride$sizeHint[BX+DI$disp8]"
          case 2 => // (BP) + (SI) + disp8
            s"$segOverride$sizeHint[BP+SI$disp8]"
          case 3 => // (BP) + (DI) + disp8
            s"$segOverride$sizeHint[BP+DI$disp8]"
          case 4 => // (SI) + disp8
            s"$segOverride$sizeHint[SI$disp8]"
          case 5 => // (DI) + disp8
            s"$segOverride$sizeHint[DI$disp8]"
          case 6 => // (BP) + disp8
            s"$segOverride$sizeHint[BP$disp8]"
          case 7 => // (BX) + disp8
            s"$segOverride$sizeHint[BX$disp8]"
      case 2 => // 16-bit displacement
        val disp16 = format(disInfo.readWord,size = 16)
        disInfo.mrmRM match
          case 0 => // (BX) + (SI) + disp16
            s"$segOverride$sizeHint[BX+SI$disp16]"
          case 1 => // (BX) + (DI) + disp16
            s"$segOverride$sizeHint[BX+DI$disp16]"
          case 2 => // (BP) + (SI) + disp16
            s"$segOverride$sizeHint[BP+SI$disp16]"
          case 3 => // (BP) + (DI) + disp16
            s"$segOverride$sizeHint[BP+DI$disp16]"
          case 4 => // (SI) + disp16
            s"$segOverride$sizeHint[SI$disp16]"
          case 5 => // (DI) + disp16
            s"$segOverride$sizeHint[DI$disp16]"
          case 6 => // (BP) + disp16
            s"$segOverride$sizeHint[BP$disp16]"
          case 7 => // (BX) + disp16
            s"$segOverride$sizeHint[BX$disp16]"
      case 3 => // Register operand
        disReg(disInfo.mrmRM,size)
  end disEffectiveAddress

  protected def disReg(_reg:Int,size:Int): String =
    var reg = _reg
    if size == 16 then reg += 8
    reg match
      case 0 => "AL"
      case 1 => "CL"
      case 2 => "DL"
      case 3 => "BL"
      case 4 => "AH"
      case 5 => "CH"
      case 6 => "DH"
      case 7 => "BH"
      case 8 => "AX"
      case 9 => "CX"
      case 10 => "DX"
      case 11 => "BX"
      case 12 => "SP"
      case 13 => "BP"
      case 14 => "SI"
      case 15 => "DI"
  end disReg

  // ==============================================
  protected def getEffectiveAddress: Int = effectiveAddress
  protected def isSPRegMode: Boolean = mrmMode == 3 && mrmRM == 4
  protected def getMRMReg: Int =
    mrmReg
  protected def getSeg: Int =
    regs.getSeg(mrmReg)
  protected def setSeg(value:Int): Unit =
    regs.setSeg(mrmReg,value)
  protected def getReg8: Int =
    regs.get8(mrmReg)
  protected def setReg8(value:Int): Unit =
    regs.set8(mrmReg,value)
  protected def getReg16: Int =
    regs.get16(mrmReg)
  protected def setReg16(value:Int): Unit =
    regs.set16(mrmReg,value)

  protected def getMem8: Int =
    if mrmMode == 3 then regs.get8(mrmRM)
    else mem.readByte(effectiveAddress)
  protected def setMem8(value:Int): Unit =
    if mrmMode == 3 then regs.set8(mrmRM,value)
    else mem.writeByte(effectiveAddress,value)
  protected def getMem16: Int =
    if mrmMode == 3 then regs.get16(mrmRM)
    else mem.readWord(effectiveAddress)
  protected def setMem16(value:Int): Unit =
    if mrmMode == 3 then regs.set16(mrmRM,value)
    else mem.writeWord(effectiveAddress,value)

  private def loadMRMByte(): Unit =
    val mrm = fetchNextByte()
    mrmMode = (mrm >> 6) & 3
    mrmReg = (mrm >> 3) & 7
    mrmRM = mrm & 7
    calculateEffectiveAddress()

  protected def fetchNextByte(): Byte =
    val b = mem.readByte(regs.cs(regs.ip,ignoreSegmentOverride = true)).asInstanceOf[Byte]
    regs.incIP(1)
    b
  protected def fetchNextWord(): Short =
    val address = regs.cs(regs.ip,ignoreSegmentOverride = true)
    val b = mem.readWord(address).asInstanceOf[Short]
    regs.incIP(2)
    wordAccessOnOddAddressPenalty = (address & 1) == 1
    b

  private def calculateEffectiveAddress(): Unit =
    effectiveAddress = mrmMode match
      case 0 => // No Displacement
        mrmRM match
          case 0 => // (BX) + (SI)
            regs.ds(regs.bx + regs.si)
          case 1 => // (BX) + (DI)
            regs.ds(regs.bx + regs.di)
          case 2 => // (BP) + (SI)
            regs.ss(regs.bp + regs.si)
          case 3 => // (BP) + (DI)
            regs.ss(regs.bp + regs.di)
          case 4 => // (SI)
            regs.ds(regs.si)
          case 5 => // (DI)
            regs.ds(regs.di)
          case 6 => // 16 bit displacement
            regs.ds(fetchNextWord())
          case 7 => // (BX)
            regs.ds(regs.bx)
      case 1 => // 8-bit displacement
        val disp8 = fetchNextByte()
        mrmRM match
          case 0 => // (BX) + (SI) + disp8
            regs.ds(regs.bx + regs.si + disp8)
          case 1 => // (BX) + (DI) + disp8
            regs.ds(regs.bx + regs.di + disp8)
          case 2 => // (BP) + (SI) + disp8
            regs.ss(regs.bp + regs.si + disp8)
          case 3 => // (BP) + (DI) + disp8
            regs.ss(regs.bp + regs.di + disp8)
          case 4 => // (SI) + disp8
            regs.ds(regs.si + disp8)
          case 5 => // (DI) + disp8
            regs.ds(regs.di + disp8)
          case 6 => // (BP) + disp8
            regs.ss(regs.bp + disp8)
          case 7 => // (BX) + disp8
            regs.ds(regs.bx + disp8)
      case 2 => // 16-bit displacement
        val disp16 = fetchNextWord()
        mrmRM match
          case 0 => // (BX) + (SI) + disp16
            regs.ds(regs.bx + regs.si + disp16)
          case 1 => // (BX) + (DI) + disp16
            regs.ds(regs.bx + regs.di + disp16)
          case 2 => // (BP) + (SI) + disp16
            regs.ss(regs.bp + regs.si + disp16)
          case 3 => // (BP) + (DI) + disp16
            regs.ss(regs.bp + regs.di + disp16)
          case 4 => // (SI) + disp16
            regs.ds(regs.si + disp16)
          case 5 => // (DI) + disp16
            regs.ds(regs.di + disp16)
          case 6 => // (BP) + disp16
            regs.ss(regs.bp + disp16)
          case 7 => // (BX) + disp16
            regs.ds(regs.bx + disp16)
      case 3 => // Register operand
        mrmRM
  end calculateEffectiveAddress

  private def getDefaultCycles: Int =
    (if wordAccessOnOddAddressPenalty then 4 else 0) + (if regs.isSegmentOverride then 2 else 0)

  protected def getEACycles: Int =
    mrmMode match
      case 0 => // No Displacement
        mrmRM match
          case 0 => // (BX) + (SI)
            7
          case 1 => // (BX) + (DI)
            8
          case 2 => // (BP) + (SI)
            8
          case 3 => // (BP) + (DI)
            7
          case 4 => // (SI)
            5
          case 5 => // (DI)
            5
          case 6 => // 16 bit displacement
            6
          case 7 => // (BX)
            5
      case 1|2 =>
        mrmRM match
          case 0 => // (BX) + (SI) + disp8/16
            11
          case 1 => // (BX) + (DI) + disp8/16
            12
          case 2 => // (BP) + (SI) + disp8/16
            12
          case 3 => // (BP) + (DI) + disp8/16
            11
          case 4 => // (SI) + disp8/16
            9
          case 5 => // (DI) + disp8/16
            9
          case 6 => // (BP) + disp8/16
            9
          case 7 => // (BX) + disp8/16
            9
      case 3 =>
          0
  end getEACycles

  // ========================================================
  protected def call(ip:Int,cs:Int = -1,addIP:Boolean = true): Unit =
    if cs != -1 then
      regs.push(regs.cs,mem)
      regs.push(regs.ip,mem)
      regs.cs = cs
      regs.ip = ip
    else
      regs.push(regs.ip,mem)
      if addIP then
        regs.ip += ip
      else regs.ip = ip
  inline private def setSZP(result:Int,size8:Boolean): Unit =
    import Registers.*
    var flags = 0
    val mask = if size8 then 0xFF else 0xFFFF
    val msb = if size8 then 0x80 else 0x8000
    if (result & mask) == 0 then flags |= F_ZERO
    if (result & msb) != 0 then flags |= F_SIGN
    if PARITY(result & 0xFF) then flags |= F_PARITY
    regs.setFlags(F_ZERO | F_SIGN | F_PARITY,flags)

  final protected def add8(a:Int,b:Int,carry:Boolean = false): Byte =
    import Registers.*
    val sum = a + b + (if carry && regs.isFlags(F_CARRY) then 1 else 0)
    val sum8 = sum.asInstanceOf[Byte]
    regs.setFlag(F_OVERFLOW,((sum ^ a) & (sum ^ b) & 0x80) != 0)
    regs.setFlag(F_CARRY,(sum & 0xFF00) != 0)
    regs.setFlag(F_AUX_CARRY,((a ^ b ^ sum) & 0x10) != 0)
    setSZP(sum8,size8 = true)
    sum8
  final protected def add16(a:Int,b:Int,carry:Boolean = false): Short =
    import Registers.*
    val sum = a + b + (if carry && regs.isFlags(F_CARRY) then 1 else 0)
    regs.setFlag(F_OVERFLOW, ((sum ^ a) & (sum ^ b) & 0x8000) != 0)
    regs.setFlag(F_CARRY, (sum & 0xFF0000) != 0)
    regs.setFlag(F_AUX_CARRY, ((a ^ b ^ sum) & 0x10) != 0)
    setSZP(sum, size8 = false)
    sum.asInstanceOf[Short]
  final protected def or(a:Int,b:Int,size:Int): Int =
    import Registers.*
    val ored = a | b
    regs.clearFlags(F_OVERFLOW|F_CARRY)
    setSZP(ored,size == 8)
    ored
  final protected def and(a: Int, b: Int, size: Int): Int =
    import Registers.*
    val anded = a & b
    regs.clearFlags(F_OVERFLOW | F_CARRY)
    setSZP(anded, size == 8)
    anded
  final protected def xor(a: Int, b: Int, size: Int): Int =
    import Registers.*
    val xored = a ^ b
    regs.clearFlags(F_OVERFLOW | F_CARRY)
    setSZP(xored, size == 8)
    xored
  final protected def sub8(a: Int, b: Int, borrow: Boolean = false): Byte =
    import Registers.*
    val sub = a - b - (if borrow && regs.isFlags(F_CARRY) then 1 else 0)
    val sub8 = sub.asInstanceOf[Byte]
    regs.setFlag(F_OVERFLOW, ((sub ^ a) & (a ^ b) & 0x80) != 0)
    regs.setFlag(F_CARRY, (sub & 0xFF00) != 0)
    regs.setFlag(F_AUX_CARRY, ((a ^ b ^ sub) & 0x10) != 0)
    setSZP(sub8, size8 = true)
    sub8
  final protected def sub16(a: Int, b: Int, borrow: Boolean = false): Short =
    import Registers.*
    val sub = a - b - (if borrow && regs.isFlags(F_CARRY) then 1 else 0)
    val sub16 = sub.asInstanceOf[Short]
    regs.setFlag(F_OVERFLOW, ((sub ^ a) & (a ^ b) & 0x8000) != 0)
    regs.setFlag(F_CARRY, (sub & 0xFF0000) != 0)
    regs.setFlag(F_AUX_CARRY, ((a ^ b ^ sub) & 0x10) != 0)
    setSZP(sub16, size8 = false)
    sub16
  final protected def cmp8(a: Int, b: Int): Unit =
    import Registers.*
    val cmp = a - b
    regs.setFlag(F_OVERFLOW, ((cmp ^ a) & (a ^ b) & 0x80) != 0)
    regs.setFlag(F_CARRY, (cmp & 0xFF00) != 0)
    regs.setFlag(F_AUX_CARRY, ((a ^ b ^ cmp) & 0x10) != 0)
    setSZP(cmp, size8 = true)
  final protected def cmp16(a: Int, b: Int): Unit =
    import Registers.*
    val cmp = a - b
    regs.setFlag(F_OVERFLOW, ((cmp ^ a) & (a ^ b) & 0x8000) != 0)
    regs.setFlag(F_CARRY, (cmp & 0xFF0000) != 0)
    regs.setFlag(F_AUX_CARRY, ((a ^ b ^ cmp) & 0x10) != 0)
    setSZP(cmp, size8 = false)
  // all bcd related operations: see https://github.com/dbalsom/martypc/blob/main/crates/marty_core/src/cpu_808x/bcd.rs
  final protected def daa(): Unit =
    import Registers.*
    val old_cf = regs.isFlags(F_CARRY)
    val old_af = regs.isFlags(F_AUX_CARRY)
    val old_al = regs.al

    regs.clearFlags(F_CARRY)
    val al_check = if old_af then 0x9F else 0x99

    regs.clearFlags(F_OVERFLOW)
    if old_cf then
      if old_al >= 0x1A && old_al <= 0x7F then
        regs.setFlags(F_OVERFLOW)
    else if old_al >= 0x7A && old_al <= 0x7F then
      regs.setFlags(F_OVERFLOW)

    if (old_al & 0x0F) > 0x09 || regs.isFlags(F_AUX_CARRY) then
      regs.al += 0x06
      regs.setFlags(F_AUX_CARRY)
    else
      regs.clearFlags(F_AUX_CARRY)

    if old_al > al_check || old_cf then
      regs.al += 0x60
      regs.setFlags(F_CARRY)
    else
      regs.clearFlags(F_CARRY)

    setSZP(regs.al, size8 = true)

  final protected def das(): Unit =
    import Registers.*
    val old_cf = regs.isFlags(F_CARRY)
    val old_af = regs.isFlags(F_AUX_CARRY)
    val old_al = regs.al

    regs.clearFlags(F_CARRY)
    val al_check = if old_af then 0x9F else 0x99

    regs.clearFlags(F_OVERFLOW)
    if !old_af && !old_cf then
      if old_al >= 0x9A && old_al <= 0xDF then regs.setFlags(F_OVERFLOW)
    else if old_af && !old_cf then
      if (old_al >= 0x80 && old_al <= 0x85) || (old_al >= 0xA0 && old_al <= 0xE5) then regs.setFlags(F_OVERFLOW)
    else if !old_af && old_cf then
      if old_al >= 0x80 && old_al <= 0xDF then regs.setFlags(F_OVERFLOW)
    else if old_af && old_cf then
      if old_al >= 0x80 && old_al <= 0xE5 then regs.setFlags(F_OVERFLOW)

    if (old_al & 0x0F) > 0x09 || regs.isFlags(F_AUX_CARRY) then
      regs.al -= 0x06
      regs.setFlags(F_AUX_CARRY)
    else
      regs.clearFlags(F_AUX_CARRY)

    if old_al > al_check || old_cf then
      regs.al -= 0x60
      regs.setFlags(F_CARRY)
    else
      regs.clearFlags(F_CARRY)

    setSZP(regs.al, size8 = true)

  final protected def aaa(): Unit =
    import Registers.*
    val old_al = regs.al
    var new_al = 0

    if (old_al & 0x0F) > 9 || regs.isFlags(F_AUX_CARRY) then
      regs.ah += 1
      new_al = (old_al + 0x06) & 0xFF
      regs.al = new_al & 0x0F
      regs.setFlags(F_AUX_CARRY | F_CARRY)
    else
      new_al = old_al
      regs.al = new_al & 0x0F
      regs.clearFlags(F_AUX_CARRY | F_CARRY)

    regs.setFlag(F_ZERO,new_al == 0)
    regs.setFlag(F_PARITY,PARITY(new_al))
    regs.setFlag(F_OVERFLOW,old_al >= 0x7A && old_al <= 0x7F)

    regs.setFlag(F_SIGN,old_al >= 0x7A && old_al <= 0xF9)

  final protected def aas(): Unit =
    import Registers.*
    val old_al = regs.al
    val old_af = regs.isFlags(F_AUX_CARRY)
    var new_al = 0

    if (old_al & 0x0F) > 9 || old_af then
      regs.ah -= 1
      new_al = (old_al - 0x06) & 0xFF
      regs.al = new_al & 0x0F
      regs.setFlags(F_AUX_CARRY | F_CARRY)
    else
      new_al = old_al
      regs.al = new_al & 0x0F
      regs.clearFlags(F_AUX_CARRY | F_CARRY)

    regs.clearFlags(F_OVERFLOW | F_SIGN)
    regs.setFlag(F_ZERO, new_al == 0)
    regs.setFlag(F_PARITY, PARITY(new_al))
    regs.setFlag(F_OVERFLOW, old_af && old_al >= 0x80 && old_al <= 0x85)
    regs.setFlag(F_SIGN,(!old_af && old_al >= 0x80) || (old_af && ((old_al <= 0x05) || (old_al >= 0x86))))

  protected def test(a:Int,b:Int,size:Int): Unit =
    import Registers.*
    regs.clearFlags(F_CARRY | F_OVERFLOW)
    setSZP(a & b,size8 = size == 8)

  protected def rol(value:Int,bits:Int,size:Int): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = false
    var overflow = false
    while count > 0 do
      carry = (shifted & overflowMask) != 0
      shifted <<= 1
      if carry then shifted |= 1
      overflow = ((shifted ^ value) & overflowMask) != 0
      count -= 1
    regs.setFlag(F_OVERFLOW, overflow)
    regs.setFlag(F_CARRY,carry)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW
    shifted & mask
  protected def ror(value: Int, bits: Int, size: Int): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = false
    var overflow = false
    while count > 0 do
      carry = (shifted & 1) != 0
      shifted >>>= 1
      if carry then shifted |= overflowMask
      overflow = ((shifted ^ value) & overflowMask) != 0
      count -= 1
    regs.setFlag(F_OVERFLOW, overflow)
    regs.setFlag(F_CARRY, carry)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW
    shifted & mask
  protected def rcl(value: Int, bits: Int, size: Int): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = regs.isFlags(F_CARRY)
    var overflow = false
    while count > 0 do
      val lastCarry = carry
      carry = (shifted & overflowMask) != 0
      shifted <<= 1
      if lastCarry then shifted |= 1
      overflow = ((shifted ^ value) & overflowMask) != 0
      count -= 1
    regs.setFlag(F_OVERFLOW, overflow)
    regs.setFlag(F_CARRY, carry)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW

    shifted & mask
  protected def rcr(value: Int, bits: Int, size: Int): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = regs.isFlags(F_CARRY)
    var overflow = false
    while count > 0 do
      val lastCarry = carry
      carry = (shifted & 1) != 0
      shifted >>>= 1
      overflow = lastCarry ^ ((shifted & (1 << (size - 2))) != 0)
      shifted |= (if lastCarry then overflowMask else 0)
      count -= 1
    regs.setFlag(F_OVERFLOW, overflow)
    regs.setFlag(F_CARRY, carry)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW

    shifted & mask
  protected def shl(value: Int, bits: Int, size: Int): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = false
    var overflow = false
    while count > 0 do
      carry = (shifted & overflowMask) != 0
      shifted <<= 1
      overflow = ((shifted ^ value) & overflowMask) != 0
      count -= 1
    regs.setFlag(F_CARRY, carry)
    regs.setFlag(F_OVERFLOW, overflow)
    setSZP(shifted,size == 8)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW | F_AUX_CARRY

    shifted & mask
  protected def shr(value: Int, bits: Int, size: Int,arithmeticShift:Boolean = false): Int =
    import Registers.*
    if bits == 0 then return value

    val mask = (1 << size) - 1
    val overflowMask = 1 << (size - 1)

    var shifted = value
    var count = bits
    var carry = false
    var overflow = false
    while count > 0 do
      carry = (shifted & 1) != 0
      shifted >>>= 1
      if arithmeticShift then
        if (value & overflowMask) != 0 then
          shifted |= overflowMask
      overflow = ((shifted ^ value) & overflowMask) != 0
      count -= 1
    regs.setFlag(F_CARRY, carry)
    regs.setFlag(F_OVERFLOW, overflow)
    setSZP(shifted, size == 8)
    if bits > 1 then regs.undefinedFlags = F_OVERFLOW | F_AUX_CARRY

    shifted & mask
  protected def setmo(value:Int,bits:Int,size:Int): Int =
    import Registers.*
    if bits == 0 then return value

    val overflowMask = 1 << (size - 1)
    val result = if size == 8 then 0xFF else 0xFFFF
    setSZP(result, size == 8)
    regs.undefinedFlags = F_OVERFLOW | F_CARRY | F_AUX_CARRY
    result
  protected def aam(div:Int): Unit =
    import Registers.*
    if div != 0 then
      val s = regs.al
      val d = (((s / div) & 0xFF) << 8) | ((s % div) & 0xFF)
      regs.ax = d
      setSZP(d,size8 = true)
    else
      regs.undefinedFlags = F_SIGN | F_PARITY | F_OVERFLOW | F_CARRY | F_AUX_CARRY
  protected def aad(mul:Int): Unit =
    import Registers.*

    regs.al += regs.ah * mul
    regs.ah = 0
    setSZP(regs.al,size8 = true)
    regs.undefinedFlags = F_OVERFLOW | F_CARRY | F_AUX_CARRY
  protected def salc(): Unit =
    import Registers.*
    regs.al = if regs.isFlags(F_CARRY) then 0xFF else 0
