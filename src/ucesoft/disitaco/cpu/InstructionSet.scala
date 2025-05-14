package ucesoft.disitaco.cpu

import ucesoft.disitaco.cpu.Instruction.Disassembled

/**
 * @author Alessandro Abbruzzetti
 *         Created on 25/02/2025 15:56  
 */
object InstructionSet:
  import InstructionCode.*
  import Registers.*

  private class SegPrefix(override val opcode:Int,override val instr:InstructionCode) extends Instruction(opcode,instr):
    override final val segmentOverrideIndex: Int = (opcode >> 3) & 3
    override protected final def process(): Int =
      regs.setSegmentOverride(segmentOverrideIndex)
      0

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = Disassembled(address,List(opcode),instr)
  end SegPrefix

  private class IncDecReg(override val opcode:Int,override val instr:InstructionCode) extends Instruction(opcode,instr):
    private final val regIndex = opcode & 0x07
    private final val inc = instr == InstructionCode.INC

    override protected final def process(): Int =
      reset(loadMRM = false)
      val old_carry = regs.isFlags(F_CARRY)
      if inc then
        regs.set16(regIndex,add16(regs.get16(regIndex),1))
      else
        regs.set16(regIndex,sub16(regs.get16(regIndex),1))
      regs.setFlag(F_CARRY,old_carry)
      // cycles
      3 // reg 3, mem 15 + ea

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => disReg(regIndex,size = 16))
  end IncDecReg

  private class PushPopReg(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private final val regIndex = opcode & 0x07
    private final val push = instr == InstructionCode.PUSH

    override protected final def process(): Int =
      reset(loadMRM = false)
      if push then
        if regIndex == 4 then // special case SP
          val sp = (regs.sp - 2) & 0xFFFF
          regs.push(sp,mem)
        else
          regs.push(regs.get16(regIndex),mem)
      else
        regs.set16(regIndex,regs.pop(mem))
      // cycles
      11

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address, segmentOverridePrefix, prefixList, _ => disReg(regIndex, size = 16))
  end PushPopReg

  private class JumpIf(override val opcode: Int, override val instr: InstructionCode,flagCond:Registers => Boolean) extends Instruction(opcode, instr):
    override protected final def process(): Int =
      reset(loadMRM = false)
      val branch = flagCond(regs)
      val offset = fetchNextByte()
      if branch then
        regs.ip += offset
        16 // branch taken
      else
        4 // branch not taken


    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8,add = address + 2)
  end JumpIf
  // XCHG reg16,AX
  private class XCHGRegs(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private val reg = opcode & 7

    override protected final def process(): Int =
      reset(loadMRM = false)
      val reg16 = regs.get16(reg)
      val ax = regs.ax
      regs.ax = reg16
      regs.set16(reg,ax)
      // cycles
      3 // ax -> reg16

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
      val dis = disNoOp(address,segmentOverridePrefix,prefixList)
      if opcode == 0x90 then
        dis.copy(mnemonic = NOP)
      else
        dis.copy(op1 = Some(disReg(reg, size = 16)), op2 = Some("AX"))
  end XCHGRegs

  private class StringInstr(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private var repeated = false
    private val checkCXOnly = instr == MOVSB || instr == MOVSW || instr == STOSB || instr == STOSW || instr == LODSB || instr == LODSW
    private val increment = if instr == MOVSB || instr == STOSB || instr == LODSB || instr == CMPSB || instr == SCASB then 1 else 2
    private val incSI = instr != STOSB && instr != STOSW && instr != SCASB && instr != SCASW
    private val incDI = instr != LODSB && instr != LODSW

    override def hasBeenRepeated: Boolean = repeated

    override protected final def process(): Int =
      reset(loadMRM = false)
      repeated = false
      val rep = regs.isRep
      // ===== REP CHECK ========================================
      if rep then
        if regs.cx == 0 then
          return 2 // 2 cycles for REP

        regs.cx -= 1
        repeated = true
      // ========================================================
      val source = regs.ds(regs.si)
      val dest = regs.es(regs.di, ignoreSegmentOverride = true)
      instr match
        case MOVSB =>
          mem.writeByte(dest, mem.readByte(source))
        case MOVSW =>
          mem.writeWord(dest, mem.readWord(source))
        case CMPSB =>
          cmp8(mem.readByte(source),mem.readByte(dest))
        case CMPSW =>
          cmp16(mem.readWord(source),mem.readWord(dest))
        case STOSB =>
          mem.writeByte(dest,regs.al)
        case STOSW =>
          mem.writeWord(dest,regs.ax)
        case LODSB =>
          regs.al = mem.readByte(source)
        case LODSW =>
          regs.ax = mem.readWord(source)
        case SCASB =>
          cmp8(regs.al,mem.readByte(dest))
        case SCASW =>
          cmp16(regs.ax,mem.readWord(dest))
        case _ =>
      val inc = if regs.isFlags(F_DIRECTION) then -increment else increment
      if incSI then
        regs.si += inc
      if incDI then
        regs.di += inc
      // check after execution ==================================
      if rep then
        if checkCXOnly then
          regs.ip = regs.getRepIP // back to REP
        else
          regs.getRep match
            case Registers.REPN =>
              if regs.cx == 0 || regs.isFlags(F_ZERO) then
                repeated = false
              else
                regs.ip = regs.getRepIP // back to REP
            case Registers.REP =>
              if regs.cx == 0 || !regs.isFlags(F_ZERO) then
                repeated = false
              else
                regs.ip = regs.getRepIP // back to REP
      // ========================================================
      // cycles
      instr match
        case MOVSB|MOVSW =>
          18 + (if rep then 2 else 0)
        case CMPSB|CMPSW =>
          22 + (if rep then 2 else 0)
        case STOSB|STOSW =>
          11 + (if rep then 2 else 0)
        case LODSB|LODSW =>
          12 + (if rep then 2 else 0)
        case SCASB|SCASW =>
          15 + (if rep then 2 else 0)
        case _ =>
          0

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
      val rep = if checkCXOnly then InstructionCode.REP else if repPrefix == Registers.REPN then InstructionCode.REPNE else InstructionCode.REPE
      val dis = disNoOp(address, segmentOverridePrefix, prefixList)
      if repPrefix != -1 then
        val prefix = if segmentOverridePrefix != -1 then Some(s"${disSeg(segmentOverridePrefix)}:") else None
        dis.copy(rep = Some(rep),repSeg = prefix)
      else
        dis
  end StringInstr

  // MOVE reg,imm
  private class MoveRegImm(override val opcode: Int, override val instr: InstructionCode,size:Int) extends Instruction(opcode, instr):
    private val reg = opcode & 7
    private val size8 = size == 8

    override protected final def process(): Int =
      reset(loadMRM = false)
      if size8 then regs.set8(reg,fetchNextByte()) else regs.set16(reg,fetchNextWord())
      // cycles
      4 // imm -> reg

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
      disImm(address,segmentOverridePrefix,prefixList,disReg(reg, size = size),size)
  end MoveRegImm

  // D0-D3 GRP2 SHIFT operations: ROL, ROR, RCL, RCR, SHL, SHR, SETMO, SAR
  private class ShiftOps(override val opcode: Int,size:Int,countCL:Boolean = false) extends Instruction(opcode,TBD) {
    private final val MNEMS = Array(ROL, ROR, RCL, RCR, SHL, SHR, SETMO, SAR)
    private val size8 = size == 8

    override protected def process(): Int =
      reset(loadMRM = true)
      val count = if countCL then regs.cl else 1
      val source = if size8 then getMem8 else getMem16
      val value = getMRMReg match
        case 0 => // ROL [X],n
          rol(source,count,size = size)
        case 1 => // ROR [X],n
          ror(source,count,size = size)
        case 2 => // RCL [X],n
          rcl(source,count,size = size)
        case 3 => // RCR [X],n
          rcr(source,count,size = size)
        case 4 => // SHL [X],n
          shl(source,count,size = size)
        case 5 => // SHR [X],n
          shr(source,count,size = size)
        case 6 => // SETMO [X], n (undocumented)
          setmo(source,count,size = size)
        case 7 => // SAR [X],n
          shr(source,count,size = size,arithmeticShift = true)
        case _ =>
          0
      if size8 then setMem8(value) else setMem16(value)
      // cycles
      if isRegMode then
        if countCL then 8 + 4 * count else 2
      else if countCL then 20 + getEACycles + 4 * count else 15 + getEACycles

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
      val dis = dis1Op(address, segmentOverridePrefix, prefixList,if size8 then disEA8 else disEA16,mrm = true)
      dis.copy(mnemonic = MNEMS(disMrmReg(dis,opcode)),op2 = if countCL then Some("CL") else None)
  }

  // ESC [X]
  private class ESCInstr(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private val xxx = opcode & 7

    override protected final def process(): Int =
      reset(loadMRM = true)
      val address = getEffectiveAddress
      val yyy = getMRMReg
      val word = getMem16
      // call ESC handler if present
      if escHandler != null then
        escHandler.esc(xxx << 3 | yyy,word)
      // cycles
      2

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address, segmentOverridePrefix, prefixList, disEA16,mrm = true)
  end ESCInstr

  // LOOPZ/LOOPNZ [X]
  private class LOOPInstr(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private val loopnz = instr == LOOPNZ
    private val loopz = instr == LOOPZ

    override protected final def process(): Int =
      reset(loadMRM = false)
      val label = fetchNextByte()
      regs.cx -= 1
      var jump = false
      if regs.cx != 0 then
        if loopz then
          if regs.isFlags(F_ZERO) then jump = true
        else if loopnz then
          if !regs.isFlags(F_ZERO) then jump = true
        else
          jump = true
      if jump then
        regs.ip += label
        if loopnz then 19 else 18 // cycles for jump taken
      else
        5 // cycles for jump not taken

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address, segmentOverridePrefix, prefixList,size = 8,address + 2)
  end LOOPInstr

  // F6-F7 GRP3a/b TEST, TEST, NOT, NEG, MUL, IMUL, DIV, IDIV
  private class GPR3Ops(override val opcode: Int, size: Int) extends Instruction(opcode, TBD) {
    override val canGenerateSoftwareInterrupt: Boolean = true

    private val size8 = size == 8
    private var interrupt = -1
    private val MNEM = Array(TEST, TEST, NOT, NEG, MUL, IMUL, DIV, IDIV)

    override def softwareInterrupt: Int = interrupt

    override protected def process(): Int =
      reset(loadMRM = true)
      interrupt = -1

      getMRMReg match
        case 0|1 => // TEST [X], imm 1 is undocumented
          if size8 then
            test(getMem8,fetchNextByte(),size = size)
          else
            test(getMem16,fetchNextWord(),size = size)
          // cycles
          if isRegMode then 5 else 11 + getEACycles
        case 2 => // NOT [X]
          if size8 then
            setMem8(~getMem8)
          else
            setMem16(~getMem16)
          // cycles
          if isRegMode then 3 else 16 + getEACycles
        case 3 => // NEG [X]
          if size8 then
            setMem8(sub8(0,getMem8))
          else
            setMem16(sub16(0,getMem16))
          // cycles
          if isRegMode then 3 else 16 + getEACycles
        case 4 => // MUL [X]
          if size8 then
            val mul = regs.al * getMem8
            regs.ax = mul
            if regs.ah != 0 then
              regs.setFlags(F_OVERFLOW | F_CARRY)
            else
              regs.clearFlags(F_OVERFLOW | F_CARRY)
            regs.undefinedFlags = F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
            // cycles
            if isRegMode then 73 else 79 + getEACycles
          else
            val mul = regs.ax * getMem16
            regs.ax = mul & 0xFFFF
            regs.dx = mul >> 16
            if regs.dx != 0 then
              regs.setFlags(F_OVERFLOW | F_CARRY)
            else
              regs.clearFlags(F_OVERFLOW | F_CARRY)
            regs.undefinedFlags = F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
            // cycles
            if isRegMode then 115 else 131 + getEACycles
        case 5 => // IMUL [X]
          if size8 then
            var mul = regs.al.toByte * getMem8.toByte
            if regs.isRep then // undocumented
              mul = -mul
            regs.ax = mul
            mul &= 0xFF80

            if mul != 0xFF80 && mul != 0 then
              regs.setFlags(F_OVERFLOW | F_CARRY)
            else
              regs.clearFlags(F_OVERFLOW | F_CARRY)
            regs.undefinedFlags = F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
            // cycles
            if isRegMode then 89 else 95 + getEACycles
          else
            var mul = regs.ax.toShort * getMem16.toShort
            if regs.isRep then // undocumented
              mul = -mul
            regs.ax = mul & 0xFFFF
            regs.dx = mul >> 16
            mul &= 0xFFFF8000

            if mul != 0xFFFF8000 && mul != 0 then
              regs.setFlags(F_OVERFLOW | F_CARRY)
            else
              regs.clearFlags(F_OVERFLOW | F_CARRY)
            regs.undefinedFlags = F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
            // cycles
            if isRegMode then 141 else 147 + getEACycles
        case 6 => // DIV [X]
          regs.undefinedFlags = F_OVERFLOW | F_CARRY | F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
          if size8 then
            val div = getMem8
            if div == 0 then // division by zero
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles
            val d = regs.ax / div
            if (d & 0xFF00) != 0 then // division overflow
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles

            regs.ax = (regs.ax % div) << 8 | d
            // cycles
            if isRegMode then 85 else 91 + getEACycles
          else
            val div = getMem16
            if div == 0 then // division by zero
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles
            val s = regs.dx.toLong << 16 | regs.ax
            val d = s / div
            if (d & 0xFFFF0000) != 0 then // division overflow
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles

            regs.ax = d.toInt
            regs.dx = (s % div).toInt
            // cycles
            if isRegMode then 153 else 159 + getEACycles
        case 7 => // IDIV [X]
          regs.undefinedFlags = F_OVERFLOW | F_CARRY | F_ZERO | F_SIGN | F_PARITY | F_AUX_CARRY
          if size8 then
            val div = getMem8.toByte
            if div == 0 then // division by zero
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles
            var d = regs.ax.toShort / div
            if regs.isRep then // undocumented
              d = -d
            if d > 127 || d < -127 then // division overflow
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles

            regs.ax = (regs.ax.toShort % div) << 8 | d & 0xFF
            // cycles
            if isRegMode then 106 else 112 + getEACycles
          else
            val div = getMem16.toShort
            if div == 0 then // division by zero
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles
            val s = regs.dx.toShort.toLong << 16 | regs.ax
            var d = s / div
            if regs.isRep then // undocumented
              d = -d
            if d > 32767 || d < -32767 then // division overflow
              interrupt = 0
              // how many cycles to consume ??
              return getEACycles

            regs.ax = d.toInt
            regs.dx = (s % div).toInt
            // cycles
            if isRegMode then 174 else 180 + getEACycles
        case _ =>
          0
    end process

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
      val dis = dis1Op(address,segmentOverridePrefix,prefixList,_ => "",mrm = true)
      disMrmReg(dis,opcode) match
        case 0|1 =>
          if size8 then
            dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disImm8).copy(mnemonic = TEST)
          else
            dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disImm16).copy(mnemonic = TEST)
        case i =>
          if size8 then
            dis1Op(address,segmentOverridePrefix,prefixList,disEA8,mrm = true).copy(mnemonic = MNEM(i))
          else
            dis1Op(address,segmentOverridePrefix,prefixList,disEA16,mrm = true).copy(mnemonic = MNEM(i))
  }

  // CLC/STC/CLI/STI/CLD/STD
  private class ClearSetFlagInstr(override val opcode: Int, override val instr: InstructionCode) extends Instruction(opcode, instr):
    private val (flag,set) = instr match
      case CLC => (F_CARRY,false)
      case STC => (F_CARRY,true)
      case CLI => (F_INT,false)
      case STI => (F_INT, true)
      case CLD => (F_DIRECTION, false)
      case STD => (F_DIRECTION, true)
      case _ => (0,false)

    override protected final def process(): Int =
      reset(loadMRM = false)
      regs.setFlag(flag,set)
      // cycles
      2

    override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
  end ClearSetFlagInstr

  final val opcodes : Array[Instruction] = Array(
    // 0X =========================================================================================================================================================================================================
    // ADD [X], reg8
    new Instruction(0x00,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(add8(getMem8, getReg8))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // ADD [X], reg16
    new Instruction(0x01,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(add16(getMem16, getReg16))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // ADD reg8, [X]
    new Instruction(0x02,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(add8(getReg8, getMem8))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // ADD reg16, [X]
    new Instruction(0x03,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(add16(getReg16, getMem16))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // ADD AL, imm8
    new Instruction(0x04,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = add8(regs.al,fetchNextByte().toInt & 0xFF)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // ADD AX, imm16
    new Instruction(0x05,ADD) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = add16(regs.ax,fetchNextWord().toInt & 0xFFFF)

        // cycles
        4 // imm16 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // PUSH ES
    new Instruction(0x06,PUSH) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.push(regs.es,mem)

        // cycles
        11 // reg 11

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "ES")
    },
    // POP ES
    new Instruction(0x07,POP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.es = regs.pop(mem)

        // cycles
        8 // reg 8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "ES")
    },
    // OR [X], reg8
    new Instruction(0x08,OR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(or(getMem8, getReg8,size = 8))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address, segmentOverridePrefix, prefixList, disEA8, disReg8)
    },
    // OR [X], reg16
    new Instruction(0x09,OR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(or(getMem16, getReg16,size = 16))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address, segmentOverridePrefix, prefixList, disEA16, disReg16)
    },
    // OR reg8, [X]
    new Instruction(0x0A,OR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(or(getReg8, getMem8,size = 8))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // OR reg16, [X]
    new Instruction(0x0B,OR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(or(getReg16, getMem16,size = 16))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // OR AL, imm8
    new Instruction(0x0C,OR) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = or(regs.al,fetchNextByte().toInt & 0xFF,size = 8)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // OR AX, imm16
    new Instruction(0x0D,OR) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = or(regs.ax,fetchNextWord().toInt & 0xFFFF,size = 16)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // PUSH CS
    new Instruction(0x0E,PUSH) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.push(regs.cs,mem)

        // cycles
        11 // reg 11

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "CS")
    },
    // POP CS (undocumented)
    new Instruction(0x0F,POP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.cs = regs.pop(mem)

        // cycles
        8 // reg 8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "CS")
    },
    // 1X =========================================================================================================================================================================================================
    // ADC [X], reg8
    new Instruction(0x10,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(add8(getMem8, getReg8,carry = true))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // ADC [X], reg16
    new Instruction(0x11,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(add16(getMem16, getReg16,carry = true))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // ADC reg8, [X]
    new Instruction(0x12,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(add8(getReg8, getMem8,carry = true))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // ADC reg16, [X]
    new Instruction(0x13,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(add16(getReg16, getMem16,carry = true))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // ADC AL, imm8
    new Instruction(0x14,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = add8(regs.al,fetchNextByte().toInt & 0xFF,carry = true)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // ADC AX, imm16
    new Instruction(0x15,ADC) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = add16(regs.ax,fetchNextWord().toInt & 0xFFFF,carry = true)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // PUSH SS
    new Instruction(0x16,PUSH) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.push(regs.ss,mem)

        // cycles
        11 // reg 11

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "SS")
    },
    // POP SS
    new Instruction(0x17,POP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ss = regs.pop(mem)

        // cycles
        8 // reg 8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "SS")
    },
    // SBB [X], reg8
    new Instruction(0x18,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(sub8(getMem8, getReg8,borrow = true))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // SBB [X], reg16
    new Instruction(0x19,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(sub16(getMem16, getReg16,borrow = true))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // SBB reg8, [X]
    new Instruction(0x1A,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(sub8(getReg8, getMem8,borrow = true))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // SBB reg16, [X]
    new Instruction(0x1B,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(sub16(getReg16, getMem16,borrow = true))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // SBB AL, imm8
    new Instruction(0x1C,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = sub8(regs.al,fetchNextByte().toInt & 0xFF,borrow = true)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // SBB AX, imm16
    new Instruction(0x1D,SBB) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = sub16(regs.ax,fetchNextWord().toInt & 0xFFFF,borrow = true)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // PUSH DS
    new Instruction(0x1E,PUSH) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.push(regs.ds,mem)

        // cycles
        11 // reg 11

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "DS")
    },
    // POP DS
    new Instruction(0x1F,POP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ds = regs.pop(mem)

        // cycles
        8 // reg 8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,_ => "DS")
    },
    // 2X =========================================================================================================================================================================================================
    // AND [X], reg8
    new Instruction(0x20,AND) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(and(getMem8, getReg8,size = 8))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address, segmentOverridePrefix, prefixList, disEA8, disReg8)
    },
    // AND [X], reg16
    new Instruction(0x21,AND) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(and(getMem16, getReg16,size = 16))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address, segmentOverridePrefix, prefixList, disEA16, disReg16)
    },
    // AND reg8, [X]
    new Instruction(0x22,AND) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(and(getReg8, getMem8,size = 8))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // AND reg16, [X]
    new Instruction(0x23,AND) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(and(getReg16, getMem16,size = 16))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // AND AL, imm8
    new Instruction(0x24,AND) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = and(regs.al,fetchNextByte().toInt & 0xFF,size = 8)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // AND AX, imm16
    new Instruction(0x25,AND) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = and(regs.ax,fetchNextWord().toInt & 0xFFFF,size = 16)

        // cycles
        4 // imm16 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // ES:
    new SegPrefix(0x26,SEG_ES),
    // DAA
    new Instruction(0x27,DAA) {
      override protected final def process(): Int =
        daa()
        // cycles
        4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // SUB [X], reg8
    new Instruction(0x28,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(sub8(getMem8, getReg8))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // SUB [X], reg16
    new Instruction(0x29,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(sub16(getMem16, getReg16))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // SUB reg8, [X]
    new Instruction(0x2A,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(sub8(getReg8, getMem8))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // SUB reg16, [X]
    new Instruction(0x2B,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(sub16(getReg16, getMem16))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // SUB AL, imm8
    new Instruction(0x2C,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = sub8(regs.al,fetchNextByte().toInt & 0xFF)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // SUB AX, imm16
    new Instruction(0x2D,SUB) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = sub16(regs.ax,fetchNextWord().toInt & 0xFFFF)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // CS:
    new SegPrefix(0x2E,SEG_CS),
    // DAS
    new Instruction(0x2F,DAS) {
      override protected final def process(): Int =
        das()
        // cycles
        4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // 3X =========================================================================================================================================================================================================
    // XOR [X], reg8
    new Instruction(0x30,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(xor(getMem8, getReg8,size = 8))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // XOR [X], reg16
    new Instruction(0x31,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(xor(getMem16, getReg16,size = 16))

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 16 + getEACycles // reg -> mem 16+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // XOR reg8, [X]
    new Instruction(0x32,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(xor(getReg8, getMem8,size = 8))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // XOR reg16, [X]
    new Instruction(0x33,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(xor(getReg16, getMem16,size = 16))

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // XOR AL, imm8
    new Instruction(0x34,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = xor(regs.al,fetchNextByte().toInt & 0xFF,size = 8)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // XOR AX, imm16
    new Instruction(0x35,XOR) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = xor(regs.ax,fetchNextWord().toInt & 0xFFFF,size = 16)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // SS:
    new SegPrefix(0x36,SEG_SS),
    // AAA
    new Instruction(0x37,AAA) {
      override protected final def process(): Int =
        aaa()
        // cycles
        8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // CMP [X], reg8
    new Instruction(0x38,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        cmp8(getMem8, getReg8)

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 9 + getEACycles // reg -> mem 9+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // CMP [X], reg16
    new Instruction(0x39,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        cmp16(getMem16, getReg16)

        // cycles
        if isRegMode then 3 // reg -> reg 3
        else 9 + getEACycles // reg -> mem 9+EA

      override def disassemble(address:Int,segmentOverridePrefix: Int, repPrefix: Int,prefixList:List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // CMP reg8, [X]
    new Instruction(0x3A,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        cmp8(getReg8,getMem8)

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // CMP reg16, [X]
    new Instruction(0x3B,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        cmp16(getReg16,getMem16)

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // CMP AL, imm8
    new Instruction(0x3C,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        cmp8(regs.al,fetchNextByte().toInt & 0xFF)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // CMP AX, imm16
    new Instruction(0x3D,CMP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        cmp16(regs.ax,fetchNextWord().toInt & 0xFFFF)

        // cycles
        4 // imm8 -> acc 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // DS:
    new SegPrefix(0x3E,SEG_DS),
    // AAA
    new Instruction(0x3F,AAS) {
      override protected final def process(): Int =
        aas()
        // cycles
        8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // 4X =========================================================================================================================================================================================================
    // INC AX
    new IncDecReg(0x40,INC),
    // INC CX
    new IncDecReg(0x41,INC),
    // INC DX
    new IncDecReg(0x42,INC),
    // INC BX
    new IncDecReg(0x43,INC),
    // INC SP
    new IncDecReg(0x44,INC),
    // INC BP
    new IncDecReg(0x45,INC),
    // INC SI
    new IncDecReg(0x46,INC),
    // INC DI
    new IncDecReg(0x47,INC),
    // DEC AX
    new IncDecReg(0x48,DEC),
    // DEC CX
    new IncDecReg(0x49,DEC),
    // DEC DX
    new IncDecReg(0x4A,DEC),
    // DEC BX
    new IncDecReg(0x4B,DEC),
    // DEC SP
    new IncDecReg(0x4C,DEC),
    // DEC BP
    new IncDecReg(0x4D,DEC),
    // DEC SI
    new IncDecReg(0x4E,DEC),
    // DEC DI
    new IncDecReg(0x4F,DEC),
    // 5X =========================================================================================================================================================================================================
    // PUSH AX
    new PushPopReg(0x50,PUSH),
    // PUSH CX
    new PushPopReg(0x51,PUSH),
    // PUSH DX
    new PushPopReg(0x52,PUSH),
    // PUSH BX
    new PushPopReg(0x53,PUSH),
    // PUSH SP
    new PushPopReg(0x54,PUSH),
    // PUSH BP
    new PushPopReg(0x55,PUSH),
    // PUSH SI
    new PushPopReg(0x56,PUSH),
    // PUSH DI
    new PushPopReg(0x57,PUSH),
    // POP AX
    new PushPopReg(0x58,POP),
    // POP CX
    new PushPopReg(0x59,POP),
    // POP DX
    new PushPopReg(0x5A,POP),
    // POP BX
    new PushPopReg(0x5B,POP),
    // POP SP
    new PushPopReg(0x5C,POP),
    // POP BP
    new PushPopReg(0x5D,POP),
    // PUSH SI
    new PushPopReg(0x5E,POP),
    // PUSH DI
    new PushPopReg(0x5F,POP),
    // 6X (undocumented, same as 70-7F) ==========================================================================================================================================================================
    // JO
    new JumpIf(0x60,JO,_.isFlags(F_OVERFLOW)),
    // JNO
    new JumpIf(0x61,JNO,!_.isFlags(F_OVERFLOW)),
    // JC
    new JumpIf(0x62,JC,_.isFlags(F_CARRY)),
    // JNC
    new JumpIf(0x63,JNC,!_.isFlags(F_CARRY)),
    // JZ
    new JumpIf(0x64,JZ,_.isFlags(F_ZERO)),
    // JNZ
    new JumpIf(0x65,JNZ,!_.isFlags(F_ZERO)),
    // JNA
    new JumpIf(0x66,JNA,r => (r.getFlagWord & (F_CARRY | F_ZERO)) != 0),
    // JA
    new JumpIf(0x67,JA,r => (r.getFlagWord & (F_CARRY | F_ZERO)) == 0),
    // JS
    new JumpIf(0x68,JS,_.isFlags(F_SIGN)),
    // JNS
    new JumpIf(0x69,JNS,!_.isFlags(F_SIGN)),
    // JP
    new JumpIf(0x6A,JP,_.isFlags(F_PARITY)),
    // JNP
    new JumpIf(0x6B,JNP,!_.isFlags(F_PARITY)),
    // JL
    new JumpIf(0x6C,JL,r => r.isFlags(F_SIGN) != r.isFlags(F_OVERFLOW)),
    // JNL
    new JumpIf(0x6D,JNL,r => r.isFlags(F_SIGN) == r.isFlags(F_OVERFLOW)),
    // JLE
    new JumpIf(0x6E,JLE,r => r.isFlags(F_ZERO) || (r.isFlags(F_SIGN) != r.isFlags(F_OVERFLOW))),
    // JNLE
    new JumpIf(0x6F,JNLE,r => !r.isFlags(F_ZERO) && (r.isFlags(F_SIGN) == r.isFlags(F_OVERFLOW))),
    // 7X =========================================================================================================================================================================================================
    // JO
    new JumpIf(0x70,JO,_.isFlags(F_OVERFLOW)),
    // JNO
    new JumpIf(0x71,JNO,!_.isFlags(F_OVERFLOW)),
    // JC
    new JumpIf(0x72,JC,_.isFlags(F_CARRY)),
    // JNC
    new JumpIf(0x73,JNC,!_.isFlags(F_CARRY)),
    // JZ
    new JumpIf(0x74,JZ,_.isFlags(F_ZERO)),
    // JNZ
    new JumpIf(0x75,JNZ,!_.isFlags(F_ZERO)),
    // JNA
    new JumpIf(0x76,JNA,r => (r.getFlagWord & (F_CARRY | F_ZERO)) != 0),
    // JA
    new JumpIf(0x77,JA,r => (r.getFlagWord & (F_CARRY | F_ZERO)) == 0),
    // JS
    new JumpIf(0x78,JS,_.isFlags(F_SIGN)),
    // JNS
    new JumpIf(0x79,JNS,!_.isFlags(F_SIGN)),
    // JP
    new JumpIf(0x7A,JP,_.isFlags(F_PARITY)),
    // JNP
    new JumpIf(0x7B,JNP,!_.isFlags(F_PARITY)),
    // JL
    new JumpIf(0x7C,JL,r => r.isFlags(F_SIGN) != r.isFlags(F_OVERFLOW)),
    // JNL
    new JumpIf(0x7D,JNL,r => r.isFlags(F_SIGN) == r.isFlags(F_OVERFLOW)),
    // JLE
    new JumpIf(0x7E,JLE,r => r.isFlags(F_ZERO) || (r.isFlags(F_SIGN) != r.isFlags(F_OVERFLOW))),
    // JNLE
    new JumpIf(0x7F,JNLE,r => !r.isFlags(F_ZERO) && (r.isFlags(F_SIGN) == r.isFlags(F_OVERFLOW))),
    // 8X =========================================================================================================================================================================================================
    // 80-83 GRP1 IMMEDIATE: ADD, OR, ADC, SBB, AND, SUB, XOR, CMP
    new Instruction(0x80,TBD) {
      private final val MNEMS = Array(ADD, OR, ADC, SBB, AND, SUB, XOR, CMP)

      override protected def process(): Int =
        reset(loadMRM = true)
        getMRMReg match
          case 0 => // ADD [X], imm8
            setMem8(add8(getMem8, fetchNextByte().toInt & 0xFF))
            if isRegMode then 4 else 17 + getEACycles
          case 1 => // OR [X], imm8
            setMem8(or(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 2 => // ADC [X], imm8
            setMem8(add8(getMem8, fetchNextByte().toInt & 0xFF, carry = true))
            if isRegMode then 4 else 17 + getEACycles
          case 3 => // SBB [X], imm8
            setMem8(sub8(getMem8, fetchNextByte().toInt & 0xFF, borrow = true))
            if isRegMode then 4 else 17 + getEACycles
          case 4 => // AND [X], imm8
            setMem8(and(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 5 => // SUB [X], imm8
            setMem8(sub8(getMem8, fetchNextByte().toInt & 0xFF))
            if isRegMode then 4 else 17 + getEACycles
          case 6 => // XOR [X], imm8
            setMem8(xor(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 7 => // CMP [X], imm8
            cmp8(getMem8, fetchNextByte().toInt & 0xFF)
            if isRegMode then 4 else 10 + getEACycles
          case _ => 0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
        val dis = dis2Ops(address, segmentOverridePrefix, prefixList, disEA8, disImm8)
        dis.copy(mnemonic = MNEMS(disMrmReg(dis, opcode)))
    },
    new Instruction(0x81,TBD) {
      private final val MNEMS = Array(ADD, OR, ADC, SBB, AND, SUB, XOR, CMP)

      override protected def process(): Int =
        reset(loadMRM = true)
        getMRMReg match
          case 0 => // ADD [X], imm16
            setMem16(add16(getMem16, fetchNextWord().toInt & 0xFFFF))
            if isRegMode then 4 else 17 + getEACycles
          case 1 => // OR [X], imm16
            setMem16(or(getMem16, fetchNextWord().toInt & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 2 => // ADC [X], imm16
            setMem16(add16(getMem16, fetchNextWord().toInt & 0xFFFF, carry = true))
            if isRegMode then 4 else 17 + getEACycles
          case 3 => // SBB [X], imm16
            setMem16(sub16(getMem16, fetchNextWord().toInt & 0xFFFF, borrow = true))
            if isRegMode then 4 else 17 + getEACycles
          case 4 => // AND [X], imm16
            setMem16(and(getMem16, fetchNextWord().toInt & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 5 => // SUB [X], imm16
            setMem16(sub16(getMem16, fetchNextWord().toInt & 0xFFFF))
            if isRegMode then 4 else 17 + getEACycles
          case 6 => // XOR [X], imm16
            setMem16(xor(getMem16, fetchNextWord().toInt & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 7 => // CMP [X], imm16
            cmp16(getMem16, fetchNextWord().toInt & 0xFFFF)
            if isRegMode then 4 else 10 + getEACycles
          case _ => 0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
        val dis = dis2Ops(address, segmentOverridePrefix, prefixList, disEA16, disImm16)
        dis.copy(mnemonic = MNEMS(disMrmReg(dis, opcode)))
    },
    // Undocumented same as 0x80
    new Instruction(0x82,TBD) {
      private final val MNEMS = Array(ADD, OR, ADC, SBB, AND, SUB, XOR, CMP)

      override protected def process(): Int =
        reset(loadMRM = true)
        getMRMReg match
          case 0 => // ADD [X], imm8
            setMem8(add8(getMem8, fetchNextByte().toInt & 0xFF))
            if isRegMode then 4 else 17 + getEACycles
          case 1 => // OR [X], imm8
            setMem8(or(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 2 => // ADC [X], imm8
            setMem8(add8(getMem8, fetchNextByte().toInt & 0xFF, carry = true))
            if isRegMode then 4 else 17 + getEACycles
          case 3 => // SBB [X], imm8
            setMem8(sub8(getMem8, fetchNextByte().toInt & 0xFF, borrow = true))
            if isRegMode then 4 else 17 + getEACycles
          case 4 => // AND [X], imm8
            setMem8(and(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 5 => // SUB [X], imm8
            setMem8(sub8(getMem8, fetchNextByte().toInt & 0xFF))
            if isRegMode then 4 else 17 + getEACycles
          case 6 => // XOR [X], imm8
            setMem8(xor(getMem8, fetchNextByte().toInt & 0xFF, size = 8))
            if isRegMode then 4 else 17 + getEACycles
          case 7 => // CMP [X], imm8
            cmp8(getMem8, fetchNextByte().toInt & 0xFF)
            if isRegMode then 4 else 10 + getEACycles
          case _ => 0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
        val dis = dis2Ops(address, segmentOverridePrefix, prefixList, disEA8, disImm8)
        dis.copy(mnemonic = MNEMS(disMrmReg(dis, opcode)))
    },
    new Instruction(0x83,TBD) {
      private final val MNEMS = Array(ADD, OR, ADC, SBB, AND, SUB, XOR, CMP)

      override protected def process(): Int =
        reset(loadMRM = true)
        getMRMReg match
          case 0 => // ADD [X], sign-extended imm8
            setMem16(add16(getMem16, fetchNextByte() & 0xFFFF))
            if isRegMode then 4 else 17 + getEACycles
          case 1 => // OR [X], sign-extended imm8
            setMem16(or(getMem16, fetchNextByte() & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 2 => // ADC [X], sign-extended imm8
            setMem16(add16(getMem16, fetchNextByte() & 0xFFFF, carry = true))
            if isRegMode then 4 else 17 + getEACycles
          case 3 => // SBB [X], sign-extended imm8
            setMem16(sub16(getMem16, fetchNextByte() & 0xFFFF, borrow = true))
            if isRegMode then 4 else 17 + getEACycles
          case 4 => // AND [X], sign-extended imm8
            setMem16(and(getMem16, fetchNextByte() & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 5 => // SUB [X], sign-extended imm8
            setMem16(sub16(getMem16, fetchNextByte() & 0xFFFF))
            if isRegMode then 4 else 17 + getEACycles
          case 6 => // XOR [X], sign-extended imm8
            setMem16(xor(getMem16, fetchNextByte() & 0xFFFF, size = 16))
            if isRegMode then 4 else 17 + getEACycles
          case 7 => // CMP [X], sign-extended imm8
            cmp16(getMem16, fetchNextByte() & 0xFFFF)
            if isRegMode then 4 else 10 + getEACycles
          case _ => 0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Disassembled =
        val dis = dis2Ops(address, segmentOverridePrefix, prefixList, disEA16, disImm8)
        dis.copy(mnemonic = MNEMS(disMrmReg(dis, opcode)), op1 = Some(s"WORD PTR ${dis.op1.get}"))
    },
    // TEST reg8, [X]
    new Instruction(0x84,TEST) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        test(getReg8, getMem8,size = 8)

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // TEST reg16, [X]
    new Instruction(0x85,TEST) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        test(getReg16, getMem16,size = 16)

        // cycles
        if isRegMode then 3 // reg,reg 3
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // XCHG reg8, [X]
    new Instruction(0x86,XCHG) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        val reg8 = getReg8
        val mem8 = getMem8
        setReg8(mem8)
        setMem8(reg8)

        // cycles
        if isRegMode then 4 // reg,reg 4
        else 17 + getEACycles // mem -> reg 17+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // XCHG reg16, [X]
    new Instruction(0x87,XCHG) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        val reg16 = getReg16
        val mem16 = getMem16
        setReg16(mem16)
        setMem16(reg16)

        // cycles
        if isRegMode then 4 // reg,reg 4
        else 17 + getEACycles // mem -> reg 17+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // MOV [X], reg8
    new Instruction(0x88,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(getReg8)

        // cycles
        if isRegMode then 2 // reg,reg 2
        else 9 + getEACycles // reg -> mem 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disReg8)
    },
    // MOV [X], reg16
    new Instruction(0x89,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(getReg16)

        // cycles
        if isRegMode then 2 // reg,reg 2
        else 9 + getEACycles // mem -> reg 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disReg16)
    },
    // MOV reg8, [X]
    new Instruction(0x8A,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg8(getMem8)

        // cycles
        if isRegMode then 2 // reg,reg 2
        else 8 + getEACycles // mem -> reg 8+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg8,disEA8)
    },
    // MOV reg16, [X]
    new Instruction(0x8B,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setReg16(getMem16)

        // cycles
        if isRegMode then 2 // reg,reg 2
        else 8 + getEACycles // mem -> reg 8+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // MOV [X], seg
    new Instruction(0x8C,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(getSeg)

        // cycles
        if isRegMode then 2 // seg -> reg 2
        else 9 + getEACycles // seg -> mem 9+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA16,disSeg)
    },
    // LEA reg16, [X]
    new Instruction(0x8D,LEA) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        // if register is used instead of memory the real hw would use the internal microcode register IND: not emulated
        setReg16(getEffectiveAddress)

        // cycles
        2 + getEACycles // mem -> reg 2+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // MOV seg, [X]
    new Instruction(0x8E,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setSeg(getMem16)

        // cycles
        if isRegMode then 2 // reg -> seg 2
        else 8 + getEACycles // mem -> seg 8+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disSeg,disEA16)
    },
    // POP [X]
    new Instruction(0x8F,POP) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(regs.pop(mem))

        // cycles
        17 // mem 17

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis1Op(address,segmentOverridePrefix,prefixList,disEA16,mrm = true)
    },
    // 9X =========================================================================================================================================================================================================
    // XCHG reg16, AX
    new XCHGRegs(0x90,XCHG),
    new XCHGRegs(0x91,XCHG),
    new XCHGRegs(0x92,XCHG),
    new XCHGRegs(0x93,XCHG),
    new XCHGRegs(0x94,XCHG),
    new XCHGRegs(0x95,XCHG),
    new XCHGRegs(0x96,XCHG),
    new XCHGRegs(0x97,XCHG),
    // CBW
    new Instruction(0x98,CBW) {
      override protected final def process(): Int =
        if (regs.al & 0x80) != 0 then regs.ah = 0xFF
        else regs.ah = 0
        // cycles
        2

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // CWD
    new Instruction(0x99,CWD) {
      override protected final def process(): Int =
        if (regs.ax & 0x8000) != 0 then regs.dx = 0xFFFF
        else regs.dx = 0
        // cycles
        5

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // CALL FAR imm16:imm16
    new Instruction(0x9A,CALL) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val ofs = fetchNextWord()
        val seg = fetchNextWord()
        call(ofs,seg)
        // cycles
        28

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disFarImm(address,segmentOverridePrefix,prefixList).copy(far = true)
    },
    // WAIT
    new Instruction(0x9B,WAIT) {
      override protected final def process(): Int =
        // do nothing
        // cycles
        4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // PUSHF
    new Instruction(0x9C,PUSHF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.push(regs.getFlagWord,mem)
        // cycles
        10

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // POPF
    new Instruction(0x9D,POPF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.setFlagWord(regs.pop(mem))
        // cycles
        8

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // SAHF
    new Instruction(0x9E,SAHF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val flags = regs.getFlagWord & 0xFF00
        regs.setFlagWord(flags | (regs.ah & 0xD5) | 0x02)
        // cycles
        4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // LAHF
    new Instruction(0x9F,LAHF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val flags = regs.getFlagWord & 0xFF
        regs.ah = (flags & 0xD5) | 0x02
        // cycles
        4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address, segmentOverridePrefix, prefixList)
    },
    // AX =========================================================================================================================================================================================================
    // MOV AL, [imm16]
    new Instruction(0xA0,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val address = regs.ds(fetchNextWord())
        regs.al = mem.readByte(address)
        // cycles
        10 // mem -> accum

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address, segmentOverridePrefix, prefixList,"AL",size = 16)
        dis.copy(op2 = Some(s"[${dis.op2.get}]"))
    },
    // MOV AX, [imm16]
    new Instruction(0xA1,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val address = regs.ds(fetchNextWord())
        regs.ax = mem.readWord(address)
        // cycles
        10 // mem -> accum

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address, segmentOverridePrefix, prefixList,"AX",size = 16)
        dis.copy(op2 = Some(s"[${dis.op2.get}]"))
    },
    // MOV [imm16], AL
    new Instruction(0xA2,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val address = regs.ds(fetchNextWord())
        mem.writeByte(address,regs.al)
        // cycles
        10 // accum -> mem

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address, segmentOverridePrefix, prefixList,"AL",size = 16)
        dis.copy(op1 = Some(s"[${dis.op2.get}]"), op2 = dis.op1)
    },
    // MOV [imm16], AX
    new Instruction(0xA3,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val address = regs.ds(fetchNextWord())
        mem.writeWord(address,regs.ax)
        // cycles
        10 // accum -> mem

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address, segmentOverridePrefix, prefixList,"AX",size = 16)
        dis.copy(op1 = Some(s"[${dis.op2.get}]"), op2 = dis.op1)
    },
    // MOVSB
    new StringInstr(0xA4,MOVSB),
    // MOVSW
    new StringInstr(0xA5,MOVSW),
    // CMPSB
    new StringInstr(0xA6,CMPSB),
    // CMPSW
    new StringInstr(0xA7,CMPSW),
    // TEST AL, imm8
    new Instruction(0xA8,TEST) {
      override protected final def process(): Int =
        test(regs.al,fetchNextByte(),size = 8)

        // cycles
        4 // acc,imm 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // TEST AX, imm16
    new Instruction(0xA9,TEST) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        test(regs.ax,fetchNextWord(),size = 16)

        // cycles
        4 // acc,imm 4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 16)
    },
    // STOSB
    new StringInstr(0xAA,STOSB),
    // STOSW
    new StringInstr(0xAB,STOSW),
    // LODSB
    new StringInstr(0xAC,LODSB),
    // LODSW
    new StringInstr(0xAD,LODSW),
    // SCASB
    new StringInstr(0xAE,SCASB),
    // SCASW
    new StringInstr(0xAF,SCASW),
    // BX =========================================================================================================================================================================================================
    new MoveRegImm(0xB0,MOV,size = 8),
    new MoveRegImm(0xB1,MOV,size = 8),
    new MoveRegImm(0xB2,MOV,size = 8),
    new MoveRegImm(0xB3,MOV,size = 8),
    new MoveRegImm(0xB4,MOV,size = 8),
    new MoveRegImm(0xB5,MOV,size = 8),
    new MoveRegImm(0xB6,MOV,size = 8),
    new MoveRegImm(0xB7,MOV,size = 8),
    new MoveRegImm(0xB8,MOV,size = 16),
    new MoveRegImm(0xB9,MOV,size = 16),
    new MoveRegImm(0xBA,MOV,size = 16),
    new MoveRegImm(0xBB,MOV,size = 16),
    new MoveRegImm(0xBC,MOV,size = 16),
    new MoveRegImm(0xBD,MOV,size = 16),
    new MoveRegImm(0xBE,MOV,size = 16),
    new MoveRegImm(0xBF,MOV,size = 16),
    // CX =========================================================================================================================================================================================================
    // RET [imm16] (undocumented same as C3)
    new Instruction(0xC0,RET) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val off = fetchNextWord()
        regs.ip = regs.pop(mem)
        regs.sp += off
        // cycles
        20

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16)
    },
    // RET (undocumented same as C3)
    new Instruction(0xC1,RET) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ip = regs.pop(mem)
        // cycles
        16

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // RET [imm16]
    new Instruction(0xC2,RET) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val off = fetchNextWord()
        regs.ip = regs.pop(mem)
        regs.sp += off
        // cycles
        20

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16)
    },
    // RET
    new Instruction(0xC3,RET) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ip = regs.pop(mem)
        // cycles
        16

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // LES reg16, [X]
    new Instruction(0xC4,LES) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        // if register is used instead of memory the real hw would use the internal microcode register OPR: not emulated
        val addressOfs = getEffectiveAddress
        val offset = mem.readWord(addressOfs)
        val addressSeg = (addressOfs & 0xFFFF0000) | ((addressOfs & 0xFFFF) + 2) & 0xFFFF
        val segment = mem.readWord(addressSeg)
        setReg16(offset)
        regs.es = segment
        // cycles
        16 + getEACycles // mem -> reg 16

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // LDS reg16, [X]
    new Instruction(0xC5,LDS) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        // if register is used instead of memory the real hw would use the internal microcode register OPR: not emulated
        val addressOfs = getEffectiveAddress
        val offset = mem.readWord(addressOfs)
        val addressSeg = (addressOfs & 0xFFFF0000) | ((addressOfs & 0xFFFF) + 2) & 0xFFFF
        val segment = mem.readWord(addressSeg)
        setReg16(offset)
        regs.ds = segment
        // cycles
        16 + getEACycles // mem -> reg 16

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disReg16,disEA16)
    },
    // MOV [X], imm8
    new Instruction(0xC6,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem8(fetchNextByte())

        // cycles
        if isRegMode then 4 // reg,imm 4
        else 10 + getEACycles // imm -> mem 10+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disImm8)
    },
    // MOV [X], imm16
    new Instruction(0xC7,MOV) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        setMem16(fetchNextWord())

        // cycles
        if isRegMode then 4 // reg,imm 4
        else 10 + getEACycles // imm -> mem 10+EA

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = dis2Ops(address,segmentOverridePrefix,prefixList,disEA8,disImm16)
    },
    // RETF [imm16] (undocumented same as CA)
    new Instruction(0xC8,RETF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val off = fetchNextWord()
        regs.ip = regs.pop(mem)
        regs.cs = regs.pop(mem)
        regs.sp += off
        // cycles
        25

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16)
    },
    // RET (undocumented same as CB)
    new Instruction(0xC9,RETF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ip = regs.pop(mem)
        regs.cs = regs.pop(mem)
        // cycles
        26

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // RETF [imm16]
    new Instruction(0xCA,RETF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val off = fetchNextWord()
        regs.ip = regs.pop(mem)
        regs.cs = regs.pop(mem)
        regs.sp += off
        // cycles
        25

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16)
    },
    // RETF
    new Instruction(0xCB,RETF) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ip = regs.pop(mem)
        regs.cs = regs.pop(mem)
        // cycles
        26

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // INT3
    new Instruction(0xCC,INT3) {
      override final val canGenerateSoftwareInterrupt: Boolean = true
      override final def softwareInterrupt: Int = 3

      override protected final def process(): Int =
        // cycles
        52

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // INT imm8
    new Instruction(0xCD,INT) {
      private var interrupt = -1
      override final val canGenerateSoftwareInterrupt: Boolean = true
      override final def softwareInterrupt: Int = interrupt

      override protected final def process(): Int =
        reset(loadMRM = false)
        interrupt = fetchNextByte() & 0xFF
        // cycles
        51

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8)
    },
    // INTO
    new Instruction(0xCE,INTO) {
      private var interrupt = false
      override final val canGenerateSoftwareInterrupt: Boolean = true
      override final def softwareInterrupt: Int = if interrupt then 4 else -1

      override protected final def process(): Int =
        if regs.isFlags(F_OVERFLOW) then
          interrupt = true
          // cycles
          53
        else
          interrupt = false
          // cycles
          4

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // IRET
    new Instruction(0xCF,IRET) {
      override protected final def process(): Int =
        regs.ip = regs.pop(mem)
        regs.cs = regs.pop(mem)
        regs.setFlagWord(regs.pop(mem))
        // cycles
        32

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // DX =========================================================================================================================================================================================================
    new ShiftOps(0xD0,size = 8),
    new ShiftOps(0xD1,size = 16),
    new ShiftOps(0xD2,size = 8,countCL = true),
    new ShiftOps(0xD3,size = 16,countCL = true),
    // AAD imm8
    new Instruction(0xD4,AAM) {
      private var interrupt = -1
      override final val canGenerateSoftwareInterrupt: Boolean = true
      override final def softwareInterrupt: Int = interrupt

      override protected final def process(): Int =
        interrupt = -1
        reset(loadMRM = false)
        val div = fetchNextByte() & 0xFF
        if div == 0 then
          aam(div)
          interrupt = 0 // division by 0
          0
        else
          aam(div)
          83

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8)
    },
    // AAD imm8
    new Instruction(0xD5,AAD) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val mul = fetchNextByte() & 0xFF
        aad(mul)
        // cycles
        60

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8)
    },
    // SALC
    new Instruction(0xD6,SALC) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        salc()
        // cycles
        3

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // XLAT
    new Instruction(0xD7,XLATB) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val address = regs.ds(regs.bx + regs.al)
        regs.al = mem.readByte(address)
        // cycles
        11

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    new ESCInstr(0xD8,ESC),
    new ESCInstr(0xD9,ESC),
    new ESCInstr(0xDA,ESC),
    new ESCInstr(0xDB,ESC),
    new ESCInstr(0xDC,ESC),
    new ESCInstr(0xDD,ESC),
    new ESCInstr(0xDE,ESC),
    new ESCInstr(0xDF,ESC),
    // EX =========================================================================================================================================================================================================
    new LOOPInstr(0xE0,LOOPNZ),
    new LOOPInstr(0xE1,LOOPZ),
    new LOOPInstr(0xE2,LOOP),
    // JCXZ label
    new Instruction(0xE3,JCXZ) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val label = fetchNextByte()
        if regs.cx == 0 then
          regs.ip += label
          18 // cycles for branch taken
        else
          6 // cycles for branch not taken

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8,add = address + 2)
    },
    // IN AL, imm8
    new Instruction(0xE4,IN) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = io.in(fetchNextByte() & 0xFF,size8 = true)
        // cycles
        10

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
    },
    // IN AX, imm8
    new Instruction(0xE5,IN) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = io.in(fetchNextByte() & 0xFF,size8 = false)
        // cycles
        10

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 8)
    },
    // OUT imm8, AL
    new Instruction(0xE6,OUT) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        io.out(fetchNextByte() & 0xFF,regs.al,size8 = true)
        // cycles
        10

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address,segmentOverridePrefix,prefixList,"AL",size = 8)
        dis.copy(op1 = dis.op2,op2 = dis.op1)
    },
    // OUT imm8, AX
    new Instruction(0xE7,OUT) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        io.out(fetchNextByte() & 0xFF,regs.ax,size8 = false)
        // cycles
        10

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disImm(address,segmentOverridePrefix,prefixList,"AX",size = 8)
        dis.copy(op1 = dis.op2,op2 = dis.op1)
    },
    // CALL near imm16
    new Instruction(0xE8,CALL) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val ofs = fetchNextWord()
        call(ofs)
        // cycles
        19

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16,add = address + 3)
    },
    // JMP near imm16
    new Instruction(0xE9,JMP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val ofs = fetchNextWord()
        regs.ip += ofs
        // cycles
        15

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 16,add = address + 3)
    },
    // JMP far imm16:imm16
    new Instruction(0xEA,JMP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val ofs = fetchNextWord()
        val seg = fetchNextWord()
        regs.cs = seg
        regs.ip = ofs
        // cycles
        15

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disFarImm(address,segmentOverridePrefix,prefixList)
    },
    // JMP short imm8
    new Instruction(0xEB,JMP) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        val ofs = fetchNextByte()
        regs.ip += ofs
        // cycles
        15

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disImm(address,segmentOverridePrefix,prefixList,size = 8,add = address + 2)
    },
    // IN AL, DX
    new Instruction(0xEC,IN) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.al = io.in(regs.dx,size8 = true)
        // cycles
        9

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disNoOp(address,segmentOverridePrefix,prefixList)
        dis.copy(op1 = Some("AL"),op2 = Some("DX"))
    },
    // IN AX, DX
    new Instruction(0xED,IN) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        regs.ax = io.in(regs.dx,size8 = false)
        // cycles
        13

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disNoOp(address,segmentOverridePrefix,prefixList)
        dis.copy(op1 = Some("AX"),op2 = Some("DX"))
    },
    // OUT DX, AL
    new Instruction(0xEE,OUT) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        io.out(regs.dx,regs.al,size8 = true)
        // cycles
        9

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disNoOp(address,segmentOverridePrefix,prefixList)
        dis.copy(op1 = Some("DX"),op2 = Some("AL"))
    },
    // OUT DX, AX
    new Instruction(0xEF,OUT) {
      override protected final def process(): Int =
        reset(loadMRM = false)
        io.out(regs.dx,regs.ax,size8 = false)
        // cycles
        9

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = disNoOp(address,segmentOverridePrefix,prefixList)
        dis.copy(op1 = Some("DX"),op2 = Some("AX"))
    },
    // FX =========================================================================================================================================================================================================
    // LOCK
    new Instruction(0xF0,LOCK) {
      override protected final def process(): Int =
        // do nothing: will be recognized as a prefix and discarded
        // cycles
        2

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // LOCK (undocumented)
    new Instruction(0xF1,LOCK) {
      override protected final def process(): Int =
        // do nothing: will be recognized as a prefix and discarded
        // cycles
        2

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // REPNE
    new Instruction(0xF2,REPNE) {
      override val repIndex : Int = Registers.REPN

      override protected final def process(): Int =
        regs.setRepN()
        // cycles
        0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // REP
    new Instruction(0xF3,InstructionCode.REP) {
      override val repIndex : Int = Registers.REP

      override protected final def process(): Int =
        regs.setRep()
        // cycles
        0

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // HLT
    new Instruction(0xF4,HLT) {
      override protected final def process(): Int =
        // do nothing: will be recognized as halt instruction
        // cycles
        2

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // CMC
    new Instruction(0xF5,CMC) {
      override protected final def process(): Int =
        regs.setFlag(F_CARRY,!regs.isFlags(F_CARRY))
        // cycles
        2

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled = disNoOp(address,segmentOverridePrefix,prefixList)
    },
    // Group 3 F6/F7
    new GPR3Ops(0xF6,size = 8),
    new GPR3Ops(0xF7,size = 16),
    // CLC, STC, CLI, STI, CLD, STD
    new ClearSetFlagInstr(0xF8,CLC),
    new ClearSetFlagInstr(0xF9,STC),
    new ClearSetFlagInstr(0xFA,CLI),
    new ClearSetFlagInstr(0xFB,STI),
    new ClearSetFlagInstr(0xFC,CLD),
    new ClearSetFlagInstr(0xFD,STD),
    // FE GROUP INC/DEC [X] 8 bit
    new Instruction(0xFE,TBD) {
      override protected final def process(): Int =
        reset(loadMRM = true)
        val old_carry = regs.isFlags(F_CARRY)

        getMRMReg match
          case 0 =>
            setMem8(add8(getMem8,1))
          case 1 =>
            setMem8(sub8(getMem8,1))
          case _ =>
        regs.setFlag(F_CARRY, old_carry)
        // cycles
        if isRegMode then 3 else 15 + getEACycles

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = dis1Op(address, segmentOverridePrefix, prefixList,disEA8, mrm = true)
        val mnem = disMrmReg(dis, opcode) match
          case 0 => INC
          case 1 => DEC
          case _ => TBD
        dis.copy(mnemonic = mnem)
    },
    // FF GROUP INC/DEC [X] 16 bit, CALL near, CALL far, JMP near, JMP far, PUSH, PUSH
    new Instruction(0xFF,TBD) {
      override protected final def process(): Int =
        reset(loadMRM = true)

        getMRMReg match
          case r@(0|1) => // INC/DEC [X] 16 bit
            val old_carry = regs.isFlags(F_CARRY)
            if r == 0 then setMem16(add16(getMem16,1)) else setMem16(sub16(getMem16,1))
            regs.setFlag(F_CARRY, old_carry)
            // cycles
            if isRegMode then 3 else 15 + getEACycles
          case 2 => // CALL near [X]
            val ofs = getMem16
            call(ofs,addIP = false)
            // cycles
            if isRegMode then 16 else 21 + getEACycles
          case 3 => // CALL far [X]
            val addressOfs = getEffectiveAddress
            val offset = mem.readWord(addressOfs)
            val addressSeg = (addressOfs & 0xFFFF0000) | ((addressOfs & 0xFFFF) + 2) & 0xFFFF
            val segment = mem.readWord(addressSeg)
            call(ip = offset,cs = segment)
            // cycles
            37
          case 4 => // JMP near [X]
            regs.ip = getMem16
            // cycles
            if isRegMode then 11 else 18 + getEACycles
          case 5 => // JMP far [X]
            val addressOfs = getEffectiveAddress
            val offset = mem.readWord(addressOfs)
            val addressSeg = (addressOfs & 0xFFFF0000) | ((addressOfs & 0xFFFF) + 2) & 0xFFFF
            val segment = mem.readWord(addressSeg)
            regs.ip = offset
            regs.cs = segment
            // cycles
            24
          case 6|7 => // PUSH [X] 7 undocumented
            // cycles
            var toPush = getMem16
            if isSPRegMode then // special case: PUSH SP, sp must be decreased before pushing
              toPush = (toPush - 2) & 0xFFFF
            regs.push(toPush,mem)
            if isRegMode then 10 else 16 + getEACycles

      override def disassemble(address: Int, segmentOverridePrefix: Int, repPrefix: Int, prefixList: List[Int]): Instruction.Disassembled =
        val dis = dis1Op(address, segmentOverridePrefix, prefixList,_ => "", mrm = true)
        disMrmReg(dis, opcode) match
          case 0 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = INC)
          case 1 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = DEC)
          case 2 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = CALL)
          case 3 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = CALL,far = true)
          case 4 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = JMP)
          case 5 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = JMP,far = true)
          case 6|7 =>
            dis1Op(address, segmentOverridePrefix, prefixList,disEA16, mrm = true).copy(mnemonic = PUSH)
          case _ =>
            dis
    },
  )
