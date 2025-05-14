package ucesoft.disitaco.cpu

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.cpu.Instruction.Disassembled

import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

object i8088:
  trait IntrAck:
    def intrAck(): Int

  trait ESCHandler:
    def esc(escCode:Int,word:Int): Unit

  trait BusListener:
    def busRead(address:Int,byteRead:Boolean): Unit
    def busWrite(address:Int,value:Int,byteWrite:Boolean): Unit
    def fetch(address:Int): Unit
    def interrupt(intNum:Int): Unit
    def reset(): Unit
    def halted(): Unit

  trait InterruptHandler:
    def interrupt(regs:Registers): Boolean
/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/02/2025 09:48  
 */
class i8088(val mem:Memory, val inOut: InOut, val intrAck:i8088.IntrAck) extends PCComponent:
  override val componentName = "CPU"
  import i8088.*

  private enum IntrType(val clocks:Int):
    case MASKABLE extends IntrType(61)
    case NMI extends IntrType(50)
    case INT_VECTOR extends IntrType(51)
    case SINGLE_STEP extends IntrType(50)
    
  final val regs = new Registers
  private final val instructions = Array.ofDim[Instruction](256)
  private final val interruptHandlers = Array.ofDim[InterruptHandler](256)

  private var lastPrefixFound = true

  private var nmiPending = false
  private var maskablePending = false
  private var trapPending = false
  private var intExecuted = -1

  private var halted = false
  private var repeated = false

  private var busListener : BusListener = uninitialized

  private var lastInstructionIP = 0
  private var lastElapsedCycles = 0
  private var totalElapsedCycles = 0L
  
  private var pendingReset = false

  buildInstructionSet()
  
  def setInterruptHandler(intNumber:Int,handler:InterruptHandler): Unit =
    interruptHandlers(intNumber) = handler
    
  def removeBusListener(): Unit = setBusListener(null)
  def setBusListener(bl:BusListener): Unit =
    this.busListener = bl
    val memory = if bl != null then new Memory:
      private final val busListener = bl
      override def readByte(address: Int, abs: Boolean): Int =
        busListener.busRead(address = address, byteRead = true)
        mem.readByte(address,abs)
      override def readWord(address: Int, abs: Boolean): Int =
        busListener.busRead(address = address, byteRead = false)
        mem.readWord(address, abs)
      override def writeByte(address: Int, value: Int, abs: Boolean): Unit =
        busListener.busWrite(address = address, value = value, byteWrite = true)
        mem.writeByte(address,value,abs)
      override def writeWord(address: Int, value: Int): Unit =
        busListener.busWrite(address = address, value = value, byteWrite = false)
        mem.writeWord(address, value)
    else
      mem

    for i <- instructions do
      i.init(regs,memory,inOut)
  end setBusListener

  def setESCHandler(eh:ESCHandler): Unit =
    for i <- instructions do
      i.setESCHandler(eh)

  private def buildInstructionSet(): Unit =
    for i <- InstructionSet.opcodes do
      instructions(i.opcode) = i
      i.init(regs,mem,inOut)

  override protected def reset(): Unit =
    regs.reset()
    nmiPending = false
    halted = false
    totalElapsedCycles = 0L
    pendingReset = true

  def raiseNMI(): Unit = nmiPending = true
  def raiseINTR(active:Boolean): Unit = maskablePending = active

  private def interrupt(intrType: IntrType,intrByte:Int = -1): Unit =
    import Registers.*
    import IntrType.*

    halted = false
    intExecuted = intrType match
      case NMI => 2
      case MASKABLE => intrAck.intrAck() // ack interrupt and get int vector
      case INT_VECTOR => intrByte
      case SINGLE_STEP => 1
      
    val handler = interruptHandlers(intExecuted)
    if handler != null && handler.interrupt(regs) then {}
    else
      regs.push(regs.getFlagWord,mem)
      regs.clearFlags(F_INT | F_TRAP)
      regs.push(regs.cs,mem)
      regs.push(regs.ip,mem)
  
      val address = intExecuted << 2
      regs.ip = mem.readWord(address)
      regs.cs = mem.readWord(address + 2)

    if busListener != null then
      busListener.interrupt(intExecuted)
  end interrupt

  final def execute(): Int =
    import IntrType.*
    import Registers.*
    
    if pendingReset then
      pendingReset = false
      if busListener != null then
        busListener.reset()

    // ============= Interrupts ================
    intExecuted = -1

    if nmiPending then
      nmiPending = false
      interrupt(NMI)
      return NMI.clocks
    else if maskablePending then
      if regs.isFlags(F_INT) then
        interrupt(MASKABLE)
        return MASKABLE.clocks
    else if trapPending then
      trapPending = false
      if regs.isFlags(F_TRAP) then
        interrupt(SINGLE_STEP)
        return SINGLE_STEP.clocks
    // ======= Halt check =======================
    if halted then
      if busListener != null then
        val address = regs.cs(lastInstructionIP,ignoreSegmentOverride = true)
        busListener.fetch(((((address >>> 16) & 0xFFFF) << 4) + (address & 0xFFFF)) & 0xF_FFFF)
      return 1
    // ==========================================
    lastPrefixFound = true
    var i : Instruction = null

    lastInstructionIP = regs.ip
    if busListener != null then
      val address = regs.cs(lastInstructionIP,ignoreSegmentOverride = true)
      busListener.fetch(((((address >>> 16) & 0xFFFF) << 4) + (address & 0xFFFF)) & 0xF_FFFF)

    while lastPrefixFound do
      val address = regs.cs(regs.ip,ignoreSegmentOverride = true)
      val opcode = mem.readByte(address)
      regs.incIP(1)
      i = instructions(opcode)
      if i == null then throw new IllegalArgumentException(s"Invalid opcode ${opcode.toHexString}")

      lastElapsedCycles = i.execute()
      repeated = i.hasBeenRepeated
      
      lastPrefixFound = i.isPrefix
    end while

    if i.opcode == 0xF4 then // HLT instruction
      halted = true
      if busListener != null then
        busListener.halted()
    else if i.canGenerateSoftwareInterrupt then
      val intr = i.softwareInterrupt
      if intr != -1 then
        interrupt(INT_VECTOR,intr)

    trapPending = regs.isFlags(F_TRAP)

    totalElapsedCycles += lastElapsedCycles
    lastElapsedCycles
  end execute
  
  final def getLastElapsedCycles: Int = lastElapsedCycles
  final def getTotalElapsedCycles: Long = totalElapsedCycles

  final def getLastInstructionIP: Int = lastInstructionIP
  final def getLastInstructionAddress: Int = (regs.cs << 4 + lastInstructionIP) & 0xFFFFF
  final def isPrefixFound: Boolean = lastPrefixFound
  final def isInstructionHasBeenRepeated: Boolean = repeated
  final def getInterruptVector: Int = intExecuted
  
  def disassemble(_address:Int,abs:Boolean = true): Disassembled =
    var absoluteAddress = if abs then _address else ((((_address >>> 16) & 0xFFFF) << 4) + (_address & 0xFFFF)) & 0xF_FFFF
    var segOverridePrefix = -1
    var repPrefix = -1
    var prefixFound = true
    var i : Instruction = null
    val prefixList = new ListBuffer[Int]

    while prefixFound do
      val opcode = mem.readByte(absoluteAddress,abs = true)
      i = instructions(opcode)
      if i == null then throw new IllegalArgumentException(s"Opcode ${opcode.toHexString} not implemented")
      
      if i.isSegmentOverrideInstruction then
        segOverridePrefix = i.segmentOverrideIndex
        absoluteAddress += 1
        prefixList += opcode
      else if i.isRepInstruction then
        repPrefix = i.repIndex
        absoluteAddress += 1
        prefixList += opcode
      else
        prefixFound = false
    end while

    i.disassemble(absoluteAddress,segOverridePrefix,repPrefix,prefixList.toList)
  end disassemble
  
      
