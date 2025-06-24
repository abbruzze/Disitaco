package ucesoft.disitaco.chips

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.cpu.i8088

import javax.swing.ImageIcon

object i8259:
  trait INTHandler:
    def int(enabled:Boolean): Unit

/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/03/2025 14:12  
 */
class i8259 extends PCComponent with i8088.IntrAck:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/interrupt.png"))
  override val componentName = "8259"
  import i8259.*
  private enum Mode:
    case FullyNestedMode, RotatingPriorityMode, SpecialMaskMode, PolledMode
  private enum State:
    case NotInitialized, ICW1, ICW2, ICW3, Ready

  import Mode.*
  import State.*

  private var intHandler : INTHandler = _ => {}
  // State
  private var state = NotInitialized
  // irqs levels
  private var irqLevels = 0
  // Interrupt Request Register
  private var irr = 0
  // Interrupt Service Register
  private var isr = 0
  // Interrupt Mask Register
  private var imr = 0
  // mode
  private var mode = FullyNestedMode
  // edge sense mode
  private var edgeSensed = true
  // special mask mode
  private var specialMaskMode = false
  // status read IRR
  private var statusReadIRR = true
  // auto EOI
  private var autoEOI = false
  // cascade mode
  private var cascadeMode = false
  // IC4
  private var ic4 = false
  // int vector
  private var intVector = 0
  // poll command
  private var pollCommand = false
  // rotation mode
  private var rotateAutoEOI = false
  // ocw2 level
  private var ocw2Level = 0

  private var highestPriorityIndex = 0
  // interrupt signal towards CPU
  private var INTR = false

  final def setIntHandler(ih:INTHandler): Unit = intHandler = ih

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    List(
      Property("State",state.toString),
      Property("IRQ levels",irqLevels.toString),
      Property("irr",irr.toString),
      Property("isr",isr.toString),
      Property("imr",imr.toString),
      Property("Mode",mode.toString),
      Property("Edge sensed",edgeSensed.toString),
      Property("Auto EOI",autoEOI.toString),
      Property("IRQ signal",INTR.toString),
    )

  override protected def reset(): Unit =
    irqLevels = 0
    setINTR(false)
    initialize()

  private def initialize(): Unit =
    state = NotInitialized
    highestPriorityIndex = 0
    irr = 0
    isr = 0
    imr = 0
    edgeSensed = true
    specialMaskMode = false
    statusReadIRR = true
    autoEOI = false
    intVector = 0
    cascadeMode = false
    pollCommand = false
    rotateAutoEOI = false

  final def setIRQ(irq:Int,set:Boolean): Unit =
    val irqBit = 1 << irq
    if edgeSensed then
      if (irqLevels & irqBit) == 0 && set then irr |= irqBit
      else if (irqLevels & irqBit) != 0 && !set then irr &= ~irqBit
    else
      if set then irr |= 1 << irq else irr &= ~(1 << irq)

    if set then irqLevels |= irqBit else irqLevels &= ~irqBit

    checkIRQ()
  end setIRQ

  private def setINTR(value:Boolean): Unit =
    if value != INTR then intHandler.int(value)
    INTR = value

  private def checkIRQ(): Unit =
    if state == NotInitialized then return

    val irq = findHighPriorityBit(irr,imr)
    if irq == -1 then setINTR(false)
    else
      val sirq = findHighPriorityBit(isr)
      if specialMaskMode then
        if irq != sirq then setINTR(true)
      else if sirq == -1 || irq < sirq then setINTR(true)


  override final def intrAck(): Int = intVector | ackIRQ()
  /*
    Upon receiving an INTA from the CPU group, the
    highest priority ISR bit is set and the corresponding
    IRR bit is reset.
    */
  private def ackIRQ(): Int =
    var irq = if autoEOI then eoiCommand(specific = false, rotate = rotateAutoEOI) else findHighPriorityBit(irr, imr)
    /*
    If no interrupt request is present at step 4 of either
    sequence (i.e., the request was too short in duration)
    the 8259A will issue an interrupt level 7. Both the
    vectoring bytes and the CAS lines will look like an
    interrupt level 7 was requested.
    */
    if irq == -1 then irq = 7
    val irqBit = 1 << irq

    if !autoEOI then
      isr |= irqBit

    if edgeSensed || (irqLevels & irqBit) == 0 then
      irr &= ~irqBit

    checkIRQ()

    irq
  end ackIRQ

  final def read(a0:Int): Int =
    a0 & 1 match
      case 0 =>
        if pollCommand then
          ackIRQ() | (if INTR then 0x80 else 0)
        else if statusReadIRR then irr else isr
      case 1 =>
        imr
  end read

  final def write(a0:Int,value:Int): Unit =
    //log.info("%s writing a0=%d = %02X",componentName,a0,value)
    a0 & 1 match
      case 0 =>
        if (value & 0x10) == 0x10 then processICW1(value)
        else if (value & 0x18) == 0 then processOCW2(value)
        else if (value & 0x18) == 0x8 then processOCW3(value)
        else
          log.error(s"Undefined command with a0=$a0 value=$value")
      case 1 =>
        state match
          case ICW1 =>
            processICW2(value)
          case ICW2 =>
            processICW3(value)
          case ICW3 =>
            processICW4(value)
          case NotInitialized =>
            log.error(s"Unexpected command on state $state with value $value")
          case Ready =>
            processOCW1(value)
  end write

  private def processICW1(value: Int): Unit =
    initialize()
    ic4 = (value & 1) != 0
    // Cascade mode is not supported
    cascadeMode = (value & 2) == 0
    if cascadeMode then
      println("Cascade mode not supported")
    edgeSensed = (value & 8) == 0
    state = ICW1
    log.info("%s ICW1 cascadeMode=%b edgeSensed=%b", componentName,cascadeMode,edgeSensed)
  private def processICW2(value: Int): Unit =
    // for 8086 processor only
    intVector = value & 0xF8
    if cascadeMode then state = ICW2
    else if ic4 then state = ICW3
    else
      state = Ready
    log.info("%s ICW2 intVector=%02X state=%s", componentName, intVector, state)
  private def processICW3(value: Int): Unit =
    // ignored
    state = if ic4 then ICW3 else Ready
    log.info("%s ICW3", componentName)
  private def processICW4(value: Int): Unit =
    if (value & 1) == 0 then
      log.error("%s ICW4 Bad processor",componentName)
    autoEOI = (value & 2) != 0
    state = Ready
    log.info("%s ICW4 autoEOI=%b", componentName, autoEOI)
  private def processOCW1(value: Int): Unit =
    imr = value & 0xFF
    log.info("%s OCW1 imr=%02X",componentName,imr)
    checkIRQ()
  private def processOCW2(value: Int): Unit =
    ocw2Level = value & 7
    //log.info("%s OCW2 ocw2Level=%02X",componentName,ocw2Level)
    (value >> 5) & 7 match
      case 0 => rotateAutoEOI = false
      case 1 => eoiCommand(specific = false)
      case 2 => // no operation
      case 3 => eoiCommand(specific = true)
      case 4 => rotateAutoEOI = true
      case 5 => eoiCommand(specific = false,rotate = true)
      case 6 => setBottomPriority(ocw2Level)
      case 7 => eoiCommand(specific = true,rotate = true)
  private def processOCW3(value: Int): Unit =
    log.info("%s OCW3 value=%02X",componentName,value)
    value & 3 match
      case 1 =>
        statusReadIRR = true
      case 3 =>
        statusReadIRR = false
      case _ =>
    pollCommand = (value & 4) != 0
    (value >> 5) & 3 match
      case 1 =>
        specialMaskMode = false
      case 3 =>
        specialMaskMode = true
      case _ =>
  end processOCW3

  private def setBottomPriority(bottomLevel:Int): Unit =
    highestPriorityIndex = (bottomLevel + 9) & 7

  private def findHighPriorityBit(reg:Int,mask:Int = 0x00): Int =
    val intr = reg & ~mask
    if intr == 0 then return -1
    var count = 0
    var bit = highestPriorityIndex
    while count < 8 do
      if (intr & (1 << bit)) != 0 then return bit
      count += 1
      bit = (bit + 1) & 7
    -1

  private def eoiCommand(specific:Boolean,rotate:Boolean = false): Int =
    //log.info("%s EOI command",componentName)
    val isrBit = if specific then ocw2Level else findHighPriorityBit(isr)
    if isrBit != -1 then
      //log.info("%s clearing %d bit of isr",componentName,isrBit)
      isr &= ~(1 << isrBit)
      if rotate then
        setBottomPriority(isrBit)

    isrBit
