package ucesoft.disitaco.chips

import ucesoft.disitaco.PCComponent

import javax.swing.ImageIcon

object INS8250:
  trait SerialDevice:
    val name : String
    def setMaster(master:SerialMaster): Unit = {}
    def dtr(on:Boolean): Unit = {}
    def rts(on:Boolean): Unit = {}
    def setTXByte(byte:Int): Unit = {}
    def checkRXByte(): Unit = {}
  trait SerialMaster:
    def cts(on:Boolean): Unit = {}
    def dsr(on: Boolean): Unit = {}
    def rlsd(on: Boolean): Unit = {} // Received Line Signal Detect
    def ri(on:Boolean): Unit = {}
    def setRXByte(byte:Int): Unit
  trait SignalListener:
    def dtrChanged(dtr:Boolean): Unit
    def rtsChanged(rts:Boolean): Unit
    def ctsChanged(cts:Boolean): Unit
    def dsrChanged(dsr:Boolean): Unit
    def rlsdChanged(rlsd:Boolean): Unit
    def riChanged(ri:Boolean): Unit

/**
 * @author Alessandro Abbruzzetti
 *         Created on 21/05/2025 11:26
 *
 * Line Control Register
 * -------------------------------------------------------------------------------
 * 0 Word Length select bit 0 |
 * 1 Word Length select bit 1 | 00 = 5 bits, 01 = 6 bits, 10 = 7bits, 11 = 8bits
 * 2 Number of stop bits, 0 = 1 stop, 1 = 2 stop if word = 6-7-8
 * 3 Parity enable
 * 4 Even parity select
 * 5 Stick parity
 * 6 Set break
 * 7 Divisor Latch Access Bit (DLAB)
 *
 * Line Status Register
 * -------------------------------------------------------------------------------
 * 0 Data Ready
 * 1 Overrun Error
 * 2 Parity Error
 * 3 Framing Error
 * 4 Break Interrupt
 * 5 Transmitter Holding Register Empty
 * 6 Tx Shift Register empty
 * 7 0
 *
 * Interrupt Identification Register
 * -------------------------------------------------------------------------------
 * 0 0 If interrupt is pending
 * 1 Interrupt ID 0
 * 2 Interrupt ID 1
 *      001 No Interrupt
 *      110 Receiver Line Status (Overrun, Parity, Framing, Break)
 *      100 Received Data Available
 *      010 Transmitter Holding Register empty
 *      000 Modem Status (CTS, DSR, RI, RLSD)
 * 3-7 0
 *
 * Interrupt Enable Register
 * -------------------------------------------------------------------------------
 * 0 Enable Data Available Interrupt
 * 1 Enable Tx Holding Register Empty Interrupt
 * 2 Enable Receiver Line Status Interrupt
 * 3 Enable Modem Status Interrupt
 * 4-7 0
 *
 * Modem Control Register
 * -------------------------------------------------------------------------------
 * 0 DTR
 * 1 RTS
 * 2 Out 1
 * 3 Out 2
 * 4 Loop
 * 5-7 0
 *
 * Modem Status Register
 * -------------------------------------------------------------------------------
 * 0 Delta CTS
 * 1 Delta DSR
 * 2 Delta RI
 * 3 Delta RLSD
 * 4 CTS
 * 5 DSR
 * 6 RI
 * 7 RLSD
 */
class INS8250(comIndex:Int,masterClockFreq:Int,irq: Boolean => Unit) extends PCComponent with INS8250.SerialMaster:
  import INS8250.*
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/serial.png"))
  override val componentName = s"COM$comIndex"

  private class NULL_DEVICE extends SerialDevice:
    override val name : String = "NULL"
    dsr(false)
    rlsd(false)

    override def setTXByte(byte: Int): Unit = print(byte.toChar)

    override def dtr(on: Boolean): Unit = if on then dsr(on = true)
    override def rts(on: Boolean): Unit = if on then cts(on = true)

  private inline val SERIAL_CLOCK = 1_843_200

  private inline val EMPTY_BUFFER = -1

  // registers
  private inline val REG_RECEIVER_BUFFER      = 0 // read only
  private inline val REG_TRANSMITTER_BUFFER   = 1 // write only
  private inline val REG_INTERRUPT_ENABLE     = 2
  private inline val REG_INTERRUPT_IDENTIFY   = 3 // read only
  private inline val REG_LINE_CONTROL         = 4
  private inline val REG_MODEM_CONTROL        = 5
  private inline val REG_LINE_STATUS          = 6
  private inline val REG_MODEM_STATUS         = 7
  private inline val REG_SCRATCH_PAD          = 8
  private inline val REG_DIVISOR_LSB          = 9
  private inline val REG_DIVISOR_MSB          = 10
  // interrupts
  private inline val INT_NONE             = 0b001
  private inline val INT_REC_LINE_STATUS  = 0b110
  private inline val INT_REC_DATA         = 0b100
  private inline val INT_TX_EMPTY         = 0b010
  private inline val INT_MODEM_STATUS     = 0b000

  private inline val OUT2_INT_ENABLE      = 0x8

  private inline val INT_MASK_REC_LINE_STATUS = 4
  private inline val INT_MASK_REC_DATA        = 1
  private inline val INT_MASK_TX_EMPTY        = 2
  private inline val INT_MASK_MODEM_STATUS    = 8
  // line status bits
  private inline val LINE_STATUS_DATA_READY   = 0x01
  private inline val LINE_STATUS_OVERRUN      = 0x02
  private inline val LINE_STATUS_PARITY_ERR   = 0x04
  private inline val LINE_STATUS_TX_EMPTY     = 0x20
  private inline val LINE_STATUS_TX_SH_EMPTY  = 0x40

  /*
   * 0 Enable Data Available Interrupt
   * 1 Enable Tx Holding Register Empty Interrupt
   * 2 Enable Receiver Line Status Interrupt
   * 3 Enable Modem Status Interrupt
   * 4-7 0
  */
  private var activeInterrupts = 0

  private val registers = Array.ofDim[Int](11)
  // signals
  private inline val DTR = 0
  private inline val RTS = 1
  private inline val CTS = 2
  private inline val DSR = 3
  private inline val RLSD = 4
  private inline val RI = 5

  private final val signals = Array.ofDim[Boolean](6)
  private var device: SerialDevice = new NULL_DEVICE
  private var signalListener : SignalListener = new SignalListener:
    override def dtrChanged(dtr: Boolean): Unit =
      println(s"Serial: DTR=$dtr")
    override def rtsChanged(rts: Boolean): Unit =
      println(s"Serial: RTS=$rts")
    override def ctsChanged(cts: Boolean): Unit =
      println(s"Serial: CTS=$cts")
    override def dsrChanged(dsr: Boolean): Unit =
      println(s"Serial: DSR=$dsr")
    override def rlsdChanged(rlsd: Boolean): Unit =
      println(s"Serial: RLSD=$rlsd")
    override def riChanged(ri: Boolean): Unit =
      println(s"Serial: RI=$ri")

  private var txShiftRegister = EMPTY_BUFFER

  private val internalTickCycles = masterClockFreq.toDouble / SERIAL_CLOCK
  private var tickCycles = 0.0
  private var bitCycles = 0
  private var bitCounterCycles = 0
  private var bitCount = 0
  private var byteBitLen = 0

  private var txByteCount = 0
  private var rxByteCount = 0

  reset()

  override protected def reset(): Unit =
    txShiftRegister = EMPTY_BUFFER
    registers(REG_RECEIVER_BUFFER) = EMPTY_BUFFER
    registers(REG_TRANSMITTER_BUFFER) = EMPTY_BUFFER
    registers(REG_INTERRUPT_ENABLE) = 0
    registers(REG_INTERRUPT_IDENTIFY) = INT_NONE
    registers(REG_LINE_CONTROL) = 0
    registers(REG_MODEM_CONTROL) = 0
    registers(REG_LINE_STATUS) = LINE_STATUS_TX_EMPTY | LINE_STATUS_TX_SH_EMPTY
    registers(REG_MODEM_STATUS) &= 0xF0
    activeInterrupts = 0

    txByteCount = 0
    rxByteCount = 0
    checkIRQ()
  end reset

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    List(
      Property("TX byte transmitted",txByteCount.toString),
      Property("RX byte received",rxByteCount.toString),
      Property("Baud",if bitCycles != 0 then (SERIAL_CLOCK / bitCycles).toString else "-"),
      Property("RTS",signals(RTS).toString),
      Property("CTS",signals(CTS).toString),
      Property("DTR",signals(DTR).toString),
      Property("DSR",signals(DSR).toString),
      Property("RLSD",signals(RLSD).toString),
      Property("RI",signals(RI).toString),
      Property("Register receiver buffer",registers(REG_RECEIVER_BUFFER).toHexString),
      Property("Register transmitter buffer",registers(REG_TRANSMITTER_BUFFER).toHexString),
      Property("Register interrupt enable",registers(REG_INTERRUPT_ENABLE).toHexString),
      Property("Register interrupt identify",registers(REG_INTERRUPT_IDENTIFY).toHexString),
      Property("Register line control",registers(REG_LINE_CONTROL).toHexString),
      Property("Register modem control",registers(REG_MODEM_CONTROL).toHexString),
      Property("Register line status",registers(REG_LINE_STATUS).toHexString),
      Property("Register modem status",registers(REG_MODEM_STATUS).toHexString),
      Property("Register divisor LSB",registers(REG_DIVISOR_LSB).toHexString),
      Property("Register divisor MSB",registers(REG_DIVISOR_MSB).toHexString),
    )

  // =================== Signals handling ==========================

  private def setSignal(signal:Int,on:Boolean): Unit =
    if signals(signal) != on then
      signals(signal) = on
      signal match
        case DTR =>
          if loopMode then
            setSignal(DSR,on)
          else
            device.dtr(on)
          signalListener.dtrChanged(on)
        case RTS =>
          if loopMode then
            setSignal(CTS,on)
          else
            device.rts(on)
          signalListener.rtsChanged(on)
        // updates Delta flags on MODEM STATUS
        case CTS =>
          registers(REG_MODEM_STATUS) |= 1
          signalListener.ctsChanged(on)
          checkModemStatusInterrupt()
        case DSR =>
          registers(REG_MODEM_STATUS) |= 2
          signalListener.dsrChanged(on)
          checkModemStatusInterrupt()
        case RLSD =>
          registers(REG_MODEM_STATUS) |= 8
          signalListener.rlsdChanged(on)
          checkModemStatusInterrupt()
        case RI =>
          registers(REG_MODEM_STATUS) |= 4
          signalListener.riChanged(on)
          checkModemStatusInterrupt()
    end if
  end setSignal

  override def cts(on: Boolean): Unit =
    setSignal(CTS,on)
    if on then registers(REG_MODEM_STATUS) |= 0x10 else registers(REG_MODEM_STATUS) &= ~0x10
  override def dsr(on: Boolean): Unit =
    setSignal(DSR, on)
    if on then registers(REG_MODEM_STATUS) |= 0x20 else registers(REG_MODEM_STATUS) &= ~0x20
  override def rlsd(on: Boolean): Unit =
    setSignal(RLSD, on)
    if on then registers(REG_MODEM_STATUS) |= 0x80 else registers(REG_MODEM_STATUS) &= ~0x80
  override def ri(on: Boolean): Unit =
    setSignal(RI, on)
    if on then registers(REG_MODEM_STATUS) |= 0x40 else registers(REG_MODEM_STATUS) &= ~0x40

  def setDevice(device: SerialDevice): Unit =
    this.device = device
    device.setMaster(this)

  def setSignalListener(sl:SignalListener): Unit =
    signalListener = sl

  private inline def dlab: Boolean = (registers(REG_LINE_CONTROL) & 0x80) != 0
  private inline def loopMode: Boolean = (registers(REG_MODEM_CONTROL) & 0x10) != 0

  def readRegister(address:Int): Int =
    log.info("%s reading register %d",componentName,address)
    address & 7 match
      case 0 => // 3F8
        if dlab then
          registers(REG_DIVISOR_LSB)
        else
          // clear Data Ready in Line Status Register
          registers(REG_LINE_STATUS) &= ~LINE_STATUS_DATA_READY

          // clear INT Received Data Available
          checkIRQ(disableMask = INT_MASK_REC_DATA)

          val data = registers(REG_RECEIVER_BUFFER)
          registers(REG_RECEIVER_BUFFER) = EMPTY_BUFFER
          //println(s"SERIAL READ: $data")
          data
      case 1 => // 3F9
        if dlab then
          registers(REG_DIVISOR_MSB)
        else
          registers(REG_INTERRUPT_ENABLE)
      case 2 => // 3FA
        // apply priorities
        registers(REG_INTERRUPT_IDENTIFY) =
        if (activeInterrupts & INT_MASK_REC_LINE_STATUS) != 0 then INT_REC_LINE_STATUS  // Highest
        else if (activeInterrupts & INT_MASK_REC_DATA) != 0 then INT_REC_DATA           // Second
        else if (activeInterrupts & INT_MASK_TX_EMPTY) != 0 then INT_TX_EMPTY           // Third
        else if (activeInterrupts & INT_MASK_MODEM_STATUS) != 0 then INT_MODEM_STATUS   // Fourth
        else INT_NONE

        // clear INT Transmitter Holding Register Empty
        checkIRQ(disableMask = INT_MASK_TX_EMPTY)

        registers(REG_INTERRUPT_IDENTIFY)
      case 3 => // 3FB
        registers(REG_LINE_CONTROL)
      case 4 => // 3FC
        registers(REG_MODEM_CONTROL)
      case 5 => // 3FD
        val status = registers(REG_LINE_STATUS)

        // Clear Overrun error in Line Status Register
        registers(REG_LINE_STATUS) &= ~LINE_STATUS_OVERRUN
        // Clear Parity error in Line Status Register
        registers(REG_LINE_STATUS) &= ~LINE_STATUS_PARITY_ERR
        checkLineStatusInterrupt()
        status
      case 6 => // 3FE
        var ms = registers(REG_MODEM_STATUS) & 0xF
        if signals(CTS) then ms |= 0x10
        if signals(DSR) then ms |= 0x20
        if signals(RI) then ms |= 0x40
        if signals(RLSD) then ms |= 0x80
        // clear delta bits
        registers(REG_MODEM_STATUS) &= 0xF0
        checkModemStatusInterrupt()
        ms
      case 7 => // 3FF
        registers(REG_SCRATCH_PAD)
  end readRegister

  def writeRegister(address:Int,value:Int): Unit =
    log.info("%s writing register %d = %d",componentName,address,value)
    address & 7 match
      case 0 => // 3F8
        if dlab then
          registers(REG_DIVISOR_LSB) = value & 0xFF
          updateBaud()
        else
          // clear INT Transmitter Holding Register Empty
          checkIRQ(disableMask = INT_TX_EMPTY)
          registers(REG_TRANSMITTER_BUFFER) = value & 0xFF
          registers(REG_LINE_STATUS) &= ~LINE_STATUS_TX_EMPTY
      case 1 => // 3F9
        if dlab then
          registers(REG_DIVISOR_MSB) = value & 0xFF
          updateBaud()
        else
          registers(REG_INTERRUPT_ENABLE) = value & 0x0F
          checkIRQ()
      case 2 => // 3FA
        // read-only
      case 3 => // 3FB
        registers(REG_LINE_CONTROL) = value & 0xFF
        // start bit + data bits + stop bits + parity bit
        byteBitLen = 1 + 5 + (registers(REG_LINE_CONTROL) & 3) + (registers(REG_LINE_CONTROL) & 4) + 1 + (if (registers(REG_LINE_CONTROL) & 8) != 0 then 1 else 0)
        log.info("%s Byte len = %d",componentName,byteBitLen)
      case 4 => // 3FC
        registers(REG_MODEM_CONTROL) = value & 0x1F
        setSignal(DTR,(value & 1) != 0)
        setSignal(RTS,(value & 2) != 0)
        //println(s"SERIAL IN ENABLED: ${(value & OUT2_INT_ENABLE) != 0}")
        log.info("%s LOOP enabled %b",componentName,loopMode)
      case 5 => // 3FD
        registers(REG_LINE_STATUS) = value & 0xBF | registers(REG_LINE_STATUS) & 0x40 // bit 6 is read-only
      case 6 => // 3FE
        registers(REG_MODEM_STATUS) = value & 0xFF
      case 7 => // 3FF
        registers(REG_SCRATCH_PAD) = value & 0xFF
  end writeRegister

  private def checkIRQ(disableMask:Int = -1,enableMask:Int = -1): Unit =
    if disableMask != -1 then
      activeInterrupts &= ~disableMask
    if enableMask != -1 then
      activeInterrupts |= enableMask

    val irqActive = (activeInterrupts & registers(REG_INTERRUPT_ENABLE)) != 0 && (registers(REG_MODEM_CONTROL) & OUT2_INT_ENABLE) != 0

    //println(s"SERIAL IRQ = $irqActive")
    irq(irqActive)
    lastIRQ = irqActive
  end checkIRQ
  private var lastIRQ = false

  private def checkLineStatusInterrupt(): Unit =
    if (registers(REG_LINE_STATUS) & (LINE_STATUS_OVERRUN|LINE_STATUS_PARITY_ERR)) != 0 then
      checkIRQ(enableMask = INT_MASK_REC_LINE_STATUS)
    else
      checkIRQ(disableMask = INT_MASK_REC_LINE_STATUS)
  end checkLineStatusInterrupt

  private def checkModemStatusInterrupt(): Unit =
    if registers(REG_MODEM_STATUS & 0xF) != 0 then
      checkIRQ(enableMask = INT_MASK_MODEM_STATUS)
    else
      checkIRQ(disableMask = INT_MASK_MODEM_STATUS)
  end checkModemStatusInterrupt

  private def updateBaud(): Unit =
    val divisor = registers(REG_DIVISOR_MSB) << 8 | registers(REG_DIVISOR_LSB)
    if divisor != 0 then
      val baudRate = SERIAL_CLOCK / (divisor << 4)
      bitCycles = divisor << 4
      bitCounterCycles = 0
      log.info("%s Baud rate = %d (divisor=%d) bitCycles=%d",componentName,baudRate,divisor,bitCycles)
  end updateBaud

  override def setRXByte(byte:Int): Unit =
    log.info("%s received byte from device %s = %02X",componentName,device.name,byte)
    //printf("%s received byte from device %s = %02X\n",componentName,device.name,byte)

    rxByteCount += 1

    if registers(REG_RECEIVER_BUFFER) != EMPTY_BUFFER then
      log.info("%s overrun condition",componentName)
      //printf("%s overrun condition lastIRQ=%b\n",componentName,lastIRQ)
      registers(REG_LINE_STATUS) |= LINE_STATUS_OVERRUN
      checkLineStatusInterrupt()
    else
      registers(REG_LINE_STATUS) |= LINE_STATUS_DATA_READY
      registers(REG_RECEIVER_BUFFER) = byte
      checkIRQ(enableMask = INT_MASK_REC_DATA)
  end setRXByte

  final def clock(): Unit =
    tickCycles += 1
    if tickCycles > internalTickCycles then
      tickCycles -= internalTickCycles
      tick()
  end clock

  private def tick(): Unit =
    if registers(REG_TRANSMITTER_BUFFER) != EMPTY_BUFFER && txShiftRegister == EMPTY_BUFFER then
      txShiftRegister = registers(REG_TRANSMITTER_BUFFER)
      registers(REG_TRANSMITTER_BUFFER) = EMPTY_BUFFER
      registers(REG_LINE_STATUS) |= LINE_STATUS_TX_EMPTY
      registers(REG_LINE_STATUS) &= ~LINE_STATUS_TX_SH_EMPTY
      checkIRQ(enableMask = INT_MASK_TX_EMPTY)
    end if

    bitCounterCycles += 1
    if bitCounterCycles >= bitCycles then
      bitCounterCycles = 0
      bitCount += 1
      if bitCount >= byteBitLen then
        bitCount = 0
        if txShiftRegister != EMPTY_BUFFER then
          if loopMode then
            log.info("%s byte sent to loop %s = %02X",componentName,device.name,txShiftRegister)
            setRXByte(txShiftRegister)
          else
            log.info("%s byte sent to device %s = %02X",componentName,device.name,txShiftRegister)
            device.setTXByte(txShiftRegister)
          txByteCount += 1
          txShiftRegister = EMPTY_BUFFER
          registers(REG_LINE_STATUS) |= LINE_STATUS_TX_SH_EMPTY

        device.checkRXByte()
  end tick



