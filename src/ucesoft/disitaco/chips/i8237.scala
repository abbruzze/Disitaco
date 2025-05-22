package ucesoft.disitaco.chips

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.cpu.Memory

import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

object i8237:
  trait DMADevice:
    def dack(): Unit
    def dmaRead(): Int
    def dmaWrite(value:Int): Unit
    def tc(): Unit

  trait CPUDevice:
    def holdRequest(hold:Boolean,holdAck: () => Unit): Unit
/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/03/2025 11:31  
 */
class i8237(val mem:Memory) extends PCComponent:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/dma.png"))
  override val componentName = "8237"
  import i8237.*

  private enum State:
    case SI, S0, S1_4

  private class Channel(val id:Int):
    var dmaDevice : DMADevice = new DMADevice:
      override def dack(): Unit = {}
      override def dmaRead(): Int = 0
      override def dmaWrite(value: Int): Unit = {}
      override def tc(): Unit = {}

    var pageAddress = 0
    var currentAddressRegister = 0
    var baseAddressRegister = 0
    var currentWordCountRegister = 0
    var baseWordCountRegister = 0
    var modeRegister = 0

    var tc = id == 0//hack for Glabios 

    def getStatusTCIndexAndClear : Int =
      val st = if tc then 1 << id else 0
      tc = false
      st

    def reset(): Unit =
      modeRegister = 0

    private def decrementAndCheckTC: Boolean =
      currentWordCountRegister = (currentWordCountRegister - 1) & 0xFFFF
      val endOfCounting = currentWordCountRegister == 0xFFFF
      if endOfCounting then tc = true
      endOfCounting

    def releaseReq(): Unit = dreqs &= ~(1 << id)

    def EOP(): Unit =
      // Autoinitialize
      if (modeRegister & 0x10) != 0 then
        currentAddressRegister = baseAddressRegister
        currentWordCountRegister = baseWordCountRegister
      else
        maskRegister |= 1 << id

      releaseReq()
      // check priority
      if (commandRegister & 0x10) != 0 then // rotating priority
        priorityChannelIndex = (id + 3) & 3

      cpuDevice.holdRequest(hold = false,() => {})

    def busCycle(): Boolean =
      val addressIncrement = if (modeRegister & 0x20) == 0 then 1 else -1
      val address = pageAddress << 16 | currentAddressRegister

      (modeRegister >> 6) & 3 match
        case 0|1 => // Demand mode, Single mode
          (modeRegister >> 2) & 3 match
            case 0 => // Verify
              dmaDevice.dmaRead()
            case 1 => // Write
//              if id == 2 then
//                println(s"Writing a byte at address ${address.toHexString}")
              mem.writeByte(address,dmaDevice.dmaRead(),abs = true)
            case 2 => // Read
              dmaDevice.dmaWrite(mem.readByte(address,abs = true))
            case 3 => // Illegal
        case 2 => // Block mode
          (modeRegister >> 2) & 3 match
            case 0 => // Verify
            case 1 => // Write
              val byteToWrite = if id == 1 && (commandRegister & 1) == 1 then
                temporaryRegister // memory to memory
              else
                dmaDevice.dmaRead()

              mem.writeByte(address,byteToWrite,abs = true)
            case 2 => // Read
              val byteRead = mem.readByte(address,abs = true)
              if id == 0 && (commandRegister & 1) == 1 then // memory to memory
                temporaryRegister = byteRead
              else
                dmaDevice.dmaWrite(byteRead)
            case 3 => // Illegal
        case 3 => // Cascade mode
          // not supported
      end match
      currentAddressRegister = (currentAddressRegister + addressIncrement) & 0xFFFF
      val tc = decrementAndCheckTC
      if tc then  
        EOP()
        dmaDevice.tc()
      tc
    end busCycle
  end Channel

  import State.*

  private var cpuDevice: CPUDevice = (hold:Boolean,holdAck: () => Unit) => holdAck()
  private final val channels = Array(new Channel(0), new Channel(1), new Channel(2), new Channel(3))
  private var state = SI
  private var stateCounter = 0
  private var addressFlipFlop = false
  private var priorityChannelIndex = 0

  private var commandRegister = 0
  private var requestRegister = 0
  private var maskRegister = 0xF
  private var temporaryRegister = 0
  private var memToMemRW = false

  private var dmaON = false
  private var dmaChannel = 0

  private var dreqs = 0

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    val props = new ListBuffer[Property]
    props += Property("State",state.toString)
    props += Property("State counter",stateCounter.toString)
    props += Property("F/F",addressFlipFlop.toString)
    props += Property("Priority channel index",priorityChannelIndex.toString)
    props += Property("Command register",commandRegister.toString)
    props += Property("Request register",requestRegister.toString)
    props += Property("DRequest register",dreqs.toString)
    props += Property("Temp register",temporaryRegister.toString)
    props += Property("DMA on",dmaON.toString)
    props += Property("DMA channel",dmaChannel.toString)
    for c <- 0 to 3 do
      props += Property(s"CH #$c Page address",channels(c).pageAddress.toString)
      props += Property(s"CH #$c Current address", channels(c).currentAddressRegister.toString)
      props += Property(s"CH #$c Base address", channels(c).baseAddressRegister.toString)
      props += Property(s"CH #$c Current word count", channels(c).currentWordCountRegister.toString)
      props += Property(s"CH #$c Base word count", channels(c).baseWordCountRegister.toString)
      props += Property(s"CH #$c Mode", channels(c).modeRegister.toString)
      props += Property(s"CH #$c TC", channels(c).tc.toString)
    props.toList

  override protected def reset(): Unit =
    state = SI
    stateCounter = 0
    addressFlipFlop = false
    priorityChannelIndex = 0

    maskRegister = 0xF
    commandRegister = 0
    requestRegister = 0
    temporaryRegister = 0

    dmaON = false
    dreqs = 0

    channels.foreach(_.reset())


  final def setCPUDevice(cpuDevice: CPUDevice): Unit = this.cpuDevice = cpuDevice
  final def setPageAddress(channel:Int,pageAddress:Int): Unit =
    channels(channel).pageAddress = pageAddress
  final def getPageAddress(channel:Int): Int = channels(channel).pageAddress

  final def DREQ(active:Boolean,channel:Int,device:DMADevice): Unit =
    if active then
      dreqs |= 1 << channel
    else
      dreqs &= ~(1 << channel)
      if dmaON && ((channels(channel).modeRegister >> 6) & 3) == 0 then // demand mode
        dmaON = false

    channels(channel).dmaDevice = device

  final def externalEOP(): Unit =
    if dmaON then
      channels(dmaChannel).EOP()
      dmaON = false

  final def writeCommandRegister(cmd:Int): Unit =
    log.info("%s writing command reg = %d",componentName,cmd)
    commandRegister = cmd
  final def writeRequestRegister(value:Int): Unit =
    log.info("%s writing request reg = %d",componentName,value)
    val channel = value & 3
    val set = (value & 4) != 0

    if set then
      requestRegister |= 1 << channel
    else
      requestRegister &= ~(1 << channel)
  final def writeModeRegister(mode:Int): Unit =
    val channel = mode & 3
    log.info("%s [for channel %d] writing command reg = %d",componentName,channel,mode)
    channels(channel).modeRegister = mode
  final def writeSetClearMaskRegister(value:Int): Unit =
    log.info("%s writing set/clear mask reg = %d",componentName,value)
    val channel = value & 3
    val set = (value & 4) != 0

    if set then
      maskRegister |= 1 << channel
    else
      maskRegister &= ~(1 << channel)
  final def writeMaskRegister(value:Int): Unit =
    log.info("%s writing mask reg = %02X",componentName,value)
    maskRegister = value & 0xF
  final def clearMaskRegister(): Unit =
    log.info("%s writing clear mask",componentName)
    maskRegister = 0
  final def clearAddressFlipFlop(): Unit =
    log.info("%s writing clear f/f",componentName)
    addressFlipFlop = false
  final def masterClear(): Unit =
    log.info("%s writing master clear",componentName)
    reset()
  final def writeChannelAddress(channel:Int,address:Int): Unit =
    log.info("%s writing channel address [%d] (low=%b) reg = %02X",componentName,channel,!addressFlipFlop,address)
    if addressFlipFlop then
      channels(channel).baseAddressRegister = (channels(channel).baseAddressRegister & 0x00FF) | (address & 0xFF) << 8
      channels(channel).currentAddressRegister = (channels(channel).currentAddressRegister & 0x00FF) | (address & 0xFF) << 8
    else
      channels(channel).baseAddressRegister = (channels(channel).baseAddressRegister & 0xFF00) | (address & 0xFF)
      channels(channel).currentAddressRegister = (channels(channel).currentAddressRegister & 0xFF00) | (address & 0xFF)

    addressFlipFlop ^= true
  final def writeWordCount(channel:Int,count:Int): Unit =
    log.info("%s writing channel count [%d] (low=%b) reg = %02X",componentName,channel,!addressFlipFlop,count)
    if addressFlipFlop then
      channels(channel).baseWordCountRegister = (channels(channel).baseWordCountRegister & 0x00FF) | (count & 0xFF) << 8
      channels(channel).currentWordCountRegister = (channels(channel).currentWordCountRegister & 0x00FF) | (count & 0xFF) << 8
    else
      channels(channel).baseWordCountRegister = (channels(channel).baseWordCountRegister & 0xFF00) | (count & 0xFF)
      channels(channel).currentWordCountRegister = (channels(channel).currentWordCountRegister & 0xFF00) | (count & 0xFF)

    addressFlipFlop ^= true
  final def readChannelAddress(channel:Int): Int =
    val address = if addressFlipFlop then (channels(channel).currentAddressRegister >> 8) & 0xFF else channels(channel).currentAddressRegister & 0xFF
    addressFlipFlop ^= true
    address
  final def readWordCount(channel: Int): Int =
    val count = if addressFlipFlop then (channels(channel).currentWordCountRegister >> 8) & 0xFF else channels(channel).currentWordCountRegister & 0xFF
    addressFlipFlop ^= true
    count
  final def readTemporaryRegister(): Int = temporaryRegister
  final def getStatusRegister: Int =
    var st = 0
    var c = 0
    while c < 4 do
      st |= channels(c).getStatusTCIndexAndClear
      c += 1
    if dmaON then
      st |= dmaChannel << 4
    st

  // ====================================================================
  final def clock(): Unit =
    if (commandRegister & 4) == 4 then // disabled
      return
    if dmaON then
      doDMA()
      return

    // check DMA requests
    val requests = dreqs & ~maskRegister & 0xF
    var dmaChannelToProcess = -1
    if requests != 0 then
      dmaChannelToProcess = priorityChannelIndex
      while (requests & (1 << dmaChannelToProcess)) == 0 do
        dmaChannelToProcess = (dmaChannelToProcess + 1) & 3
    else if requestRegister != 0 then
      dmaChannelToProcess = priorityChannelIndex
      while (requestRegister & (1 << dmaChannelToProcess)) == 0 do
        dmaChannelToProcess = (dmaChannelToProcess + 1) & 3

    if dmaChannelToProcess != -1 then
      dmaChannel = dmaChannelToProcess
      dmaON = true
      state = SI
  end clock

  private def doDMA(): Unit =
    state match
      case SI =>
        // DMA ACK
        if (commandRegister & 1) != 1 then // no memory to memory
          channels(dmaChannel).dmaDevice.dack()

        cpuDevice.holdRequest(hold = true,() => {
          state = S1_4
          stateCounter = 0
          memToMemRW = false
        })
        state =  S0
      case S0 =>
        // Waiting CPU HLDA
      case S1_4 =>
        stateCounter += 1
        if stateCounter == 4 then // DMA data transfers take 4 clock (4.77 Mhz) cycles
          stateCounter = 0
          // check memory to memory
          if dmaChannel == 0 && (commandRegister & 1) == 1 then
            if !memToMemRW then
              channels(0).busCycle() // channel 0 read
            else
              if channels(1).busCycle() then // channel 1 write
                dmaON = false
            memToMemRW ^= true
          else
            if channels(dmaChannel).busCycle() then
              dmaON = false
            // check single mode
            else if ((channels(dmaChannel).modeRegister >> 6) & 3) == 1 then
              cpuDevice.holdRequest(hold = false, () => {}) // release bus but keep request active
              dmaON = false
              channels(dmaChannel).releaseReq()
  end doDMA
