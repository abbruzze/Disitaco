package ucesoft.disitaco

import ucesoft.disitaco.MessageBus.VideoModeChanged
import ucesoft.disitaco.chips.i8237.{CPUDevice, DMADevice}
import ucesoft.disitaco.chips.{i8253, i8255}
import ucesoft.disitaco.cpu.i8088
import ucesoft.disitaco.io.{DMA, FDC, HardDiskFDC, IODevice, IOHandler, PIC, PIT, PPI, RTC, Serial}
import ucesoft.disitaco.keyboard.Keyboard
import ucesoft.disitaco.speaker.Speaker
import ucesoft.disitaco.video.{CGA, HDA, MDA, VideoCard}

import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/03/2025 10:47  
 */
class Motherboard extends PCComponent with CPUDevice with VideoCard.VideoCardListener with Clock.Clockable:
  private inline val DEFAULT_CLOCK_FREQ = 4_770_000 // Mhz
  private inline val SPEAKER_AUDIO_RATE = 44_100 // 44.1 Khz
  private inline val SPEAKER_SAMPLE_CYCLES = DEFAULT_CLOCK_FREQ / SPEAKER_AUDIO_RATE

  final val floppyDrives = 2
  final val hardDisks = 2

  final val clock = new Clock("MasterClock",DEFAULT_CLOCK_FREQ)
  final val memory = new MMU(640)
  final val dma = new DMA(memory)
  final val pic = new PIC
  final val pit = new PIT
  final val ppi = new PPI
  final val ioHandler = new IOHandler
  final val cpu = new i8088(memory,ioHandler,pic.pic)
  final val keyboard = new Keyboard(clock,pic.pic.setIRQ(1,_)) // keyboard sends interrupt to line 1
  final val videoCard = new CGA
  final val fdc = new FDC(dma.dma,2,pic.pic.setIRQ(6,_)) // fdc sends interrupt to line 6
  final val hdc = new HardDiskFDC(dma.dma,3,pic.pic.setIRQ(5,_),diskIDOffset = 2,numberOfHDDrives = hardDisks) // hdc sends interrupt to line 5
  final val rtc = new RTC(clock)
  final val speaker = new Speaker(SPEAKER_AUDIO_RATE)
  final val com1 = new Serial(0x3F8,DEFAULT_CLOCK_FREQ,pic.pic.setIRQ(4,_))

  private final val nmiMaskDevice = new IODevice:
    override protected val componentName = "NMI Mask"
    private var latch = 0
    override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(0xA0,this)
    override def in8(port: Int): Int = latch
    override def out8(port: Int, value: Int): Unit =
      latch = value & 0xFF
      nmiEnabled = (value & 0x80) != 0
      log.info("NMI mask enabled: %b",nmiEnabled)

  private val ioDevices : Array[IODevice] = Array(dma,pic,pit,ppi,nmiMaskDevice,videoCard,fdc,rtc,hdc,com1)

  private var timer2OutValue = false
  private var speakerDataEnabled = false

  private var nmiEnabled = false

  private var clockFrequency = DEFAULT_CLOCK_FREQ
  private var videoCycles = 16f
  private var videoCycleCounter = 0f
  private var i8253Cycles = 4 // 4.77 / 4
  private var i8253CycleCounter = i8253Cycles
  private var lastVideoCharFrequency = 0.0
  private var speakerCycles = 0

  private var cpuClockToWait = 0

  private var busHoldingReq = false
  private var busHoldingReqWaitingAck = false
  private var busHoldingAck = () => {}

  private var _display : Display = uninitialized

  def display : Display = _display
  def display_=(display:Display): Unit =
    _display = display
    videoCard.setDisplay(_display)
    add(_display)

  final def changeClockFrequency(freq:Int): Unit =
    // TODO
    clockFrequency = freq
    i8253Cycles = 4
    videoCycles = (clockFrequency / lastVideoCharFrequency).toFloat

  // VideoCardListener
  final def modeChanged(mode: String,sw:Int,sh:Int): Unit =
    MessageBus.send(VideoModeChanged(this,mode,sw,sh))
    log.info("Video mode changed: %s %dx%d",mode,sw,sh)
  final def charFrequencyChanged(charFrequencyInHz: Double): Unit =
    videoCycles = (clockFrequency / charFrequencyInHz).toFloat
    lastVideoCharFrequency = charFrequencyInHz
    log.info("Video char frequency changed: %f [cycles=%f]",lastVideoCharFrequency,videoCycles)

  override protected def reset(): Unit =
    timer2OutValue = false
    speakerDataEnabled = false
    pit.timer.setGate(0, true)
    pit.timer.setGate(1, true)

  /*
    |Bit           | Description
    ================================================
    | 0            | 0 = Normal POST then boot, 1 = Continuous power-on self-test (POST) - used for burn-in testing only
    | 1            | 1 = 8087 math coprocessor NOT installed
    | 3-2          | Banks of memory installed: two system boards 64/256K, 256/640K
    |              | 00 = 64K/256K
    |              | 01 = 128K/512K
    |              | 10 = 192K/576K
    |              | 11 = 256K/640K
    | 5-4          | 00 = No video or special (like EGA or VGA)
    |              | 01 = 40-column CGA video
    |              | 10 = 80-column CGA video
    |              | 11 = MDA or Hercules video
    | 7-6          | Floppies: 00 = 1 floppy, 01 = 2, 10 = 3, 11 = 4
    ================================================
  */
  final val SW1 = new DipSwitches(1,1 << 1 | 3 << 2 | videoCard.getCardInfo.dipSwitch54 << 4 | 1 << 6 | 1)

  add(clock)
  add(memory)
  add(dma)
  add(pic)
  add(pit)
  add(ppi)
  add(cpu)
  add(ioHandler)
  add(keyboard)
  add(fdc)
  add(hdc)
  add(rtc)
  add(speaker)
  add(com1)
  ioDevices.foreach(add)

  override protected def init(): Unit =
    wiring()
    speaker.setBufferInMillis(5)
    speaker.start()
    speaker.turn(on = true)
  end init

  private def wiring(): Unit =
    log.info("Wiring components ...")
    // 8253 ports ======================================================
    val timer2Out = new i8253.CounterOutListener:
      override def outChanged(value: Boolean): Unit =
        timer2OutValue = value
    log.info("Creating 8255's ports ...")
    // 8255 ports ======================================================
    val portB = new i8255.Port:
      private var latch = 0 // used to get back bit 3 for Port C
      override def read: Int = latch
      /*
       bit 7 = 1  clear keyboard (Clear KSR  +  Clear IRQ1)
       bit 6 = 0  hold keyboard clock low
       bit 5 = 0  I/O check enable
       bit 4 = 0  RAM parity check enable
       bit 3 = 0  0 = Present switches 1 to 4 in SW1 to pins PC0 through PC3, 1 = Present switches 5 to 8 in SW1 to pins PC0 through PC3
       bit 2	    reserved, often used as turbo switch
       bit 1 = 1  speaker data enable
       bit 0 = 1  timer 2 gate to speaker enable
      */
      override def write(value: Int): Unit =
        latch = value
        keyboard.clear((value & 0x80) == 0x80)
        keyboard.clockLine((value & 0x40) == 0)

        speakerDataEnabled = (value & 2) != 0

        pit.timer.setGate(2,(value & 1) != 0)
    val portC = new i8255.Port:
      /*
       bit 7 = 1  RAM parity error occured on motherboard
       bit 6 = 0  RAM parity error occured on an expansion card
       bit 5 = 0  Monitor of timer (8253) channel 2
       bit 4 = 0  Monitor of speaker
       bit 3-0    Pin PB3 controls whether these 4 pins are either:
                  1.  Switches 5 to 8 in SW1; or
                  2.  Switches 1 to 4 in SW1
       */
      override def read: Int =
        val sw1 = if (portB.read & 8) == 0 then SW1.switches & 0xF else SW1.switches >> 4
        sw1 | (if timer2OutValue then 0x20 else 0) | (if timer2OutValue && speakerDataEnabled then 0x10 else 0)
      override def write(value: Int): Unit = {}
    val portA = new i8255.Port:
      override def read: Int =
        keyboard.readScanCode
      override def write(value: Int): Unit =
        log.warning("Unexpected write to 8255 port A: %02X",value)

    // set 8255 ports
    ppi.ppi.setPort(0,portA)
    ppi.ppi.setPort(1,portB)
    ppi.ppi.setPort(2,portC)

    log.info("Setting 8253 timer ports ...")
    // set 8253 ports
    pit.timer.setCounterListener(2,timer2Out) // speaker
    pit.timer.setCounterListener(0,v => pic.pic.setIRQ(0,v)) // IRQ0
    // RAM refresh
    val ramRefreshDMADevice = new DMADevice:
      override def dack(): Unit = {}
      override def dmaRead(): Int = 0
      override def dmaWrite(value: Int): Unit = {}//log.info("Refreshing RAM ...")
      override def tc(): Unit = {}

    pit.timer.setCounterListener(1,v => dma.dma.DREQ(v,0,ramRefreshDMADevice)) // RAM refresh

    pic.pic.setIntHandler(int => cpu.raiseINTR(int))

    // DMA
    dma.dma.setCPUDevice(this)

    // I/O
    ioDevices.foreach { d =>
      log.info("Registering I/O device %s",d.getName)
      d.register(ioHandler)
    }
    // MMU
    memory.setVideoCard(videoCard)
    // Clock
    clock.setClockable(this)
    // Video card
    videoCard.setModeListener(this)
  end wiring

  override final def holdRequest(hold:Boolean,holdAck: () => Unit): Unit =
    busHoldingReq = hold
    busHoldingAck = holdAck
    busHoldingReqWaitingAck = true
  // ========================= Main Loop ==========================================
  override final def clock(cycles: Long): Unit =
    // cpu
    if cpuClockToWait > 0 then // consume cpu cycles of last instruction
      cpuClockToWait -= 1
    else if busHoldingReq then // pending bus request from DMA
      if busHoldingReqWaitingAck then // sending ack
        busHoldingReqWaitingAck = false
        busHoldingAck()
    else
      cpuClockToWait = cpu.execute() - 1
    // i8253: 4.77Mhz / 4
    i8253CycleCounter -= 1
    if i8253CycleCounter == 0 then
      i8253CycleCounter = i8253Cycles
      pit.timer.clock()
      speaker.addSample(timer2OutValue && speakerDataEnabled)
    // DMA: same clock of 8088
    dma.dma.clock()
    // video card
    videoCycleCounter += 1
    if videoCycleCounter >= videoCycles then
      videoCycleCounter -= videoCycles
      videoCard.clockChar()
    // FDC
    fdc.fdc.clock(_cycles = 2)
    // HDC
    hdc.hdFdc.clock(_cycles = 2)
    // Speaker
    speakerCycles += 1
    if speakerCycles >= SPEAKER_SAMPLE_CYCLES then
      speakerCycles = 0
      //println(s"SPEAKER OUT is timer2OutValue=$timer2OutValue speakerDataEnabled=$speakerDataEnabled")
      speaker.setOut()
    // serial
    com1.ins8250.clock()  
  end clock




