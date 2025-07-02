package ucesoft.disitaco.audio

import ucesoft.disitaco.{Clock, Config}
import ucesoft.disitaco.io.{IODevice, IOHandler}

import javax.sound.sampled.AudioFormat
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 02/07/2025 10:19  
 */
class AdLib(clock:Clock,irq0:Boolean => Unit) extends Audio(49700,"AdLib") with IODevice with Clock.Clockable:
  override protected val componentName = "AdLib"

  private inline val TIMER_80_MICROS_MILLIS = 80.0f / 1000
  private class Timer(var counter:Int = 0,var initialValue:Int = 0,var enabled:Boolean = false,var overflow:Boolean = false):
    def reset(): Unit =
      counter = 0
      initialValue = 0
      enabled = false
      overflow = false

  private val timers = Array(new Timer(),new Timer())
  private var timer2Tick = 0
  private var address = 0
  private var opl3 = new OPL3()
  private var tick80ID : Clock.EventID = uninitialized

  override protected def getPreferredVolume: Int = Config.getSAdLibVolume

  override protected def reset(): Unit =
    opl3 = new OPL3()
    for t <- timers do
      t.reset()
    timer2Tick = 0
    address = 0
    if tick80ID != null then
      tick80ID.cancel()
    tick80ID = clock.scheduleMillis(TIMER_80_MICROS_MILLIS,tickTimer80, isPeriodic = true)
    val cyclesPerSample = clock.getFrequency / 49700
    clock.schedule(cyclesPerSample, this, isPeriodic = true)
    log.info("AdLib initialized")

  override protected def getAudioFormat : AudioFormat = new AudioFormat(sampleRate.toFloat, 16, 1, true, false)

  private def tickTimer80(cycles:Long): Unit =
    if timers(0).enabled then
      timers(0).counter += 1
      if timers(0).counter == 0x100 then
        timers(0).counter = 0
        timers(0).overflow = true
        checkIRQ()

    timer2Tick += 1
    if timer2Tick == 4 then
      timer2Tick = 0
      if timers(1).enabled then
        timers(1).counter += 1
        if timers(1).counter == 0x100 then
          timers(1).counter = 0
          timers(1).overflow = true
          checkIRQ()
  end tickTimer80

  override final def clock(cycles: Long): Unit =
    val samples = opl3.read()
    var sum = 0
    var s = 0
    while s < 4 do
      sum += samples(s)
      s += 1
    if sum > 32767 then sum = 32767
    else if sum < -32768 then sum = -32768
    addSampleToBuffer(sum)

  /*
      7      6      5      4      3      2      1      0
    +------+------+------+------+------+------+------+------+
    | both | tmr  | tmr  |              unused              |
    | tmrs |  1   |  2   |                                  |
    +------+------+------+------+------+------+------+------+

    Bit 7 - set if either timer has expired.
        6 - set if timer 1 has expired.
        5 - set if timer 2 has expired.
  */
  private def getStatusRegister: Int =
    var status = 0
    if timers(0).overflow || timers(1).overflow then status |= 0x80
    if timers(0).overflow then status |= 0x40
    if timers(1).overflow then status |= 0x20
    status

  private def checkIRQ(): Unit =
    irq0(timers(0).overflow || timers(1).overflow)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(List(0x388,0x389),this)

  override def in8(port: Int): Int =
    port match
      case 0x388 =>
        getStatusRegister
      case _ =>
        0xFF
  override def out8(port: Int, byte: Int): Unit =
    port match
      case 0x388 =>
        address = byte
      case 0x389 =>
        address match
          case 2 =>
            timers(0).initialValue = byte & 0xFF
          case 3 =>
            timers(1).initialValue = byte & 0xFF
          case 4 =>
            /*
               7     6     5     4     3     2     1     0
             +-----+-----+-----+-----+-----+-----+-----+-----+
             | IRQ | T1  | T2  |     unused      | T2  | T1  |
             | RST | MSK | MSK |                 | CTL | CTL |
             +-----+-----+-----+-----+-----+-----+-----+-----+
              bit 7 - Resets the flags for timers 1 & 2.  If set,
                  all other bits are ignored.
              bit 6 - Masks Timer 1.  If set, bit 0 is ignored.
              bit 5 - Masks Timer 2.  If set, bit 1 is ignored.
              bit 1 - When clear, Timer 2 does not operate.
                      When set, the value from byte 03 is loaded into
                      Timer 2, and incrementation begins.
              bit 0 - When clear, Timer 1 does not operate.
                      When set, the value from byte 02 is loaded into
                      Timer 1, and incrementation begins.
             */
            if (byte & 0x80) != 0 then
              timers(0).overflow = false
              timers(1).overflow = false
              checkIRQ()
            else
              if (byte & 0x40) == 0 then
                if (byte & 1) == 0 then timers(0).enabled = false
                else
                  timers(0).enabled = true
                  timers(0).counter = timers(0).initialValue
              if (byte & 0x20) == 0 then
                if (byte & 2) == 0 then timers(1).enabled = false
                else
                  timers(1).enabled = true
                  timers(1).counter = timers(1).initialValue
          case _ =>
            opl3.write(0,address,byte)

