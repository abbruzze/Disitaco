package ucesoft.disitaco.io

import ucesoft.disitaco.Clock
import ucesoft.disitaco.chips.MC146818

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/05/2025 17:30  
 */
class RTC(clock:Clock) extends IODevice:
  override protected val componentName = "RTC"
  
  final val rtc = new MC146818(clock)
  
  add(rtc)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(Seq(0x70,0x71),this)

  override final def in8(port: Int): Int =
    port match
      case 0x71 =>
        rtc.readDataRegister()
      case _ =>
        log.warning("%s reading from unhandled port %02X",port)
        0
  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x70 =>
        rtc.writeAddressRegister(value)
      case 0x71 =>
        rtc.writeDataRegister(value)
      case _ =>
        log.warning("%s writing to unhandled port %02X", port)  
