package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8253
/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/03/2025 15:58  
 */
class PIT extends IODevice:
  override protected val componentName = "8253 PIT"
  final val timer = new i8253

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(0x40 to 0x43,this)

  add(timer)

  override final def in8(port: Int): Int =
    port match
      case 0x40 => timer.readCounter(0)
      case 0x41 => timer.readCounter(1)
      case 0x42 => timer.readCounter(2)
      case _ =>
        log.warning("PIT reading from unhandled port %d",port)
        0

  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x40 => timer.writeCounter(0,value)
      case 0x41 => timer.writeCounter(1,value)
      case 0x42 => timer.writeCounter(2,value)
      case 0x43 => timer.writeControlWord(value)
      case _ =>
        log.warning("PIT writing to unhandled port %d",port)