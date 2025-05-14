package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8259

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/03/2025 15:58  
 */
class PIC extends IODevice:
  override protected val componentName = "8259 PIC"
  final val pic = new i8259

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(0x20 to 0x21,this)

  add(pic)

  override final def in8(port: Int): Int =
    port match
      case 0x20 => pic.read(0)
      case 0x21 => pic.read(1)
      case _ =>
        log.warning("PIC reading from unhandled port %d",port)
        0

  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x20 => pic.write(0,value)
      case 0x21 => pic.write(1,value)
      case _ =>
        log.warning("PIC writing to unhandled port %d",port)