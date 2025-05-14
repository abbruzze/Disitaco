package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8237
import ucesoft.disitaco.storage.i8272A

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/04/2025 16:10  
 */
class FDC(dma:i8237,dmaChannel:Int,irq:Boolean => Unit) extends IODevice:
  override protected val componentName = "FDC"
  final val fdc = new i8272A(dma,dmaChannel,irq)

  add(fdc)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(Seq(0x3F2,0x3F4,0x3F5),this)

  override final def in8(port: Int): Int =
    port match
      case 0x3F4 => fdc.readMainStatusRegister
      case 0x3F5 => fdc.readCommandDataRegister
      case _ =>
        log.warning("FDC reading from unhandled port %d",port)
        0

  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x3F2 => fdc.writeDigitalOutputRegister(value)
      case 0x3F5 => fdc.writeCommandDataRegister(value)
      case _ =>
        log.warning("FDC writing to unhandled port %d",port)