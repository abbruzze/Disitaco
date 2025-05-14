package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8237
import ucesoft.disitaco.storage.XebecHDController

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/04/2025 16:10  
 */
class HardDiskFDC(dma:i8237, dmaChannel:Int, irq:Boolean => Unit,diskIDOffset:Int,numberOfHDDrives:Int) extends IODevice:
  override protected val componentName = "Xebec Controller"
  final val hdFdc = new XebecHDController(dma,dmaChannel,irq,diskIDOffset,numberOfDrives = numberOfHDDrives)

  add(hdFdc)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(Seq(0x320,0x321,0x322,0x323),this)

  override final def in8(port: Int): Int =
    port match
      case 0x320 => hdFdc.readData
      case 0x321 => hdFdc.readStatus
      case 0x322 => hdFdc.readDIPSwitches
      case _ =>
        log.warning("HD FDC reading from unhandled port %d",port)
        0

  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x320 => hdFdc.writeData(value)
      case 0x321 => hdFdc.controllerReset()
      case 0x322 => hdFdc.generateControllerSelectPulse(value)
      case 0x323 => hdFdc.writePatternDmaIrq(value)
      case _ =>
        log.warning("HD FDC writing to unhandled port %d",port)