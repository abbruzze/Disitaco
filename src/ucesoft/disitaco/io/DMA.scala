package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8237
import ucesoft.disitaco.cpu.Memory

/**
 * @author Alessandro Abbruzzetti
 *         Created on 25/03/2025 11:52  
 */
class DMA(mem:Memory) extends IODevice:
  override protected val componentName = "8237 DMA"
  final val dma = new i8237(mem)

  add(dma)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice((0x00 to 0x0F) ++ Seq(0x87,0x83,0x81,0x82),this)

  override final def in8(port: Int): Int =
    if port < 8 then
      val channel = port >> 1
      if (port & 1) == 0 then dma.readChannelAddress(channel) else dma.readWordCount(channel)
    else
      port match
        case 0x08 => dma.getStatusRegister
        case 0x0D => dma.readTemporaryRegister()
        case 0x87 => dma.getPageAddress(0)
        case 0x83 => dma.getPageAddress(1)
        case 0x81 => dma.getPageAddress(2)
        case 0x82 => dma.getPageAddress(3)
        case _ =>
          log.warning("DMA reading from unhandled port %d",port)
          0

  override final def out8(port: Int, value: Int): Unit =
    if port < 8 then
      val channel = port >> 1
      if (port & 1) == 0 then dma.writeChannelAddress(channel,value) else dma.writeWordCount(channel,value)
    else
      port match
        case 0x08 => dma.writeCommandRegister(value)
        case 0x09 => dma.writeRequestRegister(value)
        case 0x0A => dma.writeSetClearMaskRegister(value)
        case 0x0B => dma.writeModeRegister(value)
        case 0x0C => dma.clearAddressFlipFlop()
        case 0x0D => dma.masterClear()
        case 0x0E => dma.clearMaskRegister()
        case 0x0F => dma.writeMaskRegister(value)
        case 0x87 => dma.setPageAddress(0,value)
        case 0x83 => dma.setPageAddress(1,value)
        case 0x81 => dma.setPageAddress(2,value)
        case 0x82 => dma.setPageAddress(3,value)
        case _ =>
          log.warning("DMA writing to unhandled port %d",port)