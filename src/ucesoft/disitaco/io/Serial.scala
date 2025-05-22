package ucesoft.disitaco.io

import ucesoft.disitaco.chips.INS8250

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 15:07  
 */
class Serial(portBase:Int,masterClockFreq:Int,irq:Boolean => Unit) extends IODevice:
  final val ins8250 = new INS8250(masterClockFreq,irq)

  add(ins8250)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(portBase to (portBase + 8),this)

  override def in8(port: Int): Int =
    ins8250.readRegister(port & 7)

  override def out8(port: Int, byte: Int): Unit =
    ins8250.writeRegister(port & 7,byte)
