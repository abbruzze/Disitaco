package ucesoft.disitaco.io

import ucesoft.disitaco.chips.INS8250

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 15:07  
 */
class Serial(comIndex:Int,portBase:Int,masterClockFreq:Int,irq:Boolean => Unit) extends IODevice:
  override protected val componentName = s"Serial#$comIndex"
  final val ins8250 = new INS8250(comIndex,masterClockFreq,irq)
  
  var enabled = false

  add(ins8250)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(portBase to (portBase + 8),this)

  override def in8(port: Int): Int =
    if enabled then
      ins8250.readRegister(port & 7)
    else
      0xFF

  override def out8(port: Int, byte: Int): Unit =
    if enabled then
      ins8250.writeRegister(port & 7,byte)
