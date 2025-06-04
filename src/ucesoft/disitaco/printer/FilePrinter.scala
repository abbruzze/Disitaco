package ucesoft.disitaco.printer

import ucesoft.disitaco.io.IOHandler

import java.io.{FileWriter, PrintWriter}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/06/2025 18:51  
 */
class FilePrinter(lptIndex:Int, portBase:Int,file:String) extends ParallelPort:
  override protected val componentName = s"Parallel#$lptIndex"

  private val printer = new PrintWriter(new FileWriter(file))
  private var data = 0
  private var selected = false
  private var control = 0

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(portBase to (portBase + 2),this)

  override def in8(port: Int): Int =
    port & 3 match
      case 0 => readData
      case 1 => readStatus
      case 2 => readControl

  override def out8(port: Int, byte: Int): Unit =
    port & 3 match
      case 0 => writeData(byte)
      case 1 => writeStatus(byte)
      case 2 => writeControl(byte)

  override def readData: Int = data
  override def writeData(byte: Int): Unit =
    data = byte
  override def readStatus: Int =
    var status = 0x80 | 0x8 | 0x4 // busy, error, IRQ
    if selected then status |= 0x10
    status
  override def writeStatus(status: Int): Unit =
    println(s"Write status: $status")
  override def readControl: Int = control
  override def writeControl(control: Int): Unit =
    selected = (control & 0x8) != 0
    if (this.control & 1) == 0 && (control & 1) == 1 then
      strobe()
    this.control = control

  private def strobe(): Unit =
    filter(data) match
      case Some(data) =>
        printer.print(data.toChar)
      case None =>

  private def filter(byte:Int): Option[Int] =
    byte match
      case 11 => None // VT
      case 7 => None  // Bell
      case 12 => None // Form Feed
      case _ => Some(byte)

