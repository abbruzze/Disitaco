package ucesoft.disitaco.io

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.cpu.InOut

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/03/2025 14:30  
 */
class IOHandler extends PCComponent with InOut:
  override protected val componentName = "IOHandler"
  private final val devices = Array.ofDim[InOut](65536)

  def registerDevice(port:Int,device:InOut): Unit = devices(port) = device
  def registerDevice(range: Seq[Int],device:InOut): Unit = range.foreach(devices(_) = device)

  override final def in(port: Int, size8: Boolean): Int =
    val device = devices(port)
    if device != null then
      device.in(port,size8)
    else
      log.warning("[IN]No device registered for port %04X",port)
      0xFF
  override final def out(port: Int, value: Int, size8: Boolean): Unit =
    val device = devices(port)
    if device != null then
      device.out(port,value,size8)
    else
      log.warning("[OUT]No device registered for port %04X",port)
