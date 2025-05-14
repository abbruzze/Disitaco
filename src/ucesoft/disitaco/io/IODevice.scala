package ucesoft.disitaco.io

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.cpu.InOut

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/03/2025 16:02  
 */
trait IODevice extends PCComponent with InOut:
  def getName: String = componentName
  def register(ioHandler:IOHandler): Unit

  def in8(port:Int): Int
  def out8(port:Int,byte:Int): Unit

  override final def in(port: Int, size8: Boolean): Int =
    if size8 then
      in8(port)
    else
      (in8(port) & 0xFF) | ((in8(port + 1) & 0xFF) << 8)

  override final def out(port: Int, value: Int, size8: Boolean): Unit =
    if size8 then
      out8(port,value)
    else
      out8(port,value & 0xFF)
      out8(port + 1,(value >> 8) & 0xFF)
