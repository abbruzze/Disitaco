package ucesoft.disitaco.printer

import ucesoft.disitaco.io.IODevice

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/06/2025 18:43  
 */
trait ParallelPort extends IODevice:
  def readData: Int
  def writeData(byte:Int): Unit
  def readStatus: Int
  def writeStatus(status:Int): Unit
  def readControl: Int
  def writeControl(control:Int): Unit
