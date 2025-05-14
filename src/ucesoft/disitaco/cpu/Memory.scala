package ucesoft.disitaco.cpu

/**
 * 
 * if abs (absolute address flag) is false address encoding is [segment word] << 16 | offset word
 * if abs is true address is an absolute address (0-F_FFFF)
 * 
 * @author Alessandro Abbruzzetti
 *         Created on 21/02/2025 13:48  
 */
trait Memory:
  final def physicalAddress(address:Int,incOfs:Boolean = false): Int =
    ((((address >>> 16) & 0xFFFF) << 4) + ((address + (if incOfs then 1 else 0)) & 0xFFFF)) & 0xF_FFFF
    
  def readByte(address:Int,abs:Boolean = false): Int
  def readWord(address:Int,abs:Boolean = false): Int

  def writeByte(address:Int,value:Int,abs:Boolean = false): Unit
  def writeWord(address:Int,value:Int): Unit
