package ucesoft.disitaco.io

import ucesoft.disitaco.cpu.Memory

/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/05/2025 18:04  
 */
class LotechEMS(emsPort:Int) extends IODevice with Memory:
  private val addressRegisters = Array.ofDim[Int](4)
  private val mem = Array.fill[Byte](2 * 1024 * 1024)(0xAA.toByte)
  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(List(emsPort,emsPort + 1,emsPort + 2,emsPort + 3),this)

  override def in8(port: Int): Int = 0xFF
  override def out8(port: Int, byte: Int): Unit =
    val reg = port & 3
    addressRegisters(reg) = (byte & 0x7F) << 14

  override def readByte(_address: Int, abs: Boolean): Int =
    val address = if abs then _address else physicalAddress(_address)
    val page = (address & 0b1100_0000_0000_0000) >> 14
    val emsAddress = addressRegisters(page) + (address & 0b0011_1111_1111_1111)
    mem(emsAddress) & 0xFF
  override def writeByte(_address: Int, value: Int, abs: Boolean): Unit =
    val address = if abs then _address else physicalAddress(_address)
    val page = (address & 0b1100_0000_0000_0000) >> 14
    val emsAddress = addressRegisters(page) + (address & 0b0011_1111_1111_1111)
    mem(emsAddress) = value.toByte

  override def readWord(address: Int, abs: Boolean): Int = 0
  override def writeWord(address: Int, value: Int): Unit = {}

