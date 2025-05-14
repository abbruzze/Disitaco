package ucesoft.disitaco

import ucesoft.disitaco.cpu.Memory
import ucesoft.disitaco.video.VideoCard

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/03/2025 18:55  
 */
class MMU(val memorySizeInK:Int) extends PCComponent with Memory:
  override val componentName = "MMU"

  private val mem = Array.ofDim[Byte](memorySizeInK * 1024)
  private val rom = Array.ofDim[Byte](0x10000)
  private var videoMem : Array[Byte] = Array()
  private var videoMemAddress = 0
  private var videoMemEndAddress = 0

  private val optionRoms = Array.ofDim[Byte](128 * 1024)
  
  final def getRAM: Array[Byte] = mem
  final def getROM: Array[Byte] = rom

  final def setVideoCard(video:VideoCard): Unit =
    val info = video.getCardInfo
    videoMem = info.ram
    videoMemAddress = info.mainMemoryOffset & 0xFFFF
    videoMemEndAddress = videoMemAddress + videoMem.length

  def loadROM(rom:Array[Byte]): Unit =
    val startAddress = 0x10000 - rom.length
    log.info(s"Loading rom with size %d bytes at F%04X",rom.length,startAddress)
    System.arraycopy(rom,0,this.rom,startAddress,rom.length)

  def registerOptionROM(address:Int,rom:Array[Byte],label:String = ""): Unit =
    val bank = address >> 16
    if bank != 0xC && bank != 0xD && bank != 0xF then
      println(s"Cannot load option rom at address ${address.toHexString}")
      log.error("Option ROM %s cannot be loaded at address %05X".format(label, address))
      return

    if bank == 0xF then
      if rom.length > 0xFFFF then
        log.error("Option ROM %s cannot be loaded at address %05X".format(label, address))
      else
        System.arraycopy(rom,0,this.rom,address & 0xFFFF,rom.length)
        log.info("Option ROM %s loaded at address %05X".format(label,address))
    else
      val offset = ((bank - 0xC) * 0x10000) | address & 0xFFFF
      if offset + rom.length < optionRoms.length then
        System.arraycopy(rom,0,optionRoms,offset,rom.length)
        log.info("Option ROM %s loaded at address %05X".format(label,address))
      else
        log.error("Option ROM %s cannot be loaded at address %05X".format(label, address))
  end registerOptionROM

  override final def readByte(_address: Int, abs: Boolean): Int =
    val address = if abs then _address else physicalAddress(_address)
    val block = address >> 16
    if block < 0xA then
      if address < mem.length then
        mem(address) & 0xFF
      else
        0xFF
    else block match
      case 0xB =>
        val vaddress = address & 0xFFFF
        if vaddress >= videoMemAddress && vaddress < videoMemEndAddress then
          videoMem(vaddress - videoMemAddress) & 0xFF
        else
          0xFF
      case 0xC =>
        optionRoms(address & 0xFFFF) & 0xFF
      case 0xD =>
        optionRoms(address & 0xFFFF | 0x10000) & 0xFF
      case 0xF =>
        //rom(address & romMask) & 0xFF
        rom(address & 0xFFFF) & 0xFF
      case _ =>
        0xFF

  override final def readWord(address: Int, abs: Boolean): Int =
    if abs then
      readByte(address + 1,abs) << 8 | readByte(address,abs)
    else
      readByte(physicalAddress(address, incOfs = true),abs = true) << 8 | readByte(physicalAddress(address), abs = true)

  override final def writeByte(_address: Int, value: Int, abs: Boolean): Unit =
    val address = if abs then _address else physicalAddress(_address)
    val block = address >> 16
    if block < 0xA then
      if address < mem.length then
        mem(address) = value.asInstanceOf[Byte]
    else block match
      case 0xB =>
        val vaddress = address & 0xFFFF
        if vaddress >= videoMemAddress && vaddress < videoMemEndAddress then
          videoMem(vaddress - videoMemAddress) = value.asInstanceOf[Byte]
      case _ =>
        log.warning("Writing to an unhandled address: %05X = %02X",address,value)

  override final def writeWord(address: Int, value: Int): Unit =
    writeByte(physicalAddress(address),value.asInstanceOf[Byte],abs = true)
    writeByte(physicalAddress(address,incOfs = true),(value >> 8).asInstanceOf[Byte], abs = true)
