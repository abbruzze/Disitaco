package ucesoft.disitaco.storage

import ucesoft.disitaco.storage.DiskDrive.DiskDriveListener
import ucesoft.disitaco.storage.DiskImage.DiskGeometry

import scala.compiletime.uninitialized

object DiskDrive:
  trait DiskDriveListener:
    def onMotor(id:Int,motorOn:Boolean): Unit
    def onPosition(id:Int,track:Int,head:Int,sector:Int): Unit
    def onDiskInserted(id:Int,image:DiskImage): Unit
    def onDiskEjected(id:Int,image:DiskImage): Unit
    def onModeChanged(id:Int,isWriting:Boolean): Unit
/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/04/2025 16:45  
 */
class DiskDrive(val id:Int,val geometry : DiskGeometry,FDC_CLOCK:Int,RPM:Int,isFixedDrive:Boolean = false,sectorSize:Int = DiskImage.SECTOR_SIZE):
  /* MFM typical track format
   *            00 .. 00 | A1 A1 A1 FE | tt | sd | ss | bs | CRC | 4E .. 4E | 00 .. 00 | A1 A1 A1 FB | SECTOR DATA | CRC | 4E .. 4E |
   *              Sync     Address      trk  side sect bls   crc    Intra       Sync     Data Mark     Sector bytes  crc     Inter
   *              field    mark         Address field             sector gap    field                                     sector gap
   *              (12)      (4)                (4)           (2)    (22)        (12)        (4)          (512)      (2)   (84/101)
   */
  private final val MFM_SECTOR_BITS = (12 + 4 + 4 + 2 + 22 + 12 + 4 + sectorSize + 2 + 100) * 8
  private final val MFM_TRACK_BITS = MFM_SECTOR_BITS * geometry.sectorsPerTrack
  private final val CYCLES_PER_BYTE = ((FDC_CLOCK / ((RPM / 60.0) * MFM_TRACK_BITS)) * 8).toInt
  // RPM / 60 = rotation per seconds
  // RPM / 60 * MFM_TRACK_BITS / FDC_CLOCK = bit / cycles
  // RPM    rot   MFM_TRACK_BITS      bits
  // --- =  --- = -------------- = -----------
  //  60    sec     FDC_CLOCK        cycles

  private var diskImage: DiskImage = uninitialized
  private var motorOn = false
  private var currentTrack = 0
  private var targetTrack = 0
  private var head = 0
  private var sector = if isFixedDrive then 0 else 1
  private var diskListener : DiskDriveListener = new DiskDriveListener:
    def onMotor(id: Int, motorOn: Boolean): Unit = {}
    def onPosition(id: Int, track: Int, head: Int, sector: Int): Unit = {}
    def onDiskInserted(id: Int, image: DiskImage): Unit = {}
    def onDiskEjected(id: Int, image: DiskImage): Unit = {}
    def onModeChanged(id: Int, isWriting: Boolean): Unit = {}

  private var irqReady = false
  private var irqSeek = false
  private var irqResult = false
  private var irqNonDMA = false
  private var equipmentError = false
  private var seekFailed = false
  private var rwFailed = false

  private var sectorByteCounter = 0

  private var cycleCounter = 0

  def getSectorByteCycles: Int = CYCLES_PER_BYTE

  def setListener(l:DiskDriveListener): Unit = diskListener = l
  def getListener: DiskDriveListener = diskListener

  def isIrqReady: Boolean =
    val irq = irqReady
    irqReady = false
    irq
  def isIrqSeek: Boolean =
    val irq = irqSeek
    irqSeek = false
    irq
  def isIrqResult: Boolean =
    val irq = irqResult
    irqResult = false
    irq
  def isIrqNonDMA: Boolean =
    val irq = irqNonDMA
    irqNonDMA = false
    irq
  def isEquipmentError: Boolean =
    val eq = equipmentError
    equipmentError = false
    eq
  def isSeekFailed: Boolean = seekFailed
  def isRWFailed: Boolean =
    val rwf = rwFailed
    rwFailed = false
    rwf
  def setRWFailed(): Unit = rwFailed = true

  def softwareReset(): Unit =
    irqReady = true
    irqSeek = false
    irqResult = false
    irqNonDMA = false
    equipmentError = false
    seekFailed = false
    rwFailed = false
  def setIrqResult(): Unit =
    irqResult = true
  def setIrqNonDMA(): Unit =
    irqNonDMA = true
  def setEquipmentError(): Unit =
    equipmentError = true
  
  def reset(): Unit =
    motorOn = false
    currentTrack = 0
    targetTrack = 0
    head = 0
    sector = if isFixedDrive then 0 else 1
    irqReady = false
    irqSeek = false
    irqResult = false
    irqNonDMA = false
    equipmentError = false
    seekFailed = false
    sectorByteCounter = 0
    cycleCounter = 0

  def insertDisk(image:DiskImage): Unit =
    if geometry != image.diskGeometry then
      if geometry.heads == image.diskGeometry.heads && geometry.sectorsPerTrack == image.diskGeometry.sectorsPerTrack && image.diskGeometry.tracks == geometry.tracks / 2 then
        println(s"Warning: diskette ${image.diskName} geometry compatible but not equal to drive's one")
      else
        throw new IllegalArgumentException(s"Bad disk geometry. Found ${image.diskGeometry}, expected $geometry")
    if diskImage != null then
      diskImage.closeAndFlush()
    diskImage = image
    diskListener.onDiskInserted(id,diskImage)
  def ejectDisk(): Unit =
    diskImage.closeAndFlush()
    diskListener.onDiskEjected(id,diskImage)
    diskImage = null

  def hasDiskInserted: Boolean = diskImage != null

  def isMotorOn: Boolean = motorOn
  def setMotorOn(on:Boolean): Unit =
    motorOn = on
    diskListener.onMotor(id,on)
    if on then
      sectorByteCounter = 0

  def getDiskInserted: Option[DiskImage] = Option(diskImage)

  def getCurrentTrack: Int = currentTrack
  def moveOnTrack(track:Int): Unit =
    targetTrack = track
    seekFailed = false
  def stepTrack(): Boolean =
    if currentTrack < targetTrack then
      if currentTrack + 1 == geometry.tracks then
        seekFailed = true
      else
        currentTrack += 1
        diskListener.onPosition(id,currentTrack,head,sector)
    else if currentTrack > targetTrack then
      currentTrack -= 1
      diskListener.onPosition(id, currentTrack, head, sector)

    if currentTrack == targetTrack then
      seekFailed = false
    val targetReached = currentTrack == targetTrack || seekFailed

    if targetReached then
      irqSeek = true
    targetReached
  def isPositioning: Boolean = currentTrack != targetTrack
  
  def setHead(h:Int): Unit =
    head = h
    diskListener.onPosition(id, currentTrack, head, sector)
  def getHead: Int = head
  
  def getSector: Int = sector
  
  private def nextSector(): Unit =
    if diskImage != null then
      sector += 1
      if isFixedDrive then
        if sector == geometry.sectorsPerTrack then
          sector = 0
      else
        if sector > geometry.sectorsPerTrack then
          sector = 1
      diskListener.onPosition(id, currentTrack, head, sector)

  final def rotate(): Unit =
    if motorOn && diskImage != null then
      if cycleCounter == 0 then
        cycleCounter = CYCLES_PER_BYTE
        sectorByteCounter += 1
        if sectorByteCounter >= sectorSize then
          sectorByteCounter = 0
          nextSector()
      else
        cycleCounter -= 1
      
  def readSector(): Iterator[Byte] =
    if diskImage == null then 
      Iterator.empty[Byte]
    else
      diskListener.onModeChanged(id,isWriting = false)
      rwFailed = false
      diskImage.readSector(currentTrack,head,sector)
  def readSector(track:Int,head:Int,sector:Int): Iterator[Byte] =
    if diskImage == null then
      Iterator.empty[Byte]
    else
      diskListener.onModeChanged(id, isWriting = false)
      rwFailed = false
      diskImage.readSector(track, head, sector)    
  def writeSector(track:Int,head:Int,sector:Int,data:Array[Byte]): Unit =
    if diskImage != null then
      diskListener.onModeChanged(id, isWriting = true)
      diskImage.writeSector(track, head, sector, data)
      diskListener.onPosition(id, track, head, sector)
  def writeSector(data:Array[Byte]): Unit =
    if diskImage != null then
      diskListener.onModeChanged(id,isWriting = true)
      diskImage.writeSector(currentTrack,head,sector,data)
  def formatTrackSector(head:Int,sector:Int,data:Array[Byte]): Unit =
    if diskImage != null then
      setHead(head)
      this.sector = sector
      writeSector(data)