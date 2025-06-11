package ucesoft.disitaco.storage

import ucesoft.disitaco.Config

import java.io.{File, IOException}
import java.nio.file.{Paths, StandardOpenOption}
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 17/04/2025 16:35  
 */
class FloppyDiskImage(fileName:String,_readOnly:Boolean = false) extends DiskImage:
  import DiskImage.*
  private var readOnly = _readOnly
  private var geometry : DiskGeometry = uninitialized
  private var image : Array[Byte] = Array()
  private var modified = false

  private class SectorIterator(track:Int,head:Int,sector:Int) extends Iterator[Byte]:
    private val offset = sectorAbsolutePosition(track,head,sector)
    private var sectorOffset = 0
    override final def hasNext: Boolean = sectorOffset < SECTOR_SIZE
    override final def next(): Byte =
      val b = image(offset + sectorOffset)
      sectorOffset += 1
      b

  loadImage()

  // track number starts from 0, sector number starts from 1
  private def sectorAbsolutePosition(track:Int,head:Int,sector:Int): Int =
    ((track * geometry.heads + head) * geometry.sectorsPerTrack + sector - 1) * SECTOR_SIZE

  private def loadImage(): Unit =
    val file = new File(fileName)
    FLOPPY_GEOMETRY_MAP.get(file.length().toInt) match
      case None =>
        throw new IllegalArgumentException(s"Image '$fileName' has an unrecognized size ${file.length()}")
      case Some(geo) =>
        geometry = geo
        image = java.nio.file.Files.readAllBytes(file.toPath)
        readOnly |= !file.canWrite

  override def isReadOnly: Boolean = readOnly
  override def diskName: String = fileName
  override def diskGeometry: DiskImage.DiskGeometry = geometry
  override def readSector(track: Int, head: Int, sector: Int): Iterator[Byte] = new SectorIterator(track,head,sector)
  override def writeSector(track:Int,head:Int,sector:Int,data:Array[Byte]): Unit =
    modified = true
    val offset = sectorAbsolutePosition(track,head,sector)
    System.arraycopy(data,0,image,offset,data.length)
  override def closeAndFlush(): Unit =
    if !Config.isFloppyFlushingEnabled || !modified then return

    try
      java.nio.file.Files.write(Paths.get(fileName),image,StandardOpenOption.WRITE)
    catch
      case t:IOException =>
        println(s"Cannot write back image $fileName: " + t)

