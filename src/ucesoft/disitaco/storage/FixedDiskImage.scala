package ucesoft.disitaco.storage

import ucesoft.disitaco.storage.DiskImage.DiskGeometry

import java.io.File
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, StandardOpenOption}
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 09/05/2025 10:23  
 */
class FixedDiskImage(fileName:String,_readOnly:Boolean = false) extends DiskImage:
  import DiskImage.*
  private var readOnly = _readOnly
  private var geometry: DiskGeometry = uninitialized
  private var image: MappedByteBuffer = uninitialized
  private var modified = false

  loadImage()

  private class SectorIterator(track: Int, head: Int, sector: Int) extends Iterator[Byte]:
    private val data = getData
    private var sectorOffset = 0

    private def getData: Array[Byte] =
      val offset = sectorAbsolutePosition(track, head, sector)
      val data = Array.ofDim[Byte](SECTOR_SIZE)
      image.get(offset,data,0,data.length)
      data

    override final def hasNext: Boolean = sectorOffset < SECTOR_SIZE
    override final def next(): Byte =
      val b = data(sectorOffset)
      sectorOffset += 1
      b

  // track number starts from 0, sector number starts from 0
  private def sectorAbsolutePosition(track: Int, head: Int, sector: Int): Int =
    ((track * geometry.heads + head) * geometry.sectorsPerTrack + sector) * SECTOR_SIZE

  private def loadImage(): Unit =
    val file = new File(fileName)
    HD_GEOMETRY_MAP.get(file.length().toInt) match
      case None =>
        throw new IllegalArgumentException(s"Image '$fileName' has an unrecognized size ${file.length()}")
      case Some(geo) =>
        geometry = geo
        val channel = Files.newByteChannel(file.toPath, StandardOpenOption.READ, StandardOpenOption.WRITE).asInstanceOf[FileChannel]
        image = channel.map(FileChannel.MapMode.READ_WRITE, 0, channel.size())
        readOnly |= !file.canWrite

  override def isReadOnly: Boolean = readOnly
  override def diskName: String = fileName
  override def diskGeometry: DiskGeometry = geometry
  override def readSector(track: Int, head: Int, sector: Int): Iterator[Byte] = new SectorIterator(track,head,sector)
  override def writeSector(track: Int, head: Int, sector: Int, data: Array[Byte]): Unit =
    modified = true
    val offset = sectorAbsolutePosition(track, head, sector)
    image.put(offset,data,0,data.length)

  override def closeAndFlush(): Unit =
    println(s"Flushing harddisk $fileName")
    image.force()
