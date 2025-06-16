package ucesoft.disitaco.storage

import java.io.File

/**
 * @author Alessandro Abbruzzetti
 *         Created on 16/06/2025 16:02  
 */
class LocalDirectoryFloppyDiskImage(directory:String) extends DiskImage:
  private val fat12DiskBuilder = new FAT12Disk1_44Builder(new File(directory))
  private val geometry = DiskImage.geo1440K

  private def sectorAbsolutePosition(track: Int, head: Int, sector: Int): Int =
    (track * geometry.heads + head) * geometry.sectorsPerTrack + sector - 1

  override def isReadOnly: Boolean = true
  override def diskName: String = directory
  override def diskGeometry: DiskImage.DiskGeometry = geometry
  override def readSector(track: Int, head: Int, sector: Int): Iterator[Byte] =
    val sec = fat12DiskBuilder.getSector(sectorAbsolutePosition(track,head,sector))
    sec.iterator
  override def writeSector(track: Int, head: Int, sector: Int, data: Array[Byte]): Unit = {}
  override def closeAndFlush(): Unit = {}
