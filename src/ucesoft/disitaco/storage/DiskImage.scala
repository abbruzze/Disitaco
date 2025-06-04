package ucesoft.disitaco.storage

import ucesoft.disitaco.storage.DiskImage.DiskGeometry

object DiskImage:
  inline val SECTOR_SIZE = 512

  case class DiskGeometry(tracks:Int,heads:Int,sectorsPerTrack:Int,label:String)

  final val FLOPPY_GEOMETRY_MAP = Map(
    160 * 1024 -> DiskGeometry(tracks = 40, heads = 1, sectorsPerTrack = 8,label = "160Ksd"),
    320 * 1024 -> DiskGeometry(tracks = 40, heads = 2, sectorsPerTrack = 8,label = "320Kdd"),
    180 * 1024 -> DiskGeometry(tracks = 40, heads = 1, sectorsPerTrack = 9,label = "180Ksd"),
    360 * 1024 -> DiskGeometry(tracks = 40, heads = 2, sectorsPerTrack = 9,label = "360Kdd"),
    720 * 1024 -> DiskGeometry(tracks = 80, heads = 2, sectorsPerTrack = 9,label = "720Kdd"),
    1200 * 1024 -> DiskGeometry(tracks = 80, heads = 2, sectorsPerTrack = 15,label = "1.2Mdd"),
    1440 * 1024 -> DiskGeometry(tracks = 80, heads = 2, sectorsPerTrack = 18,label = "1.4Mdd"),
    2880 * 1024 -> DiskGeometry(tracks = 80, heads = 2, sectorsPerTrack = 36,label = "2.8Mdd"),
  )

  final val HD_GEOMETRY_MAP = Map(
    10_404 * 1024 -> DiskGeometry(tracks = 306,heads = 4, sectorsPerTrack = 17, label = "10MHD"),
    20_910 * 1024 -> DiskGeometry(tracks = 615,heads = 4, sectorsPerTrack = 17, label = "20MHD"), // type 2
    20_808 * 1024 -> DiskGeometry(tracks = 612,heads = 4, sectorsPerTrack = 17, label = "20'MHD"),// type 16
  )

  final val geoHD10M : DiskGeometry = HD_GEOMETRY_MAP(10_404 * 1024)
  final val geoHD20M : DiskGeometry = HD_GEOMETRY_MAP(20_910 * 1024)
  final val geoHD20_M : DiskGeometry = HD_GEOMETRY_MAP(20_808 * 1024)

  final val geo160K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(160 * 1024)
  final val geo180K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(180 * 1024)
  final val geo320K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(320 * 1024)
  final val geo360K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(360 * 1024)
  final val geo720K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(720 * 1024)
  final val geo1200K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(1200 * 1024)
  final val geo1440K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(1440 * 1024)
  final val geo2880K: DiskGeometry  = FLOPPY_GEOMETRY_MAP(2880 * 1024)
  

end DiskImage

/**
 * @author Alessandro Abbruzzetti
 *         Created on 17/04/2025 16:21  
 */
trait DiskImage:
  def isReadOnly: Boolean
  def diskName: String
  def diskGeometry: DiskGeometry

  def readSector(track:Int,head:Int,sector:Int): Iterator[Byte]
  def writeSector(track:Int,head:Int,sector:Int,data:Array[Byte]): Unit

  def closeAndFlush(): Unit

