package ucesoft.disitaco

import ucesoft.disitaco.storage.DiskImage
import ucesoft.disitaco.video.{CGA, HDA, MDA, VideoCard}

import java.io.{File, FileNotFoundException, FileReader}
import java.util.Properties

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/03/2025 18:17  
 */
object Config:
  private inline val CONFIG_FILE = "disitaco.config"

  case class OptionROM(label:String, address:Int, rom:Array[Byte], refs:Option[String])

  private val homeDir = System.getProperty("disitaco.home","./")
  private val config = new Properties()
  private var optionRomList : List[OptionROM] = Nil

  private def romDir : File = new File(homeDir,"rom")
  private def configDir: File = new File(homeDir,"config")
  
  def printerDir: File = new File(homeDir,"printer")

  def loadConfig(): Unit =
    // check printer dir
    val pdir = printerDir
    if !pdir.exists() then
      pdir.mkdirs()
    val file = new File(configDir,CONFIG_FILE)
    if !file.exists() then
      throw new FileNotFoundException(s"Missing Disitaco configuration file: $file")

    val in = new FileReader(file)
    config.load(in)
    in.close()

    loadOptionRom()

    println("Configuration file loaded")
  end loadConfig

  private def loadOptionRom(): Unit =
    import scala.jdk.CollectionConverters.*
    val RE = """option\.rom\.([0-9]+)\.(name|address|path|refs)""".r
    val options = config.keys().asScala.map(_.toString).filter(RE.matches).toList
    val options2 = options.map(k => (k,config.getProperty(k))).map { (k,v) =>
      k match
        case RE(n, what) => (n.toInt,what,v)
    }
    val groups = options2.groupBy(_._1).toList.sortBy(_._1)
    optionRomList =
      for
        (_,list) <- groups
        (_,_,label) <- list.find(_._2 == "name")
        (_,_,address) <- list.find(_._2 == "address")
        (_,_,path) <- list.find(_._2 == "path")
        (_,_,file) <- Some(list.find(_._2 == "refs").getOrElse((0,"",null)))
      yield
          getHomeResource(path) match
            case Some(rom) =>
              OptionROM(label,Integer.parseInt(address,16),rom,Option(file))
            case None =>
              throw new FileNotFoundException(s"Missing option rom '$label' file: $path")
  end loadOptionRom

  def getClockFrequency: Int = config.getProperty("clock","4770000").toInt

  def getOptionRomList: List[OptionROM] = optionRomList

  def getMdaCgaCharROM: Array[Byte] =
    getHomeResource(config.getProperty("cga.char.rom","rom/IBM_5788005_AM9264_1981_CGA_MDA_CARD.BIN")) match
      case Some(rom) =>
        rom
      case None =>
        throw new FileNotFoundException("Missing MDA/CGA char rom file: rom/IBM_5788005_AM9264_1981_CGA_MDA_CARD.BIN")

  def getBios: Array[Byte] =
    val biosPath = config.getProperty("bios")
    if biosPath != null then
      getHomeResource(biosPath) match
        case Some(rom) =>
          rom
        case None =>
          throw new FileNotFoundException(s"Missing bios file: $biosPath")
    else
      val biosu18 = config.getProperty("bios.u18")
      val biosu19 = config.getProperty("bios.u19")
      if biosu18 == null || biosu19 == null then
        throw new IllegalArgumentException("Missing 'bios' property or 'bios.u18'/'bios.u19'")

      (getHomeResource(biosu18),getHomeResource(biosu19)) match
        case (Some(u18),Some(u19)) =>
          u19 ++ u18
        case _ =>
          throw new IllegalArgumentException("Missing 'bios' property or 'bios.u18'/'bios.u19'")
  end getBios

  def isEMSLotechEnabled: Boolean = config.getProperty("ems.lotech.enabled","false").toBoolean
  def getEMSLotechPort: Int = Integer.parseInt(config.getProperty("ems.lotech.port","260"),16)
  def getEMSLotechAddressPage: Int = Integer.parseInt(config.getProperty("ems.lotech.address","E0000"),16) >> 16

  def getFloppyAGeometry: DiskImage.DiskGeometry = getFloppyGeometry(a = true)
  def getFloppyBGeometry: DiskImage.DiskGeometry = getFloppyGeometry(a = false)
  private def getFloppyGeometry(a:Boolean): DiskImage.DiskGeometry =
    import DiskImage.*
    config.getProperty(if a then "floppy.a.geometry" else "floppy.b.geometry","720k").toUpperCase() match {
      case "160K" => geo160K
      case "180K" => geo180K
      case "320K" => geo320K
      case "360K" => geo360K
      case "720K" => geo720K
      case "1200K" => geo1200K
      case "1440K" => geo1440K
      case "2880K" => geo2880K
      case _ => geo720K
    }

  def isCGAAlternativeCharSet: Boolean = config.getProperty("cga.altCharSet","false").toBoolean
  def isCGACompositeMonitor: Boolean = config.getProperty("cga.composite","false").toBoolean
  def isCGA: Boolean = config.getProperty("video.card","cga") == "cga"

  def isHDConfigured: Boolean = optionRomList.exists(_.label.toUpperCase().startsWith("XEBEC"))

  def getHDImages: List[String] =
    optionRomList.find(_.label.toUpperCase().startsWith("XEBEC")) match
      case Some(rom) =>
        rom.refs match
          case Some(paths) =>
            paths.split(",").toList
          case None =>
            Nil
      case None =>
        Nil

  def getHDDriveCSizeInMb: Int = config.getProperty("hd.c.size","10").toInt
  def getHDDriveDSizeInMb: Int = config.getProperty("hd.d.size","10").toInt

  def getFloppyAImage: Option[String] = Option(config.getProperty("floppy.a.image"))
  def getFloppyBImage: Option[String] = Option(config.getProperty("floppy.b.image"))

  def isMouseEnabled: Boolean = config.getProperty("mouse.serial.enabled","false").toBoolean
  def isMouse3Buttons: Boolean = config.getProperty("mouse.serial.3buttons","false").toBoolean
  def getMouseCOMPort: Int = config.getProperty("mouse.serial.com","1").toInt
  def getMouseScaleX: Double = config.getProperty("mouse.serial.scale.x","1.0").toDouble
  def getMouseScaleY: Double = config.getProperty("mouse.serial.scale.y","1.0").toDouble
  
  def getVideoCard: VideoCard =
    config.getProperty("video.card","cga").toUpperCase() match
      case "MDA" => new MDA
      case "HDA" => new HDA
      case _ => new CGA
  
  def getMemoryInKBytes: Int = config.getProperty("memory","640").toInt
  
  def getCPUCorrectionFactor: Float = config.getProperty("cpu.correctionFactor","1.0").toFloat
  
  def getSpeakerSamplingFreq: Int = config.getProperty("speaker.samplingFreq","44100").toInt
  def getSpeakerBufferMillis: Int = config.getProperty("speaker.audioBufferMillis","5").toInt
  
  def isDebuggerOpenAtStartup: Boolean = config.getProperty("debugger.openAtStartup","false").toBoolean

  def isTurboEnabled: Boolean = config.getProperty("turbo.port") != null
  def getTurboPort: Int = config.getProperty("turbo.port","68").toInt

  def isHostFTPEnabled: Boolean = config.getProperty("hostftp.enabled","false").toBoolean
  def getHostFTPCom: Int = config.getProperty("hostftp.com","2").toInt

  def isFastINT13Enabled: Boolean = config.getProperty("fastInt13.enabled","false").toBoolean

  def isFloppyFlushingEnabled: Boolean = config.getProperty("floppy.flushing","false").toBoolean
    
  private def getHomeResource(path:String): Option[Array[Byte]] =
    val filePath = if path.startsWith("/") || path.charAt(1) == ':' then java.nio.file.Paths.get(path) else java.nio.file.Paths.get(homeDir,path)
    if java.nio.file.Files.exists(filePath) then
      Some(java.nio.file.Files.readAllBytes(filePath))
    else
      None

  def getAbsolutePath(f:String): String =
    if f.startsWith("/") || f.charAt(1) == ':' then f else java.nio.file.Paths.get(homeDir,f).toString
      
//  def getHomeResourceAsString(homeRelativePath:String): Option[String] = getHomeResource(homeRelativePath).map(new String(_,"UTF-8"))
//
//  def getResource(path:String): Option[Array[Byte]] =
//    val in = getClass.getResourceAsStream(path)
//    if in != null then
//      Some(in.readAllBytes())
//    else
//      None
//  def getResourceAsString(path:String): Option[String] = getResource(path).map(new String(_,"UTF-8"))
