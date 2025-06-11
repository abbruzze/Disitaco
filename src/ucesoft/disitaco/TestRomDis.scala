package ucesoft.disitaco

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.disitaco.debugger.Debugger
import ucesoft.disitaco.io.{LotechEMS, Turbo}
import ucesoft.disitaco.mouse.SerialMouse
import ucesoft.disitaco.serial.HostFileTransferSerialDevice
import ucesoft.disitaco.storage.{Fast13IntHandler, FixedDiskImage, FloppyDiskImage}
import ucesoft.disitaco.ui.StoragePanel
import ucesoft.disitaco.{Display, Logger, MessageBus, Motherboard}

import java.awt.{BorderLayout, Dimension, FlowLayout}
import java.io.File
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/03/2025 19:36  
 */
object TestRomDis:
  def main(args:Array[String]): Unit =

    if System.getProperty("swing.defaultlaf") == null then
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    Config.loadConfig()
/*
    val u19 = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\turboxtbios-u19.rom"""))
    val u18 = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\turboxtbios-u18.rom"""))
//    val u19 = java.nio.file.Files.readAllBytes(Paths.get("""C:\temp\86box\roms\machines\ibmxt86\BIOS_5160_10JAN86_U19_62X0854_27256_F000.BIN"""))
//    val u18 = java.nio.file.Files.readAllBytes(Paths.get("""C:\temp\86box\roms\machines\ibmxt86\BIOS_5160_10JAN86_U18_62X0851_27256_F800.BIN"""))
//    val u19 = java.nio.file.Files.readAllBytes(Paths.get("""C:\temp\86box\roms\machines\ibmxt\BIOS_5160_16AUG82_U19_5000027.BIN"""))
//    val u18 = java.nio.file.Files.readAllBytes(Paths.get("""C:\temp\86box\roms\machines\ibmxt\BIOS_5160_16AUG82_U18_5000026.BIN"""))
    if (u19.length % 8192) != 0 || (u18.length % 8192) != 0 then
      println("ROM's length is not a multiple of 8192")
      sys.exit(1)

    var rom = u19
    for _ <- 1 to (32768 - u19.length) / 8192 do
      rom = rom ++ u19
    rom = rom ++ u18
    for _ <- 1 to (32768 - u18.length) / 8192 do
      rom = rom ++ u18
*/
    val rom/*glabios*/ = Config.getBios

    val mother = new Motherboard
    val frame = new JFrame()
    val dim = mother.videoCard.getPreferredSize
    val display = new Display(dim.width, dim.height, "MDA Test", frame, mother.clock)
    display.setFocusable(true)
    display.setFocusTraversalKeysEnabled(false)
    display.grabFocus()
    val displayLabel = new JLabel("Waiting video display updating ...")
    val displayPanel = new JPanel(new BorderLayout())
    val infoPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val warpButton = new JToggleButton("Warp")
    warpButton.setFocusable(false)
    northPanel.add(warpButton)
    frame.getContentPane.add("North",northPanel)
    warpButton.addActionListener(_ => {
      MessageBus.send(MessageBus.WarpMode(this,warpButton.isSelected))
    })
    infoPanel.add(displayLabel)
    val storagePanel = new StoragePanel
    SwingUtilities.invokeAndWait(() => {
      infoPanel.add(storagePanel)
      displayPanel.add("Center", display)
      displayPanel.add("South", infoPanel)
      display.setPreferredSize(new Dimension(dim.width, dim.height))
      mother.display = display
      frame.getContentPane.add("Center", displayPanel)
      frame.pack()
      frame.setVisible(true)
    })

    val mem = mother.memory
    val cpu = mother.cpu

    storagePanel.setDiskette(2,Config.getHDImages.size,new StoragePanel.StorageListener:
      private var lastDir = new File("./")
      override def openImage(diskId: Int): Unit =
        val fc = new JFileChooser()
        fc.setCurrentDirectory(lastDir)
        fc.showOpenDialog(frame) match
          case JFileChooser.APPROVE_OPTION =>
            val image = new FloppyDiskImage(fc.getSelectedFile.toString)
            lastDir = fc.getSelectedFile.getParentFile
            mother.fdc.fdc.getDrives(diskId).insertDisk(image)
          case _ =>
      override def ejectImage(diskId: Int): Unit =
        mother.fdc.fdc.getDrives(diskId).ejectDisk()
    )
    for d <- mother.fdc.fdc.getDrives do
      d.setListener(storagePanel)

    // attach disk
//    val disk = new FloppyDiskImage("""G:\My Drive\Emulatori\x86\dos\msdos-3.3_01.img""")
//    val disk2 = new FloppyDiskImage("""G:\My Drive\Emulatori\x86\dos\checkit.img""")
//    mother.fdc.fdc.getDrives(0).insertDisk(disk)
//    mother.fdc.fdc.getDrives(1).insertDisk(disk2)

    MessageBus.add {
      case MessageBus.VideoModeChanged(_, mode, w, h) =>
        SwingUtilities.invokeLater(() => {
          displayLabel.setText(s"$mode $w x $h")
          val zoomX = mother.videoCard.getPreferredZoomX
          val zoomY = mother.videoCard.getPreferredZoomY
          displayLabel.invalidate()
          if h > 100 then
            display.setPreferredSize(new Dimension((w * zoomX).toInt,(h * zoomY).toInt))
            MessageBus.send(MessageBus.DisplaySizeChanged(this,zoomX,zoomY))
            frame.pack()
        })
      case _ =>
    }

    val debugger = new Debugger(cpu,mother.videoCard,() => {},mother.pic.pic,mother.videoCard,mother.dma.dma,mother.pit.timer,mother.keyboard,mother.fdc.fdc,mother.rtc.rtc,mother.speaker,mother.com1.ins8250,mother.com2.ins8250)
    val log = Logger.setLogger(debugger.log)
    mother.setLogger(log)

    mem.loadROM(rom)

    // option roms
    for or <- Config.getOptionRomList do
      mem.registerOptionROM(or.address,or.rom,or.label)
      log.info("Option ROM '%s' registered",or.label)

    // HDD
    if Config.isHDConfigured then
      for d <- mother.hdc.hdFdc.getDrives do
        d.setListener(storagePanel)

      for (hdd,i) <- Config.getHDImages.zipWithIndex do
        mother.hdc.hdFdc.getDrives(i).insertDisk(new FixedDiskImage(Config.getAbsolutePath(hdd)))

    // Floppy
    Config.getFloppyAImage.foreach(f => mother.fdc.fdc.getDrives(0).insertDisk(new FloppyDiskImage(Config.getAbsolutePath(f))))
    Config.getFloppyBImage.foreach(f => mother.fdc.fdc.getDrives(1).insertDisk(new FloppyDiskImage(Config.getAbsolutePath(f))))

//    val glatick = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\GLaTICK_0.8.5_AT.ROM"""))
//    mem.registerOptionROM(0xD2000,glatick,"Glatick")
//    val harddisk = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\IBM_XEBEC_5000059_1982.BIN""")) // IBM_XEBEC_62X0822_1985.BIN / IBM_XEBEC_5000059_1982
//    mem.registerOptionROM(0xD0000,harddisk,"harddisk")
//    val hdDisk = new FixedDiskImage("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\disitaco\H_C.img""")
//    val h1Disk = new FixedDiskImage("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\disitaco\H_D.img""")
//    for d <- mother.hdc.hdFdc.getDrives do
//      d.setListener(storagePanel)
//    mother.hdc.hdFdc.getDrives(0).insertDisk(hdDisk)
//    mother.hdc.hdFdc.getDrives(1).insertDisk(h1Disk)

    // EMS
    if Config.isEMSLotechEnabled then
      val ems = new LotechEMS(Config.getEMSLotechPort)
      mother.add(ems)
      ems.register(mother.ioHandler)
      mem.registerEMSHandler(Config.getEMSLotechAddressPage,ems)

    // Mouse
    if Config.isMouseEnabled then
      val mouse = new SerialMouse(display,logitech3Buttons = Config.isMouse3Buttons)
      mother.add(mouse)
      mouse.enable(enabled = true)
      mouse.setScaleXY(Config.getMouseScaleX,Config.getMouseScaleY)
      if Config.getMouseCOMPort == 1 then
        mother.com1.ins8250.setDevice(mouse)
      else
        mother.com2.ins8250.setDevice(mouse)

    // Turbo
    if Config.isTurboEnabled then
      val turbo = new Turbo(Config.getTurboPort)
      mother.add(turbo)
      turbo.register(mother.ioHandler)

    // Host ftp
    if Config.isHostFTPEnabled then
      val localftp = new HostFileTransferSerialDevice
      if Config.getHostFTPCom == 1 then
        mother.com1.ins8250.setDevice(localftp)
      else
        mother.com2.ins8250.setDevice(localftp)

    mother.videoCard.setClippingOn(on = true)

    display.addKeyListener(mother.keyboard)
    display.setFocusable(true)

    // Fast13
    if Config.isFastINT13Enabled then
      mother.cpu.setInterruptHandler(0x13,new Fast13IntHandler(mother))

    mother.initComponent()
    debugger.setRAM(mem.getRAM)
    debugger.setROM(mem.getROM)

    Logger.getLogger.setLevel(java.util.logging.Level.SEVERE)

    if Config.isDebuggerOpenAtStartup then
      SwingUtilities.invokeAndWait(() => debugger.enableTracing(true))

    mother.resetComponent()
    mother.clock.start()
    mother.clock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })


//    val tcpSerial = new TCPSerialDevice
//    mother.com2.ins8250.setDevice(tcpSerial)
//    
//    while true do
//      val host = scala.io.StdIn.readLine("Connect to host :")
//      val port = scala.io.StdIn.readLine("Connect to port :").toInt
//      tcpSerial.connect(host,port) match
//        case Some(e) =>
//          println(s"Error while connecting to $host:$port: $e")
//        case None =>
//          println(s"OK, connected")
