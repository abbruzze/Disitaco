package ucesoft.disitaco

import ucesoft.disitaco.cpu.Registers
import ucesoft.disitaco.debugger.Debugger
import ucesoft.disitaco.storage.{DiskImage, Fast13IntHandler, FixedDiskImage, FloppyDiskImage}
import ucesoft.disitaco.ui.StoragePanel
import ucesoft.disitaco.{Display, Logger, MessageBus, Motherboard}

import java.awt.{BorderLayout, Dimension, FlowLayout}
import java.io.File
import java.nio.file.Paths
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/03/2025 19:36  
 */
object TestRomDis:
  def main(args:Array[String]): Unit =
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
    val rom/*glabios*/ = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\GLABIOS_0.2.5_8T.ROM"""))

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

    storagePanel.setDiskette(2,2,new StoragePanel.StorageListener:
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
          displayLabel.invalidate()
          if h > 100 then
            display.setPreferredSize(new Dimension(w,h * 2))
            frame.pack()
        })
    }

    val debugger = new Debugger(cpu,mother.videoCard,() => {},mother.videoCard,mother.dma.dma,mother.pit.timer,mother.keyboard,mother.fdc.fdc,mother.rtc.rtc,mother.speaker)
    val log = Logger.setLogger(debugger.log)
    mother.setLogger(log)

    mem.loadROM(rom)
    val glatick = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\GLaTICK_0.8.5_AT.ROM"""))
    mem.registerOptionROM(0xD0000,glatick,"Glatick")
    val harddisk = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\IBM_XEBEC_5000059_1982.BIN""")) // IBM_XEBEC_62X0822_1985.BIN / IBM_XEBEC_5000059_1982
    mem.registerOptionROM(0xD4000,harddisk,"harddisk")
    val hdDisk = new FixedDiskImage("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\disitaco\H_C.img""")
    val h1Disk = new FixedDiskImage("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\disitaco\H_D.img""")
    for d <- mother.hdc.hdFdc.getDrives do
      d.setListener(storagePanel)
    mother.hdc.hdFdc.getDrives(0).insertDisk(hdDisk)
    mother.hdc.hdFdc.getDrives(1).insertDisk(h1Disk)
//    val basic = java.nio.file.Files.readAllBytes(Paths.get("""G:\My Drive\Emulatori\x86\dos\basic_1.10.rom"""))
//    mem.registerOptionROM(0xF6000,basic,"Basic 1.10")

    mother.videoCard.setClippingOn(on = true)

    display.addKeyListener(mother.keyboard)
    display.setFocusable(true)

    mother.cpu.setInterruptHandler(0x13,new Fast13IntHandler(mother))

    mother.initComponent()
    debugger.setRAM(mem.getRAM)
    debugger.setROM(mem.getROM)

    SwingUtilities.invokeAndWait(() => debugger.enableTracing(true))

    mother.resetComponent()
    mother.clock.start()
    mother.clock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })
