package ucesoft.disitaco.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.disitaco.debugger.Debugger
import ucesoft.disitaco.io.{LotechEMS, Turbo}
import ucesoft.disitaco.mouse.SerialMouse
import ucesoft.disitaco.serial.HostFileTransferSerialDevice
import ucesoft.disitaco.storage.{Fast13IntHandler, FixedDiskImage, FloppyDiskImage, LocalDirectoryFloppyDiskImage}
import ucesoft.disitaco.*

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout}
import java.io.File
import javax.swing.*
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

object DisitacoUI:
  def main(args:Array[String]): Unit =
    try
      println("Loading configuration file ...")
      Config.loadConfig()
    catch
      case t:Throwable =>
        println(s"Error while loading configuration file: ${t.getMessage}")
        sys.exit(1)

    val preBuildLogs = new ListBuffer[String]
    Logger.setLogger(msg => preBuildLogs += msg)

    if System.getProperty("swing.defaultlaf") == null then
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    try
      val disitaco = new DisitacoUI
      SwingUtilities.invokeAndWait(() => disitaco.setGUI())
      preBuildLogs.toList.foreach(Logger.getLogger.addLog)
      disitaco.boot()
    catch
      case t:Throwable =>
        t.printStackTrace()
        sys.exit(1)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/06/2025 14:26  
 */
class DisitacoUI extends MessageBus.MessageListener with StoragePanel.StorageListener:
  private val debugMenuItem = new JCheckBoxMenuItem("Debugger")
  private val fastINT13MenuItem = new JCheckBoxMenuItem("Fast INT13")
  private val serialDialogItem = new JCheckBoxMenuItem("Serial dialog")
  private val warpItem = new JCheckBoxMenuItem("Warp mode")
  private val mouseCapItem = new JCheckBoxMenuItem("Mouse capture")
  private val turboButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/turbo.png")))

  private val mother = new Motherboard
  private val frame = new JFrame()
  private val serialDialog = new SerialsDialog(frame,() => serialDialogItem.setSelected(false))
  private var display : Display = uninitialized
  private val storagePanel = new StoragePanel
  private val debugger = new Debugger(mother.cpu,mother.videoCard,() => debugMenuItem.setSelected(false),mother.pic.pic,mother.videoCard,mother.dma.dma,mother.pit.timer,mother.keyboard,mother.fdc.fdc,mother.rtc.rtc,mother.speaker,mother.com1.ins8250,mother.com2.ins8250)
  private var lastDir = new File("./")
  private val displayLabel = new JLabel("Waiting video display updating ...")
  private val videoCardInfoPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private var enableDisplayResizing = true

  private var mouse : SerialMouse = uninitialized

  override def openImage(diskId: Int): Unit =
    val fc = new JFileChooser()
    fc.setCurrentDirectory(lastDir)
    fc.showOpenDialog(frame) match
      case JFileChooser.APPROVE_OPTION =>
        try
          val image = new FloppyDiskImage(fc.getSelectedFile.toString)
          lastDir = fc.getSelectedFile.getParentFile
          mother.fdc.fdc.getDrives(diskId).insertDisk(image)
        catch
          case t:Throwable =>
            JOptionPane.showMessageDialog(frame,"Floppy error",s"Error while opening image ${fc.getSelectedFile.toString}: $t",JOptionPane.ERROR_MESSAGE)
      case _ =>

  override def openDirectory(diskId: Int): Unit =
    val fc = new JFileChooser()
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    fc.setCurrentDirectory(lastDir)
    fc.showOpenDialog(frame) match
      case JFileChooser.APPROVE_OPTION =>
        try
          val image = new LocalDirectoryFloppyDiskImage(fc.getSelectedFile.toString)
          lastDir = fc.getSelectedFile.getParentFile
          mother.fdc.fdc.getDrives(diskId).insertDisk(image)
        catch
          case t:Throwable =>
            JOptionPane.showMessageDialog(frame,"Floppy error",s"Error while building virtual image from ${fc.getSelectedFile.toString}: $t",JOptionPane.ERROR_MESSAGE)
      case _ =>

  override def ejectImage(diskId: Int): Unit =
    mother.fdc.fdc.getDrives(diskId).ejectDisk()

  override def onMessage(msg: MessageBus.Message): Unit =
    import MessageBus.*
    msg match
      case WarpMode(_,warp) =>
        warpItem.setSelected(warp)
        turboButton.setSelected(warp)
      case MessageBus.VideoModeChanged(_, mode, w, h) =>
        SwingUtilities.invokeLater(() => {
          displayLabel.setText(s"$mode $w x $h")
          if enableDisplayResizing then
            val zoomX = mother.videoCard.getPreferredZoomX
            val zoomY = mother.videoCard.getPreferredZoomY
            displayLabel.invalidate()
            if h > 100 then // TODO
              display.setPreferredSize(new Dimension((w * zoomX).toInt,(h * zoomY).toInt))
              MessageBus.send(MessageBus.DisplaySizeChanged(this,zoomX,zoomY))
              frame.pack()
        })
      case _ =>
  end onMessage

  private def errorHandler(t: Throwable): Unit =
    t.printStackTrace()
    JOptionPane.showOptionDialog(
      frame,
      s"Unexpected error: $t",
      "Unexpected error",
      JOptionPane.YES_NO_CANCEL_OPTION,
      JOptionPane.ERROR_MESSAGE,
      null,
      Array("Ignore", "Open debugger", "Reset"),
      "Ignore"
    ) match
      case JOptionPane.NO_OPTION =>
        openDebugger(enable = true)
      case JOptionPane.CANCEL_OPTION =>
        mother.clock.pause()
        reset(hard = true)
        mother.clock.play()
      case _ => // do nothing
  end errorHandler

  private def pause(on: Boolean): Unit =
    if on then
      mother.clock.pause()
    else
      mother.clock.play()
  end pause

  private def reset(hard: Boolean): Unit =
    pause(on = true)
    if hard then
      mother.hardResetComponent()
    else
      mother.resetComponent()

    pause(on = false)
  end reset

  private def warpMode(on: Boolean): Unit =
    warpItem.setSelected(on)
    MessageBus.send(MessageBus.WarpMode(this, on))

  private def openDebugger(enable: Boolean): Unit =
    if enable then
      debugger.enableTracing(true)
    debugger.showDebugger(true)

  private def closeDebugger(): Unit =
    debugger.enableTracing(false)
    debugger.showDebugger(false)

  private def setGUI(): Unit =
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/disitacoLogo.png")).getImage)
    val dim = mother.videoCard.getPreferredSize
    display = new Display(dim.width, dim.height, s"Disitaco ver. ${Version.VERSION}", frame, mother.clock)
    display.addKeyListener(mother.keyboard)
    display.setFocusable(true)
    display.setFocusTraversalKeysEnabled(false)
    display.grabFocus()

    val displayPanel = new JPanel(new BorderLayout())
    val infoPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))

    turboButton.setFocusable(false)
    turboButton.setToolTipText("Warp mode")
    turboButton.addActionListener(_ => MessageBus.send(MessageBus.WarpMode(this,turboButton.isSelected)))
    videoCardInfoPanel.add(displayLabel)
    infoPanel.add(turboButton)
    infoPanel.add(storagePanel)

    displayPanel.add("Center", display)
    displayPanel.add("South", infoPanel)
    displayPanel.add("North", videoCardInfoPanel)
    videoCardInfoPanel.setVisible(false)
    displayLabel.setIcon(new ImageIcon(getClass.getResource("/resources/trace/monitor.png")))
    display.setPreferredSize(new Dimension(dim.width, dim.height))
    mother.display = display
    frame.getContentPane.add("Center", displayPanel)

    storagePanel.setDiskette(2,Config.getHDImages.size,this)
    for d <- mother.fdc.fdc.getDrives do
      d.setListener(storagePanel)

    mother.videoCard.setClippingOn(on = true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = shutdown()
    })
    buildMenuBar()
    frame.pack()
    frame.setVisible(true)
  end setGUI

  private def shutdown(): Unit =
    MessageBus.send(MessageBus.Shutdown(this))
    frame.setVisible(false)
    sys.exit(0)

  private def boot(): Unit =
    MessageBus.add(this)
    val log = Logger.setLogger(debugger.log)
    mother.setLogger(log)

    mother.initComponent()

    mother.memory.loadROM(Config.getBios)
    debugger.setRAM(mother.memory.getRAM)
    debugger.setROM(mother.memory.getROM)

    // option roms
    for or <- Config.getOptionRomList do
      mother.memory.registerOptionROM(or.address, or.rom, or.label)
      log.info("Option ROM '%s' registered", or.label)

    // HDD
    if Config.isHDConfigured then
      for d <- mother.hdc.hdFdc.getDrives do
        d.setListener(storagePanel)

      for (hdd, i) <- Config.getHDImages.zipWithIndex do
        mother.hdc.hdFdc.getDrives(i).insertDisk(new FixedDiskImage(Config.getAbsolutePath(hdd)))

    // Floppy
    Config.getFloppyAImage.foreach(f => {
      val file = new File(Config.getAbsolutePath(f))
      if file.isDirectory then
        mother.fdc.fdc.getDrives(0).insertDisk(new LocalDirectoryFloppyDiskImage(Config.getAbsolutePath(f)))
      else
        mother.fdc.fdc.getDrives(0).insertDisk(new FloppyDiskImage(Config.getAbsolutePath(f)))
    })
    Config.getFloppyBImage.foreach(f => {
      val file = new File(Config.getAbsolutePath(f))
      if file.isDirectory then
        mother.fdc.fdc.getDrives(1).insertDisk(new LocalDirectoryFloppyDiskImage(Config.getAbsolutePath(f)))
      else
        mother.fdc.fdc.getDrives(1).insertDisk(new FloppyDiskImage(Config.getAbsolutePath(f)))
    })

    // EMS
    if Config.isEMSLotechEnabled then
      val ems = new LotechEMS(Config.getEMSLotechPort)
      mother.add(ems)
      ems.register(mother.ioHandler)
      mother.memory.registerEMSHandler(Config.getEMSLotechAddressPage,ems)

    // Mouse
    if Config.isMouseEnabled then
      mouse = new SerialMouse(display,() => mouseCapItem.setSelected(false),logitech3Buttons = Config.isMouse3Buttons)
      mother.add(mouse)
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

    // Serial signals
    mother.com1.ins8250.setSignalListener(serialDialog.com1)
    mother.com2.ins8250.setSignalListener(serialDialog.com2)

    // Fast13
    if Config.isFastINT13Enabled then
      setFastINT13(enabled = true)

    Logger.getLogger.setLevel(java.util.logging.Level.SEVERE)

    if Config.isDebuggerOpenAtStartup then
      SwingUtilities.invokeAndWait(() => debugger.enableTracing(true))

    mother.clock.setErrorHandler(errorHandler)
    mother.resetComponent()
    mother.clock.start()
  end boot

  private def setFastINT13(enabled:Boolean): Unit =
    fastINT13MenuItem.setSelected(enabled)
    if enabled then
      mother.cpu.setInterruptHandler(0x13, new Fast13IntHandler(mother))
    else
      mother.cpu.setInterruptHandler(0x13, null)
  end setFastINT13

  private def buildMenuBar(): Unit =
    val menubar = new JMenuBar
    frame.setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    val debugMenu = new JMenu("Debug")
    val toolsMenu = new JMenu("Tools")
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(debugMenu)
    menubar.add(toolsMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildDebugMenu(debugMenu)
    buildToolsMenu(toolsMenu)
    buildHelpMenu(helpMenu)
  end buildMenuBar

  private def buildFileMenu(fileMenu:JMenu): Unit =
    val insertFloppy = new JMenu("Insert floppy")
    val floppyAItem = new JMenuItem("Floppy A ...")
    val floppyBItem = new JMenuItem("Floppy B ...")
    floppyAItem.addActionListener(_ => openImage(diskId = 0))
    floppyBItem.addActionListener(_ => openImage(diskId = 1))
    insertFloppy.add(floppyAItem)
    insertFloppy.add(floppyBItem)
    fileMenu.add(insertFloppy)

    val insertDir = new JMenu("Insert virtual floppy")
    val vfloppyAItem = new JMenuItem("Virtual Floppy A ...")
    val vfloppyBItem = new JMenuItem("Virtual Floppy B ...")
    vfloppyAItem.addActionListener(_ => openDirectory(diskId = 0))
    vfloppyBItem.addActionListener(_ => openDirectory(diskId = 1))
    insertDir.add(vfloppyAItem)
    insertDir.add(vfloppyBItem)
    fileMenu.add(insertDir)

    val ejectFloppy = new JMenu("Eject floppy")
    val efloppyAItem = new JMenuItem("Floppy A ...")
    val efloppyBItem = new JMenuItem("Floppy B ...")
    efloppyAItem.addActionListener(_ => ejectImage(diskId = 0))
    efloppyBItem.addActionListener(_ => ejectImage(diskId = 1))
    ejectFloppy.add(efloppyAItem)
    ejectFloppy.add(efloppyBItem)
    fileMenu.add(ejectFloppy)

    val resetItem = new JMenuItem("Reset")
    val powerOffOnItem = new JMenuItem("Power off/on")
    fileMenu.add(resetItem)
    fileMenu.add(powerOffOnItem)
    resetItem.addActionListener(_ => reset(hard = false))
    powerOffOnItem.addActionListener(_ => reset(hard = true))

    val shutdownItem = new JMenuItem("Shutdown")
    fileMenu.add(shutdownItem)
    shutdownItem.addActionListener(_ => shutdown())

  private def buildDebugMenu(debugMenu:JMenu): Unit =
    debugMenu.add(debugMenuItem)
    debugMenuItem.addActionListener(_ => if debugMenuItem.isSelected then openDebugger(enable = false) else closeDebugger())
  private def buildToolsMenu(toolsMenu:JMenu): Unit =
    val ctrlAltDelItem = new JMenuItem("CTRL+ALT+DEL")
    toolsMenu.add(ctrlAltDelItem)
    ctrlAltDelItem.addActionListener(_ => mother.keyboard.pressCtrlAltDel())
    toolsMenu.add(warpItem)
    warpItem.addActionListener(_ => MessageBus.send(MessageBus.WarpMode(this,warpItem.isSelected)))
    mouseCapItem.setToolTipText("Press CTRL+wheel to uncapture")
    toolsMenu.add(mouseCapItem)
    mouseCapItem.addActionListener(_ => mouse.setCapture(mouseCapItem.isSelected))
    toolsMenu.add(fastINT13MenuItem)
    if Config.isCGA then
      val cgaComp = new JCheckBoxMenuItem("CGA video composite")
      toolsMenu.add(cgaComp)
      cgaComp.setSelected(Config.isCGACompositeMonitor)
      cgaComp.addActionListener(_ => mother.videoCard.enableCompositeMonitor(cgaComp.isSelected))
    toolsMenu.add(serialDialogItem)
    serialDialogItem.addActionListener(_ => serialDialog.dialog.setVisible(serialDialogItem.isSelected))
    val showVideoCardInfoItem = new JCheckBoxMenuItem("Show video card info")
    toolsMenu.add(showVideoCardInfoItem)
    showVideoCardInfoItem.addActionListener(_ => {
      videoCardInfoPanel.setVisible(showVideoCardInfoItem.isSelected)
      frame.pack()
    })
    val displayResItem = new JCheckBoxMenuItem("Enable dynamic display resizing")
    displayResItem.setSelected(enableDisplayResizing)
    toolsMenu.add(displayResItem)
    displayResItem.addActionListener(_ => enableDisplayResizing = displayResItem.isSelected)
  private def buildHelpMenu(helpMenu:JMenu): Unit =
    val aboutItem = new JMenuItem("About")
    helpMenu.add(aboutItem)
    aboutItem.addActionListener(_ => AboutPanel.showAboutDialog(frame))




