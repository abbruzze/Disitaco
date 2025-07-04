package ucesoft.disitaco.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.disitaco.cpu.{Instruction, i8088}
import ucesoft.disitaco.debugger.DebuggerUI.*
import ucesoft.disitaco.{Logger, PCComponent}

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout, GridLayout}
import javax.swing.*
import javax.swing.text.DefaultCaret
import scala.compiletime.uninitialized

object Debugger:
  trait VideoFrameListener:
    def newFrame(): Unit
  trait VideoFrameProducer:
    def addVideoFrameListener(vfl:VideoFrameListener): Unit
    def removeVideoFrameListener(vfl:VideoFrameListener): Unit

  sealed trait BreakType

  case class AddressBreakType(address: Int, execute: Boolean = false, read: Boolean = false, write: Boolean = false, var enabled: Boolean = true) extends BreakType:
    override def toString: String =
      val sb = new StringBuilder()
      if read then sb += 'R'
      if write then sb += 'W'
      if execute then sb += 'E'
      sb.toString()

  case object ResetBreak extends BreakType
  case object HaltBreak extends BreakType
  case object StopBreak extends BreakType
  case class InterruptBreak(number: Int, label: String = "") extends BreakType

  trait BreakListener:
    def addBreak(break: AddressBreakType): Unit
    def removeBreak(address: Int): Unit

  trait DisassemblerBreakHandler:
    private var breakListener: List[BreakListener] = Nil

    def addBreakListener(l: BreakListener): Unit =
      breakListener = l :: breakListener
    def hasBreakAt(address: Int): Boolean
    def addExecuteBreakAt(address: Int): Unit = addBreakAt(address, execute = true)
    def addBreakAt(address: Int, read: Boolean = false, write: Boolean = false, execute: Boolean = false): Unit
    def removeBreakAt(address: Int): Unit
    def getBreakStringAt(address: Int): Option[String]
    def getBreakEvent(eventName: String): Option[AnyRef]
    def addBreakEvent(eventName: String, value: AnyRef): Unit
    def removeBreakEvent(eventName: String): Unit

    protected def notifyBreakAdded(b: AddressBreakType): Unit =
      for l <- breakListener do
        l.addBreak(b)

    protected def notifyBreakRemoved(address: Int): Unit =
      for l <- breakListener do
        l.removeBreak(address)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/11/2024 15:12  
 */
class Debugger(cpu:i8088,
               video:Debugger.VideoFrameProducer,
               windowCloseOperation: () => Unit,
               componentsToObserve:PCComponent*) extends Debugger.VideoFrameListener:
  import Debugger.*
  private inline val MAX_LOG_LINES = 1000
  private inline val DIS_LINES = 25
  private enum StepState:
    case NoStep, WaitReturn, WaitTarget

  private val frame = new JFrame("Debugger")
  private val componentToObserveModels = for c <- componentsToObserve.filterNot(_ == null) yield new PropertiesTableModel(c,frame)
  private val logPanel = new RSyntaxTextArea(10,100)
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))

  private var frameByFrameMode = false
  private val frameByFrameLock = new Object
  private var frameByFrameCond = false
  private var frameCount = 0

  private val i8088ramMemoryDumpItem = new JCheckBoxMenuItem("RAM")
  private var i8088ramDialog : JDialog = uninitialized
  private val romDumpItem = new JCheckBoxMenuItem("ROM")
  private var romDialog: JDialog = uninitialized
  private val i8088DisassemblerItem = new JCheckBoxMenuItem("Disassembler")

  private val i8088Debugger = new i8088Debugger
  private val i8088DisassemblerPanel = new DisassemblerPanel("8088",
    (model,a) => {
      val dis = cpu.disassemble(a)
      model.add(dis)
      a + dis.size
    },
    frame,
    i8088Debugger,
    () => i8088DisassemblerItem.setSelected(false))
  private val i8088DisassemblerDialog = i8088DisassemblerPanel.dialog

  private var selectedDebugger = i8088Debugger

  private val i8088BreakItem = new JCheckBoxMenuItem("Breaks")
  private val i8088BreakDialog = new BreakMasterPanel(
    "8088",
    frame,
    6,
    break => i8088Debugger.removeBreakAt(break.address),
    break => i8088Debugger.addBreakAt(break.address,read = break.read,write = break.write,execute = break.execute),
    new i8088BreakEventPanel(i8088Debugger),
    i8088Debugger,
    () => i8088BreakItem.setSelected(false)
  ).dialog

  private val tabbedPane = new JTabbedPane()
  private var logLines = 0

  // =============================================================================================================
  private class i8088Debugger extends JPanel with DisassemblerBreakHandler with i8088.BusListener:
    private val registerTableModel = new i8088RegisterTableModel(cpu, general = true)
    private val segmentRegisterTableModel = new i8088RegisterTableModel(cpu, general = false)
    private val statusRegisterTableModel = new i8088StatusRegisterTableModel(cpu)
    private val pcRegisterTableModel = new i8088PCTableModel(cpu)
    private val disassembledTableModel = new DisassembledTableModel(address => getBreakStringAt(address))
    private val componentPanel = new JTabbedPane()
    private val distable = new JTable(disassembledTableModel)
    private val commentedROMPanel = new CommentedROMPanel
    private var commentedROMEnabled = false
    private var commentedROMSplitPane : JSplitPane = uninitialized

    private var stepOverPending, stepOutPending = StepState.NoStep
    private var stepOverOutStopPending = false
    private var stepOverTargetAddress = 0
    private val semaphore = new Object

    private val breaks = new collection.mutable.HashMap[Int,AddressBreakType]
    private var breakOnReset = false
    private var breakOnInt = false
    private var breakIntNum = 0
    private var breakOnHalt = false

    private var stepByStep = false
    private var stepAlways = false
    private var stepInstruction: Instruction = uninitialized
    private var stepDisassemble: Instruction.Disassembled = uninitialized

    private val cpuEnabled = {
      val item = new JCheckBox("CPU enabled")
      item.setSelected(true)
      item.addActionListener(_ => onCPUEnabled(item.isSelected))
      item
    }
    private var tracingOnFile = false
    private var tracingListener: TraceListener = scala.compiletime.uninitialized

    private def onCPUEnabled(enabled: Boolean): Unit = cpu.setComponentEnabled(enabled)

    private def existsBreakPending: Boolean =
      breaks.nonEmpty || breakOnReset || breakOnInt || breakOnHalt

    def startTracingOnFile(tracingListener: TraceListener): Unit =
      this.tracingListener = tracingListener
      tracingOnFile = true
      stepAlways = true
      cpu.setBusListener(this)
      nextStep()

    def stopTracingOnFile(): Unit =
      tracingOnFile = false
      if !existsBreakPending then
        cpu.removeBusListener()
      stepAlways = false

    def isTracing: Boolean = stepByStep

    def enableCommentedROMPanel(enabled:Boolean): Unit =
      commentedROMEnabled = enabled
      if !enabled then
        commentedROMSplitPane.setRightComponent(null)
        commentedROMPanel.clear()
      else
        commentedROMSplitPane.setRightComponent(commentedROMPanel)
        commentedROMSplitPane.setDividerLocation(0.5)

    def commentedROMGotoAddress(address:Int): Unit = commentedROMPanel.gotoAddress(address)
    def commentedROMLoad(file:String): Unit =
      commentedROMPanel.load(file,cpu.getLastInstructionAddress)
    // breaks
    override def getBreakEvent(eventName: String): Option[AnyRef] =
      eventName match
        case "reset" => if breakOnReset then Some(java.lang.Boolean.TRUE) else None
        case "halt" => if breakOnHalt then Some(java.lang.Boolean.TRUE) else None
        case "int" => if breakOnInt then Some(java.lang.Integer.valueOf(breakIntNum)) else None
        case _ => None
    override def addBreakEvent(eventName: String, value: AnyRef): Unit =
      cpu.setBusListener(this)
      eventName match
        case "reset" => breakOnReset = true
        case "halt" => breakOnHalt = true
        case "interrupt" =>
          breakOnInt = true
          breakIntNum = value.asInstanceOf[java.lang.Integer].intValue()
          println(s"Break check on $breakIntNum")
        case _ =>
    override def removeBreakEvent(eventName: String): Unit =
      if !existsBreakPending then
        cpu.removeBusListener()
      eventName match
        case "reset" => breakOnReset = false
        case "halt" => breakOnHalt = false
        case "interrupt" =>
          breakOnInt = false
        case _ =>
    override def hasBreakAt(address: Int): Boolean = breaks.contains(address)
    override def addBreakAt(address: Int, read: Boolean, write: Boolean, execute: Boolean): Unit =
      cpu.setBusListener(this)
      breaks += address -> AddressBreakType(address, execute = execute, read = read, write = write)
      notifyBreakAdded(AddressBreakType(address, read = read, write = write, execute = execute))
      disassembledTableModel.update()
    override def removeBreakAt(address: Int): Unit =
      breaks -= address
      notifyBreakRemoved(address)
      disassembledTableModel.update()
      if !existsBreakPending then
        cpu.removeBusListener()
    override def getBreakStringAt(address: Int): Option[String] = breaks.get(address).map(_.toString)
    // bus listener
    def busRead(address: Int, byteRead: Boolean): Unit =
      if tracingOnFile then return

      breaks.get(address) match
        case Some(break) if break.read  =>
          log(s"Break on 8088 read on address ${address.toHexString}")
          stepByStep = true
          disassembledTableModel.clear()
          updateDisassembly(address)

          checkTracingState(true)
          updateModels()
          semaphore.synchronized {
            semaphore.wait()
          }
        case _ =>
    def busWrite(address: Int, value: Int, byteWrite: Boolean): Unit =
      if tracingOnFile then return

      breaks.get(address) match
        case Some(break) if break.write =>
          log(s"Break on 8088 write on address ${address.toHexString}")
          stepByStep = true
          disassembledTableModel.clear()
          updateDisassembly(address)

          checkTracingState(true)
          updateModels()
          semaphore.synchronized {
            semaphore.wait()
          }
        case _ =>
    def fetch(address: Int): Unit =
      if tracingOnFile then
        tracingListener.onTrace(cpu.disassemble(address,abs = true).getDisassembled,address)
        return

      breaks.get(address) match
        case Some(break) if break.execute =>
          log(s"Break $break on address ${address.toHexString}")
          stepByStep = true
        case _ =>

      checkStepOverOut(address)

      if !stepAlways && stepByStep then
        disassembledTableModel.clear()
        updateDisassembly(address)
        checkTracingState(true)
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }
    def interrupt(intNum: Int): Unit = {
      if tracingOnFile then
        tracingListener.onTrace(s"Break on 8088 interrupt $intNum",-1)
        return
      if breakOnInt && breakIntNum == intNum then
        log(s"Break on 8088 interrupt $intNum")
        stepByStep = true
        stepOutPending = StepState.NoStep
        stepOverPending = StepState.NoStep
        checkTracingState(true)
        updateDisassembly()
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }
      else if stepByStep then
        log(s"Break on 8088 interrupt $intNum")
    }

    def reset(): Unit =
      if tracingOnFile then return

      if breakOnReset then
        log(s"Break on 8088 reset")
        stepByStep = true
        stepOutPending = StepState.NoStep
        stepOverPending = StepState.NoStep
        checkTracingState(true)
        updateDisassembly()
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }
    def halted(): Unit =
      if tracingOnFile then return

      if breakOnHalt then
        log(s"Break on 8088 halt")
        stepByStep = true
        stepOutPending = StepState.NoStep
        stepOverPending = StepState.NoStep
        checkTracingState(true)
        updateDisassembly()
        updateModels()
        semaphore.synchronized {
          semaphore.wait()
        }

    private def checkStepOverOut(address: Int): Unit =
      stepOverPending match
        case StepState.WaitTarget =>
          if address == stepOverTargetAddress then
            stepOutPending = StepState.NoStep
            stepAlways = false
        case _ =>
    end checkStepOverOut

    private def updateDisassembly(address: Int = -1): Unit = swing {
      var adr = if address == -1 then cpu.regs.absoluteIP else address
      for a <- 1 to DIS_LINES do
        val dis = cpu.disassemble(adr)
        if a == 1 then
          stepDisassemble = dis
        disassembledTableModel.add(dis)
        adr = (adr + dis.size) & 0xF_FFFF
      disassembledTableModel.update()
    }
    // ================================
    def enableTracing(enabled: Boolean): Unit =
      if stepByStep != enabled then
        if enabled then
          cpu.setBusListener(this)
          frame.setVisible(true)
        else
          nextStep()
          if !existsBreakPending then
            cpu.removeBusListener()
      stepByStep = enabled

      if !enabled then
        stepAlways = false
        stepByStep = false

      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep

    def stepIn(): Unit =
      nextStep()

    def stepOver(): Unit =
      stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
      stepOverPending = StepState.WaitTarget
      stepAlways = true
      nextStep()

    def stepOut(): Unit = stepIn() // TODO

    def nextStep(): Unit =
      semaphore.synchronized {
        semaphore.notify()
      }

    init()

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      add("North", northPanel)
      val rightPanel = new JPanel(new BorderLayout())
      // registers
      val registerPanel = new JPanel(new GridLayout(0, 1))
      // sr
      val srtable = new JTable(statusRegisterTableModel)
      srtable.getTableHeader.setReorderingAllowed(false)
      srtable.setDefaultRenderer(classOf[java.lang.Boolean], new StatusRegisterRenderer)
      srtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%01X"))
      var sp = new JScrollPane(srtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      srtable.setPreferredScrollableViewportSize(srtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Status register"))
      registerPanel.add(sp)
      // data
      val datatable = new JTable(registerTableModel)
      datatable.getTableHeader.setReorderingAllowed(false)
      datatable.setDefaultRenderer(classOf[String], RegisterRenderer("%04X"))
      sp = new JScrollPane(datatable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      datatable.setPreferredScrollableViewportSize(datatable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Data registers"))
      registerPanel.add(sp)
      // address
      val adrtable = new JTable(segmentRegisterTableModel)
      adrtable.getTableHeader.setReorderingAllowed(false)
      adrtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%04X"))
      sp = new JScrollPane(adrtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      adrtable.setPreferredScrollableViewportSize(adrtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Segment registers"))
      registerPanel.add(sp)
      // misc.
      val pctable = new JTable(pcRegisterTableModel)
      pctable.getTableHeader.setReorderingAllowed(false)
      pctable.setDefaultRenderer(classOf[String], new RegisterRenderer("%04X"))
      pctable.setDefaultRenderer(classOf[java.lang.Integer], new RegisterRenderer("%s"))
      sp = new JScrollPane(pctable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      pctable.setPreferredScrollableViewportSize(pctable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Misc."))
      registerPanel.add(sp)
      // components
      for tm <- componentToObserveModels do
        val table = new JTable(tm)
        table.getTableHeader.setReorderingAllowed(false)
        table.setDefaultRenderer(classOf[Prop], new PropertiesCellRenderer(tm))
        sp = new JScrollPane(table)
        table.setPreferredScrollableViewportSize(new Dimension(0, 200))
        val compColModel = table.getColumnModel
        compColModel.getColumn(0).setMinWidth(80)
        compColModel.getColumn(0).setMaxWidth(150)
        compColModel.getColumn(1).setMinWidth(150)
        compColModel.getColumn(1).setMaxWidth(300)

        componentPanel.addTab(tm.comp.getComponentName, tm.comp.getIcon.orNull,sp)

      rightPanel.add("Center", componentPanel)
      rightPanel.add("North", registerPanel)

      // disassemble panel
      distable.getTableHeader.setReorderingAllowed(false)
      distable.setDefaultRenderer(classOf[String], new DisassembledCellRenderer)
      sp = new JScrollPane(distable)
      sp.setBorder(BorderFactory.createTitledBorder("Disassembler"))
      val colModel = distable.getColumnModel
      colModel.getColumn(0).setMinWidth(25)
      colModel.getColumn(0).setMaxWidth(30)
      colModel.getColumn(1).setMinWidth(70)
      colModel.getColumn(1).setMaxWidth(80)
      colModel.getColumn(2).setMinWidth(140)
      colModel.getColumn(2).setMaxWidth(160)
      colModel.getColumn(3).setMinWidth(220)
      colModel.getColumn(3).setMaxWidth(250)
      distable.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit =
          if e.getClickCount == 2 then
            val row = distable.rowAtPoint(e.getPoint)
            val address = disassembledTableModel.getAddressAt(row)
            if hasBreakAt(address) then
              removeBreakAt(address)
            else
              addExecuteBreakAt(address)
            disassembledTableModel.update()
      })

      sp.setMinimumSize(new Dimension(500, 0))
      commentedROMSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, rightPanel, null)
      commentedROMSplitPane.setContinuousLayout(true)
      commentedROMSplitPane.setOneTouchExpandable(true)
      val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sp, commentedROMSplitPane)
      splitPane.setContinuousLayout(true)
      splitPane.setOneTouchExpandable(true)
      add("Center", splitPane)
    end init

    private def updateModels(): Unit = swing {
      registerTableModel.contentUpdated()
      segmentRegisterTableModel.contentUpdated()
      statusRegisterTableModel.contentUpdated()
      pcRegisterTableModel.contentUpdated()
      disassembledTableModel.update()
      for tm <- componentToObserveModels do tm.update()
      distable.setRowSelectionInterval(0, 0)
    }

  end i8088Debugger

  // ==================================================================================================
  
  def setRAM(ram:Array[Byte]): Unit =
    i8088ramDialog = new MemoryDumper(ram, 0x000000, "RAM", frame, () => i8088ramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog

  swing {
    init()
  }

  private def swing(f: => Unit) : Unit =
    if !SwingUtilities.isEventDispatchThread then
      SwingUtilities.invokeLater(() => f)
    else f

  private def init(): Unit =
    frame.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        windowCloseOperation()
    )
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/logo.png")).getImage)
    val mainPanel = new JPanel(new BorderLayout())
    mainPanel.setPreferredSize(new Dimension(1000,400))
    val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    mainPanel.add("North",northPanel)

    val toolBar = new JToolBar("Tracer")
    toolBar.setRollover(true)
    northPanel.add(toolBar)

    // buttons
    onOffButton.setToolTipText("Enable tracing")
    onOffButton.addActionListener(_ => enableTracing(onOffButton.isSelected))

    val stepInButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down.png")))
    stepInButton.setToolTipText("Step in")
    stepInButton.addActionListener(_ => stepIn() )
    val stepOverButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down_left.png")))
    stepOverButton.addActionListener(_ => stepOver())
    stepOverButton.setToolTipText("Step over")
    val stepOutButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/up.png")))
    stepOutButton.addActionListener(_ => stepOut())
    stepOutButton.setToolTipText("Step out")

    val enableButtons = (enabled:Boolean) => {
      onOffButton.setEnabled(enabled)
      stepInButton.setEnabled(enabled)
      stepOverButton.setEnabled(enabled)
      stepOutButton.setEnabled(enabled)
    }

    val disaButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
    disaButton.addActionListener(_ => disassembleGUI())
    disaButton.setToolTipText("Disassemble")

    val writeButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/write.png")))
    writeButton.addActionListener(_ => memoryGUI())
    writeButton.setToolTipText("Memory")

    val nextFrame = new JButton(new ImageIcon(getClass.getResource("/resources/trace/nextFrame.png")))
    nextFrame.addActionListener(_ => advanceByOneFrame())
    nextFrame.setToolTipText("Advance by one frame")
    nextFrame.setEnabled(false)

    val frameByFrameMode = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/frameByFrameMode.png")))
    frameByFrameMode.addActionListener(_ => {
      nextFrame.setEnabled(frameByFrameMode.isSelected)
      setFrameByFrameMode(frameByFrameMode.isSelected)
    })
    frameByFrameMode.setToolTipText("Frame by frame mode")

    val breakPoint = new JButton(new ImageIcon(getClass.getResource("/resources/trace/red_breakpoint.png")))
    breakPoint.addActionListener(_ => breakGUI())
    breakPoint.setToolTipText("Breakpoints")

    val saveTrace = new JButton(new ImageIcon(getClass.getResource("/resources/trace/save.png")))
    saveTrace.addActionListener(_ => saveTraceUI())
    saveTrace.setToolTipText("Save live disassembly on file")

    val commentedROMButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/rom.png")))
    commentedROMButton.setToolTipText("Enable/disable commented rom")
    commentedROMButton.addActionListener(_ => if enableCommentedROMPanel(commentedROMButton.isSelected) then commentedROMButton.setSelected(false))

    toolBar.add(onOffButton)
    toolBar.add(stepInButton)
    toolBar.add(stepOverButton)
    toolBar.add(stepOutButton)
    toolBar.add(disaButton)
    toolBar.add(writeButton)
    toolBar.add(frameByFrameMode)
    toolBar.add(nextFrame)
    toolBar.add(breakPoint)
    toolBar.add(saveTrace)
    toolBar.add(commentedROMButton)

    // log panel
    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val lscroll = new RTextScrollPane(logPanel)
    lscroll.setMinimumSize(new Dimension(0, 70))
    lscroll.setPreferredSize(new Dimension(0, 150))
    lscroll.setBorder(BorderFactory.createTitledBorder("Log panel"))


    val logButtonPanel = new JPanel(new BorderLayout())
    logButtonPanel.add("Center", lscroll)
    val logToolBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logButtonPanel.add("South", logToolBar)
    val clearLog = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    clearLog.setToolTipText("Clear log panel")
    logToolBar.add(clearLog)
    val logSeverityGroup = new ButtonGroup
    val logSeverityInfoButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_info.png")))
    logSeverityInfoButton.setToolTipText("Set log level to INFO")
    val logSeverityWarningButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_warning.png")))
    logSeverityWarningButton.setToolTipText("Set log level to WARNING")
    val logSeverityOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_off.png")))
    logSeverityOffButton.setToolTipText("Set log level to OFF, log disabled")
    logSeverityGroup.add(logSeverityInfoButton)
    logSeverityGroup.add(logSeverityWarningButton)
    logSeverityGroup.add(logSeverityOffButton)
    val logSeverityPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logSeverityPanel.add(logSeverityInfoButton)
    logSeverityPanel.add(logSeverityWarningButton)
    logSeverityPanel.add(logSeverityOffButton)
    logToolBar.add(logSeverityPanel)
    logSeverityOffButton.setSelected(true)
    logSeverityInfoButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.INFO)
    })
    logSeverityWarningButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.WARNING)
    })
    logSeverityOffButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.OFF)
    })
    clearLog.addActionListener(_ => {
      logLines = 0
      logPanel.setText("")
    })

    tabbedPane.addTab("8088",new ImageIcon(getClass.getResource("/resources/trace/cpu.png")),i8088Debugger)
    tabbedPane.addChangeListener(e => {
      tabbedPane.getSelectedIndex match
        case 0 =>
          selectedDebugger = i8088Debugger
          enableButtons(true)
    })
    mainPanel.add("Center",tabbedPane)

    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, logButtonPanel)
    splitPane.setOneTouchExpandable(true)

    // menu
    val menu = new JMenuBar
    val memoryMenu = new JMenu("Memory")
    val disMenu = new JMenu("Disassembler")
    val breakMenu = new JMenu("Breaks")

    i8088ramMemoryDumpItem.addActionListener(_ => i8088ramDialog.setVisible(i8088ramMemoryDumpItem.isSelected) )
    romDumpItem.addActionListener(_ => romDialog.setVisible(romDumpItem.isSelected) )
    memoryMenu.add(romDumpItem)
    memoryMenu.add(i8088ramMemoryDumpItem)
    menu.add(memoryMenu)

    i8088DisassemblerItem.addActionListener(_ => i8088DisassemblerDialog.setVisible(i8088DisassemblerItem.isSelected))
    disMenu.add(i8088DisassemblerItem)

    menu.add(disMenu)

    i8088BreakItem.addActionListener(_ => i8088BreakDialog.setVisible(i8088BreakItem.isSelected))

    breakMenu.add(i8088BreakItem)

    menu.add(breakMenu)

    frame.setJMenuBar(menu)

    frame.getContentPane.add("Center",splitPane)
    frame.pack()
  end init

  def enableTracing(enabled: Boolean): Unit =
    selectedDebugger.enableTracing(enabled)
    checkTracingState(enabled)
    if !enabled then
      windowCloseOperation()

  protected def selectDebugger(index:Int): Unit =
    swing {
      tabbedPane.setSelectedIndex(index)
      selectedDebugger = index match
        case 0 => i8088Debugger
    }

  def showDebugger(show:Boolean): Unit =
    frame.setVisible(show)

  private def checkTracingState(enabled: Boolean): Unit =
    if enabled then
      onOffButton.setToolTipText("Disable tracing")
      if !frame.isVisible then
        frame.setVisible(true)
    else
      onOffButton.setToolTipText("Enable tracing")
      i8088Debugger.enableTracing(false)
      i8088Debugger.nextStep()
    onOffButton.setSelected(enabled)

  private def stepIn(): Unit =
    selectedDebugger.stepIn()

  private def stepOver(): Unit =
    selectedDebugger.stepOver()

  private def stepOut(): Unit =
    selectedDebugger.stepOut()

  private def disassembleGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        i8088DisassemblerItem.setSelected(true)
        i8088DisassemblerDialog.setVisible(true)

  private def memoryGUI(): Unit =
    i8088ramMemoryDumpItem.setSelected(true)
    i8088ramDialog.setVisible(true)

  def log(msg: String): Unit = swing {
    if logLines == MAX_LOG_LINES then
      logPanel.append("Hey, too many logs here, please clear this panel to keep reading new logs")
      logLines += 1
    else if logLines < MAX_LOG_LINES then
      logPanel.append(msg)
      logPanel.append("\n")
      logLines += 1
  }

  private def checkVBlankState(): Unit =
    if frameByFrameMode then
      video.addVideoFrameListener(this)
    else
      video.removeVideoFrameListener(this)
      advanceByOneFrame()

  override def newFrame(): Unit =
    if frameByFrameMode then
      frameByFrameLock.synchronized {
        while frameByFrameCond do
          frameByFrameLock.wait(1000)
      }
      frameByFrameCond = true

  private def setFrameByFrameMode(on:Boolean): Unit =
    frameByFrameMode = on
    frameByFrameCond = on
    frameCount = 0
    checkVBlankState()
  private def advanceByOneFrame(): Unit =
    frameByFrameLock.synchronized {
      frameByFrameCond = false
      frameByFrameLock.notify()
    }
    frameCount += 1

  private def breakGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        i8088BreakItem.setSelected(true)
        i8088BreakDialog.setVisible(true)

  private def saveTraceUI(): Unit =
    val std = new SaveTraceDialog(frame,tl => selectedDebugger.startTracingOnFile(tl),() => selectedDebugger.stopTracingOnFile())
    std.setLocationRelativeTo(frame)
    std.setVisible(true)

  private def enableCommentedROMPanel(enabled:Boolean): Boolean =
    if enabled then
      val fc = new JFileChooser("Load commented rom")
      fc.showOpenDialog(frame) match
        case JFileChooser.CANCEL_OPTION =>
          true
        case JFileChooser.APPROVE_OPTION =>
          i8088Debugger.commentedROMLoad(fc.getSelectedFile.toString)
          i8088Debugger.enableCommentedROMPanel(enabled)
          false
    else
      i8088Debugger.enableCommentedROMPanel(enabled)
      false

  def setROM(rom:Array[Byte]): Unit =
    romDialog = new MemoryDumper(rom,0,"ROM",frame,() => romDumpItem.setSelected(false),canUpdate = false,setPreferredScrollableViewportSize = false, showASCII = true).dialog
