package ucesoft.disitaco.storage

import ucesoft.disitaco.{MessageBus, PCComponent}
import ucesoft.disitaco.chips.i8237
import ucesoft.disitaco.chips.i8237.DMADevice
import ucesoft.disitaco.storage.DiskImage.DiskGeometry

import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 17/04/2025 18:49
 *
 * Notes:
 * 1) The original NEC µPD765A provided a mechanism to determine whether drives were “ready”, that is, a diskette was inserted and the drive door closed.
 * The FDC would poll all drives and trigger an interrupt whenever the ready state for any drive changed.
 * IBM did not use this mechanism—there was no ready signal on the floppy drive cable (or the drive), and the diskette adapter card
 * was built such that the FDC thought the drives were always ready by tying the ready signal permanently high.
 * As a consequence, software could never be certain whether a diskette had been changed or not.
 *
 * 2) IBM subverted the NEC µPD765A design in more ways than just the ready signal mentioned above.
 * The FDC was built to control up to four drives, and was intended to support seek/recalibrate operations as non-exclusive tasks, potentially seeking on all drives at once.
 * The FDC required drive selection bits for almost every command, specifying the drive which the operation applied to.
 * An overlapped seek would select one drive, send a step pulse, select the next drive, send a step pulse, etc.
 * However, IBM did not connect the drive selection bits of the FDC. Instead, the Digital Output Register (DOR) was solely in control of drive selection (and motor control).
 * As a consequence, the drive selection bits programmed into the FDC did not matter—only the DOR determined which drive, if any, was selected.
 * This arrangement made it impossible to use overlapped seeks because the FDC could not rotate the drive selection.
 * This design (apart from the drive ready signal tied high) also precluded the use of drive polling, which likewise required the FDC to change drive selection.
 */
class i8272A(floppyAGeometry:DiskGeometry,floppyBGeometry:DiskGeometry,dma:i8237, dmaChannel:Int, irq: Boolean => Unit) extends PCComponent with DMADevice:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/fdc.png"))
  override protected val componentName = "NEC_PD765"

  private inline val FDC_CLOCK = 8_000_000
  private inline val RESET_WAIT_MILLIS = 1

  private enum State:
    case Idle, Cmd, Execution, Result

  // commands, id bit 0-4 of command byte
  private case class Command(id:Int,cmd:Int,inputBytes:Int,label:String,command: Command => Unit):
    val MT : Boolean = (cmd & 0x80) != 0 // true=multi-track
    val MF : Boolean = (cmd & 0x40) != 0 // FM or MFM true=MFM
    val SK : Boolean = (cmd & 0x20) != 0 // true=Skip deleted Data Address Mark
    private var _waitCycles = 0
    private var _state = 0
    private val commandBytes = new ListBuffer[Int]
    private var commandBytesArray : Array[Int] = Array()
    private var resultBytes : Seq[Int] = Seq.empty
    private var resultIndex = 0
    private val dataQueue = new collection.mutable.Queue[Byte]
    private var tc = false
    private var dmaReady = true
    private var totalSectorRead = 0
    private var totalSectorWrite = 0
    private var writeLen = 0
    private var writeReady = false
    private var writeTransferAction : Array[Byte] => Unit = _ => {}

    def getTotalSectorRead:Int = totalSectorRead
    def getTotalSectorWrite: Int = totalSectorWrite

    def terminalCount(): Unit = tc = true
    def isTerminalCount: Boolean = tc

    def isDMAReady: Boolean = dmaReady
    def setDMABusy(): Unit = dmaReady = false
    def setDMAReady(): Unit = dmaReady = true

    def setWriteNotReady(): Unit = writeReady = false
    def setWriteReady(len:Int, writeAction:Array[Byte] => Unit): Unit =
      writeLen = len
      writeReady = true
      writeTransferAction = writeAction
    def isWriteReady: Boolean = writeReady

    def dataWriteEnqueue(b:Byte): Unit =
      dataQueue.enqueue(b)
      if dataQueue.length == writeLen then
        totalSectorWrite += 1
        writeTransferAction(dataQueue.toArray)
        dataQueue.clear()
    def dataReadEnqueue(i:IterableOnce[Byte]): Unit =
      dataQueue.addAll(i)
      totalSectorRead += 1
    def dataQueueSize: Int = dataQueue.size
    def dataDequeue(): Byte = dataQueue.dequeue()

    def addCMDByte(b: Int): Boolean =
      commandBytes += b
      val completed = commandBytes.length == inputBytes
      if completed then
        commandBytesArray = commandBytes.toArray
      completed
    def getCMDBytes: Array[Int] = commandBytesArray
    def setResultBytes(rb:Int*): Unit =
      resultBytes = rb
    def getResultByte: Int =
      val b = resultBytes(resultIndex)
      resultIndex += 1
      b
    def hasResultBytes: Boolean = resultIndex < resultBytes.length

    def state : Int = _state
    def state_=(s:Int): Unit = _state = s

    def waitCycles(wc:Int): Unit = _waitCycles = wc
    def decWaitCycles(): Boolean =
      _waitCycles -= 1
      _waitCycles <= 0
    override def toString: String = s"$label(MT=$MT,id=$id,cmd=$cmd,dataQueueSize=${dataQueue.size},commandBytes=${commandBytes.map("%02X" format _).mkString("[",",","]")},resultBytes=${resultBytes.map("%02X" format _).mkString("[",",","]")})"
  end Command

  private val noCommand = new Command(-1,-1,0,"Empty",null):
    override def toString = "-"

  private inline val CMD_READ_DATA_ID        = 0b00110
  private inline val CMD_READ_TRACK_ID       = 0b00010
  private inline val CMD_READ_DELETED_ID     = 0b01100
  private inline val CMD_READ_ID_ID          = 0b01010
  private inline val CMD_WRITE_DATA_ID       = 0b00101
  private inline val CMD_FORMAT_TRACK_ID     = 0b01101
  private inline val CMD_WRITE_DEL_DATA_ID   = 0b01001
  private inline val CMD_SCAN_EQUAL_ID       = 0b10001
  private inline val CMD_SCAN_LOW_EQUAL_ID   = 0b11001
  private inline val CMD_RECALIBRATE_ID      = 0b00111
  private inline val CMD_SENSE_INT_STATUS_ID = 0b01000
  private inline val CMD_SPECIFY_ID          = 0b00011
  private inline val CMD_SENSE_DRV_STATUS_ID = 0b00100
  private inline val CMD_SEEK_ID             = 0b01111
  private inline val CMD_SCAN_HIGH_EQUAL_ID  = 0b11101

  private val COMMAND_INFO : Map[Int,(String,Int,Command => Unit)] = Map(
    CMD_READ_DATA_ID        -> ("Read Data",8,commandReadWriteData),
    CMD_READ_TRACK_ID       -> ("Read track",8,commandUnimplemented),
    CMD_READ_DELETED_ID     -> ("Read Deleted",8,commandUnimplemented),
    CMD_READ_ID_ID          -> ("Read ID",1,commandUnimplemented),
    CMD_WRITE_DATA_ID       -> ("Write data",8,commandReadWriteData),
    CMD_FORMAT_TRACK_ID     -> ("Format track",5,commandFormatTrack),
    CMD_WRITE_DEL_DATA_ID   -> ("Write Deleted",8,commandUnimplemented),
    CMD_SCAN_EQUAL_ID       -> ("Scan equal",8,commandUnimplemented),
    CMD_SCAN_LOW_EQUAL_ID   -> ("Scan low equal",8,commandUnimplemented),
    CMD_RECALIBRATE_ID      -> ("Recalibrate",1,commandCalibrate),
    CMD_SENSE_INT_STATUS_ID -> ("Sense interrupt",0,commandSenseInterrupt),
    CMD_SPECIFY_ID          -> ("Specify",2,commandSpecify),
    CMD_SENSE_DRV_STATUS_ID -> ("Sense drive",1,commandSenseDrive),
    CMD_SEEK_ID             -> ("Seek",2,commandSeek),
    CMD_SCAN_HIGH_EQUAL_ID  -> ("Scan high equal",8,commandUnimplemented)
  )

  import State.*
  
  MessageBus.add(this)

  private val drives = Array(
    new DiskDrive(0,floppyAGeometry,FDC_CLOCK,RPM = 300),
    new DiskDrive(1,floppyBGeometry,FDC_CLOCK,RPM = 300),
    new DiskDrive(2,floppyAGeometry,FDC_CLOCK,RPM = 300),
    new DiskDrive(3,floppyBGeometry,FDC_CLOCK,RPM = 300)
  )

  private var state = Idle
  private var dmaEnabled = false
  private var fdcEnabled = false
  private var pendingCommand : Command = noCommand
  private var executingCommand : Command = noCommand

  private var selectedDrive = 0

  private var waitCycles = 0

  private var pendingReset = false

  // status register bits
  private var status_rqm = true
  private var status_dio = false // If DIO = "1" then transfer is from Data Register to the processor.
  private var status_busy = false

  // Drive parameters
  /*
  SRT stands for the Stepping Rate for the FDD (1 to 16 ms in 1 me increments).
  The same Stepping Rate applies to ail drives (F=1 ms. E»2 ms. etc.).
  */
  private var stepRateTime = 0
  /*
  HUT stands for the head unload time after a read or write operation has occurred
  (16 to 240 ms In 16 ms Increments).
  */
  private var headUnloadTime = 0
  /*
  HLT stands for the head load time In the FDD (2 to 254 ms In 2 ms Increments).
  */
  private var headLoadTime = 0

  final def getDrives: Array[DiskDrive] = drives

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.Shutdown(_) =>
        drives.flatMap(_.getDiskInserted).foreach(_.closeAndFlush())
      case _ =>
  end onMessage

  override final def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    val list = new ListBuffer[Property]
    list += Property("State",state.toString)
    list += Property("Enabled",fdcEnabled.toString)
    list += Property("Pending command",pendingCommand.toString)
    list += Property("Executing command",executingCommand.toString)
    list += Property("Selected drive",selectedDrive.toString)
    list += Property("Main status",readMainStatusRegister.toString)
    list += Property("Step rate time (cycles)",stepRateTime.toString)
    list += Property("Head load time (cycles)",headLoadTime.toString)
    list += Property("Head unload time (cycles)",headUnloadTime.toString)
    var d = 0
    while d < 2 do
      val dl = if d == 0 then "A" else "B"
      list += Property(s"Drive $dl disk",drives(d).getDiskInserted.map(_.diskName).getOrElse("-"))
      list += Property(s"Drive $dl disk geometry",drives(d).getDiskInserted.map(_.diskGeometry.toString).getOrElse("-"))
      list += Property(s"Drive $dl head",drives(d).getHead.toString)
      list += Property(s"Drive $dl track",drives(d).getCurrentTrack.toString)
      list += Property(s"Drive $dl sector",drives(d).getSector.toString)
      list += Property(s"Drive $dl cycles per byte", drives(d).getSectorByteCycles.toString)
      d += 1
    list.toList

  override final protected def reset(): Unit =
    resetState()
    for d <- drives do
      d.reset()
  end reset

  private def resetState(): Unit =
    state = Idle
    fdcEnabled = false

    status_rqm = true
    status_dio = false
    status_busy = false
    selectedDrive = 0

  private def doReset(): Unit =
    resetState()
    fdcEnabled = true
    triggerIRQ(true)
    for d <- drives do
      d.softwareReset()
    log.info("%s doReset",componentName)

  private inline def millisToCycles(millis:Int): Int = (FDC_CLOCK / 1000.0 * millis).toInt

  private def triggerIRQ(on:Boolean): Unit =
    if on then
      irq(false)
      irq(true)
    else
      irq(false)

  final def writeCommandDataRegister(cmd:Int): Unit =
    if !fdcEnabled then
      log.info("%s not enabled, ignoring command %d",componentName,cmd)
      return

    triggerIRQ(false)
    log.info("%s write command data reg = %02X",componentName,cmd)
    state match
      case Idle =>
        createCommand(cmd) match {
          case Some(command) =>
            log.info("%s Issued command %s, expecting %d bytes",componentName,command.label,command.inputBytes)
            pendingCommand = command
            status_busy = true
            if command.inputBytes > 0 then
              state = Cmd
            else
              executingCommand = pendingCommand
              pendingCommand = noCommand
              state = Execution
              status_rqm = dmaEnabled // when in dma-mode rqm becomes false
          case None =>
            log.warning("%s invalid command %02X",componentName,cmd)
        }
      case Cmd =>
        log.info("%s adding byte %d to command %s",componentName,cmd,pendingCommand.label)
        if pendingCommand.addCMDByte(cmd) then
          log.info("%s command %s completed. Executing ...",componentName,pendingCommand.label)
          executingCommand = pendingCommand
          pendingCommand = noCommand
          state = Execution
      case Execution =>
      case Result =>
        log.warning("%s command byte ignored during RESULT phase",componentName)

  private def createCommand(cmd:Int): Option[Command] =
    COMMAND_INFO.get(cmd & 0x1F) match
      case Some((label,bytes,command)) =>
        Some(Command(cmd & 0x1F,cmd,bytes,label,command))
      case None =>
        None

  // DMA handling ========================================================
  override def dack(): Unit =
    dma.DREQ(active = false, channel = dmaChannel, this)
  override def dmaRead(): Int =
    executingCommand.setDMAReady()
    if executingCommand.dataQueueSize > 0 then
      executingCommand.dataDequeue() & 0xFF
      //println(s"DMA read ${r.toHexString} remaining in queue ${executingCommand.dataQueueSize}")
    else
      log.error("%s DMA reading from an empty queue. Command is %s",componentName,executingCommand.label)
      0
  override def dmaWrite(value: Int): Unit =
    executingCommand.dataWriteEnqueue(value.toByte)
    executingCommand.setDMAReady()
  override def tc(): Unit = executingCommand.terminalCount()

  // Commands ===========================================================
  private def commandUnimplemented(cmd:Command): Unit =
    println(s"Error command $cmd not implemented")
    sys.exit(1)
  private def commandCompleted(resultPhase:Boolean,irq:Boolean = false): Unit =
    status_rqm = true

    if irq then
      triggerIRQ(true)

    if !resultPhase then
      executingCommand = noCommand
      state = Idle
      status_dio = false
      status_busy = false
    else
      state = Result
      status_dio = true

  private def commandFormatTrack(cmd:Command): Unit =
    cmd.state match
      case 0 =>
        val bytes = cmd.getCMDBytes
        val drive = drives(selectedDrive)
        log.info("%s executing %s on track %d with %s",componentName,cmd.label,drive.getCurrentTrack,bytes.mkString("[",",","]"))
        //printf("%s executing %s on track %d with %s\n",componentName,cmd.label,drive.getCurrentTrack,bytes.mkString("[",",","]"))
        val n = bytes(1)
        val sc = bytes(2)
        if !drive.isMotorOn then
          log.warning("%s %s ignored: drive(%d) motor is off",componentName,cmd.label,selectedDrive)
          commandCompleted(resultPhase = false,irq = true)
        else if n != 2 || sc != drive.geometry.sectorsPerTrack then
          log.warning("%s %s failed: parameters error: N=%d SC=%d incompatibles with drive's geometry %s",componentName,cmd.label,n,sc,drive.geometry)
          val st0 = makeST0(IC = 0b01, HD = drive.getHead, US = selectedDrive)
          val st1 = makeST1()
          val st2 = makeST2()
          cmd.setResultBytes(st0, st1, st2, drive.getCurrentTrack, drive.getHead, drive.getSector, 2)
          commandCompleted(resultPhase = true, irq = true)
        else if !drive.hasDiskInserted then
          val st0 = makeST0(IC = 0b01,HD = drive.getHead,US = selectedDrive)
          val st1 = makeST1(ND = 1,MA = 1)
          val st2 = makeST2(MD = 1)
          cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
          commandCompleted(resultPhase = true, irq = true)
        else if drive.getDiskInserted.get.isReadOnly then
          val st0 = makeST0(IC = 0b01,HD = drive.getHead,US = selectedDrive)
          val st1 = makeST1(NW = 1)
          val st2 = makeST2()
          cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
          commandCompleted(resultPhase = true, irq = true)
        else // ok
          cmd.state = 1
      case 1 => // seek sector 1
        val bytes = cmd.getCMDBytes
        val drive = drives(selectedDrive)
        if drive.getSector == 1 then
          cmd.state = 2
      case 2 => // go to next sector
        val bytes = cmd.getCMDBytes
        val secPerTrack = bytes(2)
        if cmd.getTotalSectorWrite == secPerTrack then
          cmd.state = 4
        else
          cmd.setWriteReady(4, params => {
            log.info("%s formatting params received [%d]: %s",componentName,cmd.getTotalSectorWrite,params.mkString("[",",","]"))
            //printf("%s formatting params received [%d]: %s\n",componentName,cmd.getTotalSectorWrite,params.mkString("[",",","]"))
            // format sector
            val formattedSector = Array.fill[Byte](DiskImage.SECTOR_SIZE)(bytes(4).toByte)
            drives(selectedDrive).formatTrackSector(head = params(1),sector = params(2),data = formattedSector)
            cmd.state = 2
          })
          cmd.state = 3
      case 3 => // wait write completed
      case 4 => // completed
        val drive = drives(selectedDrive)
        log.info("%s formatting of track %d/%d completed",componentName,drive.getCurrentTrack,drive.getHead)
        //printf("%s formatting of track %d/%d completed\n",componentName,drive.getCurrentTrack,drive.getHead)
        val st0 = makeST0(HD = drive.getHead,US = selectedDrive)
        val st1 = makeST1()
        val st2 = makeST2()
        cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
        commandCompleted(resultPhase = true, irq = true)

  end commandFormatTrack

  private def commandSpecify(cmd:Command): Unit =
    val bytes = cmd.getCMDBytes
    log.info("%s executing %s with %s",componentName,cmd.label,bytes.mkString("[",",","]"))
    // SRT stands for the stepping rate for the FDD (2 to 32 ms in 2-ms increments).
    val SRT = bytes(0) >> 4
    // HUT stands for the head unload time after a read or write operation has occurred (0 to 480 ms in 32-ms increments).
    val HUT = bytes(0) & 0xF
    val ND = (bytes(1) & 1) != 0
    // HLT stands for the head load time in the FDD (4 to 512 ms in 4-ms increments).
    val HLT = bytes(1) >> 1
    stepRateTime = millisToCycles((SRT + 1) << 1)
    headUnloadTime = millisToCycles(HUT << 5)
    headLoadTime = millisToCycles((HLT + 1) << 2)
    log.info("%s SRT=%d(%d) HUT=%d(%d) ND=%b HLT=%d(%d)", componentName,SRT,stepRateTime,HUT,headUnloadTime,ND,HLT,headLoadTime)
    commandCompleted(resultPhase = false)
  end commandSpecify

  private def commandSenseInterrupt(cmd:Command): Unit =
    try
      var d = 0
      while d < 4 do
        val PCN = drives(d).getCurrentTrack
        if drives(d).isIrqReady then
          val st0 = makeST0(US = d, IC = 0b11,HD = drives(d).getHead)
          cmd.setResultBytes(st0, PCN)
          return
        else if drives(d).isIrqSeek then
          val equipError = if drives(d).isEquipmentError then 1 else 0
          val ic = if drives(d).isSeekFailed then 0b01 else 0b00
          val st0 = makeST0(IC = ic,US = d, SE = 1,EC = equipError, HD = drives(d).getHead)
          cmd.setResultBytes(st0, PCN)
          return
        else if drives(d).isIrqResult then // TODO check
          val equipError = if drives(d).isEquipmentError then 1 else 0
          val ic = if drives(d).isSeekFailed || drives(d).isRWFailed then 0b01 else 0b00
          val st0 = makeST0(IC = ic,US = d, SE = 1,EC = equipError, HD = drives(d).getHead)
          cmd.setResultBytes(st0, PCN)
        d += 1
      end while
      val st0 = makeST0(US = selectedDrive, IC = 0b10) // INVALID COMMAND
      cmd.setResultBytes(st0, 0)
    finally
      commandCompleted(resultPhase = true)
  end commandSenseInterrupt

  private def commandCalibrate(cmd:Command): Unit =
    cmd.state match
      case 0 => // start calibration
        if !drives(selectedDrive).isMotorOn then
          log.warning("%s %s ignored: drive(%d) motor is off",componentName,cmd.label,selectedDrive)
          commandCompleted(resultPhase = false,irq = true)
        else
          drives(selectedDrive).moveOnTrack(track = 0)
          cmd.waitCycles(stepRateTime)
          cmd.state = 1
      case 1 =>
        if cmd.decWaitCycles() then
          if drives(selectedDrive).stepTrack() then
            commandCompleted(resultPhase = false,irq = true)
          else
            cmd.waitCycles(stepRateTime)
  end commandCalibrate

  private def commandSeek(cmd:Command): Unit =
    cmd.state match
      case 0 => // start seek
        val drive = drives(selectedDrive)
        if !drive.isMotorOn then
          log.warning("%s %s ignored: drive(%d) motor is off", componentName, cmd.label, selectedDrive)
          commandCompleted(resultPhase = false)
        else
          val bytes = cmd.getCMDBytes
          val head = (bytes(0) >> 2) & 1
          val ncn = bytes(1)
          log.info("%s seeking on drive %d NCN=%d H=%d",componentName,selectedDrive,ncn,head)
          drive.setHead(head)
          if drive.getCurrentTrack != ncn then
            drive.moveOnTrack(track = ncn)
            cmd.waitCycles(stepRateTime)
          else
            cmd.waitCycles(0)
          cmd.state = 1
      case 1 =>
        if cmd.decWaitCycles() then
          if drives(selectedDrive).stepTrack() then
            if drives(selectedDrive).getCurrentTrack == cmd.getCMDBytes(1) then
              log.info("%s seek completed on drive %d track set t %d",componentName,selectedDrive,drives(selectedDrive).getCurrentTrack)
            else
              log.info("%s seek failed on drive %d, track %d not found",componentName,selectedDrive,cmd.getCMDBytes(1))
            commandCompleted(resultPhase = false,irq = true)
          else
            cmd.waitCycles(stepRateTime)
  end commandSeek

  private def commandSenseDrive(cmd:Command): Unit =
    val drive = drives(selectedDrive)
    val st3 = makeST3(
      US = selectedDrive,
      WP = drive.getDiskInserted.map(d => if d.isReadOnly then 1 else 0).getOrElse(0),
      T0 = if drive.getCurrentTrack == 0 then 1 else 0,
      TS = if drive.geometry.heads == 2 then 1 else 0,
      HD = drive.getHead
    )
    cmd.setResultBytes(st3)
    commandCompleted(resultPhase = true)
  end commandSenseDrive

  private def commandReadWriteData(cmd:Command): Unit =
    val drive = drives(selectedDrive)
    cmd.state match
      case 0 => // start
        if !drive.hasDiskInserted then
          val st0 = makeST0(IC = 0b01,HD = drive.getHead,US = selectedDrive)
          val st1 = makeST1(ND = 1,MA = 1)
          val st2 = makeST2(MD = 1)
          cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
          commandCompleted(resultPhase = true, irq = true)
        else if drive.getDiskInserted.get.isReadOnly then
          val st0 = makeST0(IC = 0b01,HD = drive.getHead,US = selectedDrive)
          val st1 = makeST1(NW = 1)
          val st2 = makeST2()
          cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
          commandCompleted(resultPhase = true, irq = true)
        else if !drive.isMotorOn then
          log.warning("%s %s ignored: drive(%d) motor is off", componentName, cmd.label, selectedDrive)
          commandCompleted(resultPhase = false)
        else
          val bytes = cmd.getCMDBytes
          val track = bytes(1)
          val head = bytes(2)
          val sector = bytes(3)
          val sectorSize = bytes(4)
          val eot = bytes(5)
          val gpl = bytes(6)
          val dtl = bytes(7)
          if sectorSize != 2 then
            log.warning("%s command %s sectorSize is not 2: %d",componentName,cmd.label,sectorSize)
          if eot != drive.geometry.sectorsPerTrack then
            log.warning("%s command %s eot is not %d: %d",componentName,cmd.label,drive.geometry.sectorsPerTrack,eot)

          if cmd.id == CMD_READ_DATA_ID then
            log.info("%s reading data on drive %d track=%d head=%d sector=%d size=%d eot=%d gpl=%d dtl=%d",componentName,selectedDrive,track,head,sector,sectorSize,eot,gpl,dtl)
            //printf("%s reading data on drive %d track=%d head=%d sector=%d size=%d eot=%d gpl=%d dtl=%d\n",componentName,selectedDrive,track,head,sector,sectorSize,eot,gpl,dtl)
          else
            log.info("%s writing data on drive %d track=%d head=%d sector=%d size=%d eot=%d gpl=%d dtl=%d",componentName,selectedDrive,track,head,sector,sectorSize,eot,gpl,dtl)
            //printf("%s writing data on drive %d track=%d head=%d sector=%d size=%d eot=%d gpl=%d dtl=%d\n",componentName,selectedDrive,track,head,sector,sectorSize,eot,gpl,dtl)

          drive.setHead(head)
          drive.moveOnTrack(track = track)
          cmd.waitCycles(stepRateTime)
          cmd.state = 1
      case 1 => // seek track
        if cmd.decWaitCycles() then
          if drive.stepTrack() then
            if drive.getCurrentTrack == cmd.getCMDBytes(1) then
              log.info("%s seek completed on drive %d track set to %d", componentName, selectedDrive, drive.getCurrentTrack)
            else
              log.info("%s seek failed on drive %d, track %d not found", componentName, selectedDrive, cmd.getCMDBytes(1))
            cmd.state = cmd.id match
              case CMD_READ_DATA_ID => 2
              case CMD_WRITE_DATA_ID => 3
              case _ =>
                throw new IllegalArgumentException(s"Bad command ${cmd.label} during read/write")
          else
            cmd.waitCycles(stepRateTime)
      case 2 => // read seek sector
        if cmd.isTerminalCount then // all bytes transferred?
          cmd.state = 4
        else
          val cmdBytes = cmd.getCMDBytes
          if drive.getSector == cmdBytes(3) && cmd.dataQueueSize == 0 then // target sector ?
            val bytes = drive.readSector()
            if cmdBytes(4) == 0 then
              //println(s"Read sector with custom sector size ${cmdBytes(7)}")
              val firstBytes = bytes.toArray.take(cmdBytes(7))
              cmd.dataReadEnqueue(firstBytes)
            else
              cmd.dataReadEnqueue(bytes)
            // ok, go to next sector
            cmdBytes(3) += 1
            if cmdBytes(3) > cmdBytes(5) then // go over last sector?
              cmdBytes(3) = 1
      case 3 => // write seek sector
        if cmd.isTerminalCount then // all bytes transferred?
          cmd.state = 4
        else
          val cmdBytes = cmd.getCMDBytes
          if drive.getSector == cmdBytes(3)  && !cmd.isWriteReady then // target sector ?
            val sectorSize = cmdBytes(4) match
              case 0 => cmdBytes(7)
              case 2 => DiskImage.SECTOR_SIZE
              case _ =>
                //println(s"While writing sector size is not 2 nor 0: ${cmdBytes(4)}")
                DiskImage.SECTOR_SIZE
            cmd.setWriteReady(sectorSize,data => {
              //println(s"Writing data to sector ${drive.getSector} ...")
              drive.writeSector(data)
              cmd.setWriteNotReady()
            })
            cmdBytes(3) += 1
            if cmdBytes(3) > cmdBytes(5) then // go over last sector?
              cmdBytes(3) = 1
      case 4 => // command completed on TC
        val cmdBytes = cmd.getCMDBytes
        if cmd.id == CMD_READ_DATA_ID then
          log.info(s"%s %s completed [MT=${cmd.MT}]. Total sector read: %d",componentName,executingCommand.label,executingCommand.getTotalSectorRead)
          //printf(s"%s %s completed [MT=${cmd.MT}]. Total sector read: %d\n",componentName,executingCommand.label,executingCommand.getTotalSectorRead)
        else
          log.info(s"%s %s completed [MT=${cmd.MT}]. Total sector write: %d", componentName, executingCommand.label, executingCommand.getTotalSectorWrite)
          //printf(s"%s %s completed [MT=${cmd.MT}]. Total sector write: %d\n", componentName, executingCommand.label, executingCommand.getTotalSectorWrite)
        val st0 = makeST0(HD = drive.getHead,US = selectedDrive)
        val st1 = makeST1()
        val st2 = makeST2()
        cmd.setResultBytes(st0,st1,st2,drive.getCurrentTrack,drive.getHead,drive.getSector,2)
        commandCompleted(resultPhase = true, irq = true)
  // ============================================================

  /*
  diskette command/data register 0 (ST0)
		 bit 7-6 (IC) last command status
			 = 00  command terminated successfully
			 = 01  command terminated abnormally
			 = 10  invalid command
			 = 11  terminated abnormally by change in ready signal
		 bit 5 (SE)	 = 1  seek completed
		 bit 4 (EC)	 = 1  equipment check occurred after error
		 bit 3 (NR)	 = 1  not ready
		 bit 2 (HD)	 = 1  head number at interrupt
		 bit 1-0 (US) = 1  unit select (0=A 1=B .. )
			      (on PS/2	01=A  10=B)
  */
  private def makeST0(IC:Int = 0,SE:Int = 0,EC:Int = 0,NR:Int = 0,HD:Int = 0,US:Int = 0): Int =
    (IC & 3) << 6 | (SE & 1) << 5 | (EC & 1) << 4 | (NR & 1) << 3 | (HD & 1) << 2 | (US & 3)
  /*
  status register 1 (ST1)
		 bit 7	(EN)    end of cylinder; sector# greater then sectors/track
		 bit 6 = 0
		 bit 5 (DE) = 1  CRC error in ID or data field
		 bit 4 (OR) = 1  overrun
		 bit 3 = 0
		 bit 2 (ND) = 1  sector ID not found
		 bit 1 (NW) = 1  write protect detected during write
		 bit 0 (MA) = 1  ID address mark not found
  */
  private def makeST1(EN:Int = 0,DE:Int = 0,OR:Int = 0,ND:Int = 0,NW:Int = 0,MA:Int = 0): Int =
    (EN & 1) << 7 | (DE & 1) << 5 | (OR & 1) << 4 | (ND & 1) << 2 | (NW & 1) << 1 | (MA & 1)
  /*
  status register 2 (ST2)
		 bit 7 = 0
		 bit 6 (CM) = 1  deleted Data Address Mark detected
		 bit 5 (DD) = 1  CRC error in data
		 bit 4 (WC) = 1  wrong cylinder detected
		 bit 3 (SH) = 1  scan command equal condition satisfied
		 bit 2 (SN) = 1  scan command failed, sector not found
		 bit 1 (BC) = 1  bad cylinder, ID not found
		 bit 0 (MD) = 1  missing Data Address Mark
  */
  private def makeST2(CM:Int = 0,DD:Int = 0,WC:Int = 0,SH:Int = 0,SN:Int = 0,BC:Int = 0,MD:Int = 0): Int =
    (CM & 1) << 6 | (DD & 1) << 5 | (WC & 1) << 4 | (SH & 1) << 3 | (SN & 1) << 2 | (BC & 1) << 1 | (MD & 1)
  /*
  status register 3 (ST3)
		 bit 7	(FT)    fault status signal
		 bit 6	(WP)    write protect status
		 bit 5	(RDY)   ready status
		 bit 4	(T0)    track zero status
		 bit 3	(TS)    two-sided status signal
		 bit 2	(HD)    side select (head select)
		 bit 1-0 (US)   unit select (0=A 1=B .. )
  */
  private def makeST3(FT:Int = 0,WP:Int = 0,RDY:Int = 0,T0:Int = 0,TS:Int = 0,HD:Int = 0,US:Int = 0): Int =
    (FT & 1) << 7 | (WP & 1) << 6 | (RDY & 1) << 5 | (T0 & 1) << 4 | (TS & 1) << 3 | (HD & 1) << 2 | (US & 3)

  // Ports
  /*
  diskette controller DOR (Digital Output Register)
		 bit 7-6    reserved on PS/2
		 bit 7 = 1  drive 3 motor enable
		 bit 6 = 1  drive 2 motor enable
		 bit 5 = 1  drive 1 motor enable
		 bit 4 = 1  drive 0 motor enable
		 bit 3 = 1  DMA & Interrupt enable
		 bit 2 = 1  FDC enable	(controller reset)
		       = 0  hold FDC at reset
		 bit 1-0    drive select (0=A 1=B ..)
  */
  final def writeDigitalOutputRegister(value:Int): Unit =
    log.info("%s write DOR = %02X",componentName,value)
    dmaEnabled = (value & 0x8) != 0
    val oldFdcEnabled = fdcEnabled
    fdcEnabled = (value & 0x4) != 0
    // check reset condition
    if !oldFdcEnabled && fdcEnabled then
      log.info("%s resetting and sending IRQ ...",componentName)
      pendingReset = true
      waitCycles = millisToCycles(RESET_WAIT_MILLIS)
    selectedDrive = value & 3
    if selectedDrive > 1 then
      log.warning("%s selected a drive > 1 = %d",componentName,selectedDrive)
    if (value & (0x40 | 0x80)) != 0 then
      log.warning("%s motor enabled on a unhandled drive",componentName)
    drives(0).setMotorOn((value & 0x10) != 0)
    drives(1).setMotorOn((value & 0x20) != 0)
    log.info("%s dmaEnabled=%b fdcEnabled=%b selected drive=%d motorA=%b motorB=%b",componentName,dmaEnabled,fdcEnabled,selectedDrive,drives(0).isMotorOn,drives(1).isMotorOn)
  /*
  diskette controller main status register
		 bit 7 = 1  RQM	 data register is ready
			 0  no access is permitted
		 bit 6 = 1  transfer is from controller to system
			 0  transfer is from system to controller
		 bit 5 = 1  non-DMA mode
		 bit 4 = 1  diskette controller is busy
		 bit 3 = 1  drive 3 busy (reserved on PS/2)
		 bit 2 = 1  drive 2 busy (reserved on PS/2)
		 bit 1 = 1  drive 1 busy (= drive is in seek mode)
		 bit 0 = 1  drive 0 busy (= drive is in seek mode)
		Note:	in non-DMA mode, all data transfers occur through
			  port 03F5h and the status registers (bit 5 here
			  indicates data read/write rather than
			  command/status read/write)
  */
  final def readMainStatusRegister: Int =
    triggerIRQ(false)
    var status = 0
    if status_rqm then status |= 0x80
    if status_dio then status |= 0x40
    if !dmaEnabled then status |= 0x20
    if status_busy then status |= 0x10
    if selectedDrive == 1 && executingCommand != null && executingCommand.id == CMD_SEEK_ID then status |= 0x2
    else if selectedDrive == 0 && executingCommand != null && executingCommand.id == CMD_SEEK_ID then status |= 0x1
    status
  /*
  diskette command/data register 0 (ST0)
		 bit 7-6      last command status
			 = 00  command terminated successfully
			 = 01  command terminated abnormally
			 = 10  invalid command
			 = 11  terminated abnormally by change in ready signal
		 bit 5	 = 1  seek completed
		 bit 4	 = 1  equipment check occurred after error
		 bit 3	 = 1  not ready
		 bit 2	 = 1  head number at interrupt
		 bit 1-0 = 1  unit select (0=A 1=B .. )
			      (on PS/2	01=A  10=B)

		status register 1 (ST1)
		 bit 7	    end of cylinder; sector# greater then sectors/track
		 bit 6 = 0
		 bit 5 = 1  CRC error in ID or data field
		 bit 4 = 1  overrun
		 bit 3 = 0
		 bit 2 = 1  sector ID not found
		 bit 1 = 1  write protect detected during write
		 bit 0 = 1  ID address mark not found

		status register 2 (ST2)
		 bit 7 = 0
		 bit 6 = 1  deleted Data Address Mark detected
		 bit 5 = 1  CRC error in data
		 bit 4 = 1  wrong cylinder detected
		 bit 3 = 1  scan command equal condition satisfied
		 bit 2 = 1  scan command failed, sector not found
		 bit 1 = 1  bad cylinder, ID not found
		 bit 0 = 1  missing Data Address Mark

		status register 3 (ST3)
		 bit 7	    fault status signal
		 bit 6	    write protect status
		 bit 5	    ready status
		 bit 4	    track zero status
		 bit 3	    two-sided status signal
		 bit 2	    side select (head select)
		 bit 1-0    unit select (0=A 1=B .. )
  */
  final def readCommandDataRegister: Int =
    triggerIRQ(false)
    state match
      case Result =>
        if executingCommand.hasResultBytes then
          val result = executingCommand.getResultByte
          log.info("%s reading %s result byte %d",componentName,executingCommand.label,result)
          if !executingCommand.hasResultBytes then
            commandCompleted(resultPhase = false)
          result
        else
          log.warning("%s reading result byte overflow!",componentName)
          0
      case _ =>
        log.warning("%s reading command data register in phase %s",componentName,state)
        //printf("%s reading command data register in phase %s\n",componentName,state)
        sys.exit(1)
        0

  // =================== Clock 8Mhz ===========================
  final def clock(_cycles:Int): Unit =
    var cycles = _cycles
    while cycles > 0 do
      if waitCycles > 0 then
        waitCycles -= 1
        if waitCycles == 0 then
          if pendingReset then
            pendingReset = false
            doReset()

      // rotate disks
      var d = 0
      while d < 2 do
        drives(d).rotate()
        d += 1

      state match
        case Execution =>
          // check DMA
          if dmaEnabled then
            executingCommand.id match
              case CMD_READ_DATA_ID =>
                if executingCommand.dataQueueSize > 0 && executingCommand.isDMAReady then
                  executingCommand.setDMABusy()
                  dma.DREQ(active = true,channel = dmaChannel,this)
              case CMD_WRITE_DATA_ID =>
                if executingCommand.isWriteReady && executingCommand.isDMAReady then
                  executingCommand.setDMABusy()
                  dma.DREQ(active = true,channel = dmaChannel,this)
              case CMD_FORMAT_TRACK_ID =>
                if executingCommand.isWriteReady && executingCommand.isDMAReady then
                  executingCommand.setDMABusy()
                  dma.DREQ(active = true,channel = dmaChannel,this)
              case _ =>
          end if
          executingCommand.command(executingCommand)
        case _ =>

      cycles -= 1
    end while
