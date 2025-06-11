package ucesoft.disitaco.storage

import ucesoft.disitaco.{DipSwitches, MessageBus, PCComponent}
import ucesoft.disitaco.chips.i8237
import ucesoft.disitaco.chips.i8237.DMADevice
import ucesoft.disitaco.storage.DiskImage.DiskGeometry

import javax.swing.ImageIcon
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 07/05/2025 17:37
 *
 *         Type Tracks Heads
 *           1    306    4
 *           2    615    4
 *          13    306    8
 *          16    612    4
 */
class XebecHDController(dma:i8237, dmaChannel:Int, irq: Boolean => Unit,diskIDOffset:Int,revision3:Boolean = false,numberOfDrives:Int = 1) extends PCComponent with DMADevice:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/hdc.png"))
  override protected val componentName = "XebecController"

  private inline val FDC_CLOCK = 8_000_000 // TODO CHECK
  private inline val RPM = 3000

  private enum State:
    case Reset, WaitCommand, ReceivingCommandBytes, CommandExecution, CommandStatus, ResponseSenseBytes

  import State.*

  private case class DCB(drive:Int = 0,var head:Int = 0,var track:Int = 0,var sector:Int = 0,interleaveOrBlockCount:Int = 0,controlField:Int = 0)
  private case class DriveCharacteristics(var maxTracks:Int,var maxHeads:Int)
  private case class Error(var flag:Boolean,var drive:Int = 0,var errorType:Int = 0,var errorCode:Int = 0,var dcb:DCB = DCB()):
    def setError(error:Int,drive:Int): Unit =
      this.drive = drive
      flag = error != NO_ERROR
      errorType = error >> 4
      errorCode = error & 0xF

  private case class Command(id:Int,dcbLen:Int,label:String,command: Command => Unit):
    private var _state = 0
    private var commandBytesArray : Array[Int] = Array()
    private val commandBytes = new ArrayBuffer[Int]
    private val senseStatusBytes = new collection.mutable.Queue[Int]
    private val dataQueue = new collection.mutable.Queue[Byte]
    private var initDriveCharsFlag = false
    private var dmaReady = true
    private var tc = false
    private var writeLen = 0
    private var writeReady = false
    private var writeTransferAction: Array[Byte] => Unit = _ => {}
    private var totalSectorRead = 0
    private var totalSectorWrite = 0
    private var _waitCycles = 0
    private var dcb : DCB = uninitialized

    def waitCycles(wc: Int): Unit = _waitCycles = wc
    def decWaitCycles(): Boolean =
      _waitCycles -= 1
      _waitCycles <= 0

    def state: Int = _state
    def state_=(s: Int): Unit = _state = s

    def getTotalSectorRead: Int = totalSectorRead
    def getTotalSectorWrite: Int = totalSectorWrite

    def terminalCount(): Unit = tc = true
    def isTerminalCount: Boolean = tc

    def getInitDriveCharsFlag: Boolean = initDriveCharsFlag
    def setInitDriveCharsFlag(): Unit = initDriveCharsFlag = true

    def isDMAReady: Boolean = dmaReady
    def setDMABusy(): Unit = dmaReady = false
    def setDMAReady(): Unit = dmaReady = true

    def setWriteNotReady(): Unit = writeReady = false
    def setWriteReady(len: Int, writeAction: Array[Byte] => Unit): Unit =
      writeLen = len
      writeReady = true
      writeTransferAction = writeAction

    def isWriteReady: Boolean = writeReady

    def dataWriteEnqueue(b: Byte): Unit =
      dataQueue.enqueue(b)
      if dataQueue.length == writeLen then
        totalSectorWrite += 1
        writeTransferAction(dataQueue.toArray)
        dataQueue.clear()

    def dataReadEnqueue(i: IterableOnce[Byte]): Unit =
      dataQueue.addAll(i)
      totalSectorRead += 1

    def dataQueueSize: Int = dataQueue.size
    def dataDequeue(): Byte = dataQueue.dequeue()

    def getDCB: DCB =
      if dcb == null then
        dcb = DCB(
          drive = (commandBytes(0) >> 5) & 1,
          head = commandBytes(0) & 0x1F,
          track = (commandBytes(1) & 0xC0) << 2 | commandBytes(2),
          sector = commandBytes(1) & 0x3F,
          interleaveOrBlockCount = commandBytes(3),
          controlField = commandBytes(4)
        )
      dcb

    def setSenseStatusBytes(byte:Int*): Unit =
      senseStatusBytes.clear()
      senseStatusBytes.addAll(byte)
    def hasNextSenseStatusByte: Boolean = senseStatusBytes.nonEmpty
    def nextSenseStatusByte(): Int = senseStatusBytes.dequeue()

    def getCMDBytes: Array[Int] = commandBytesArray
    def addCMDByte(b: Int): Boolean =
      commandBytes += b
      val completed = commandBytes.length == dcbLen
      if completed then
        commandBytesArray = commandBytes.toArray
      completed
    def getCMDByteSize: Int = commandBytes.length

  private val noCommand = Command(-1,0,"",null)

  private inline val CMD_TEST_DRIVE_READY   = 0b000_00000
  private inline val CMD_RECALIBRATE        = 0b000_00001
  private inline val CMD_REQ_SENSE_STATUS   = 0b000_00011
  private inline val CMD_FORMAT_DRIVE       = 0b000_00100
  private inline val CMD_READY_VERIFY       = 0b000_00101
  private inline val CMD_FORMAT_TRACK       = 0b000_00110
  private inline val CMD_FORMAT_BAD_TRACK   = 0b000_00111
  private inline val CMD_READ               = 0b000_01000
  private inline val CMD_WRITE              = 0b000_01010
  private inline val CMD_SEEK               = 0b000_01011
  private inline val CMD_INIT_DRIVE_CHAR    = 0b000_01100
  private inline val CMD_READ_ECC_BURST     = 0b000_01101
  private inline val CMD_READ_FROM_BUFFER   = 0b000_01110
  private inline val CMD_WRITE_TO_BUFFER    = 0b000_01111
  private inline val CMD_RAM_DIAGNOSTIC     = 0b111_00000
  private inline val CMD_DRIVE_DIAGNOSTIC   = 0b111_00011
  private inline val CMD_INTERNAL_DIAG      = 0b111_00100
  private inline val CMD_READ_LONG          = 0b111_00101
  private inline val CMD_WRITE_LONG         = 0b111_00110

  private val COMMAND_INFO : Map[Int,(String,Int,Command => Unit)] = Map(
    CMD_TEST_DRIVE_READY  -> ("TestDriveReady",5,commandTestDriveReady),
    CMD_RECALIBRATE       -> ("Recalibrate",5,commandRecalibrate),
    CMD_REQ_SENSE_STATUS  -> ("RequestSenseStatus",5,commandSenseStatus),
    CMD_FORMAT_DRIVE      -> ("FormatDrive",5,commandFormatDrive),
    CMD_READY_VERIFY      -> ("ReadyVerify",5,commandReadyVerify),
    CMD_FORMAT_TRACK      -> ("FormatTrack",5,commandUnimplemented),
    CMD_FORMAT_BAD_TRACK  -> ("FormatBadTrack",5,commandUnimplemented),
    CMD_READ              -> ("Read",5,commandReadWrite),
    CMD_WRITE             -> ("Write",5,commandReadWrite),
    CMD_SEEK              -> ("Seek",5,commandUnimplemented),
    CMD_INIT_DRIVE_CHAR   -> ("InitializeDriveCharacteristics",13,commandInitializeDriveCharacteristics),
    CMD_READ_ECC_BURST    -> ("ReadECCBurstErrorLength",5,commandUnimplemented),
    CMD_READ_FROM_BUFFER  -> ("ReadDataFromSectorBuffer",5,commandUnimplemented),
    CMD_WRITE_TO_BUFFER   -> ("WriteDataToBuffer",5,commandWriteBuffer),
    CMD_RAM_DIAGNOSTIC    -> ("RAMDiagnostic",5,commandRAMDiagnostic),
    CMD_DRIVE_DIAGNOSTIC  -> ("DriveDiagnostic",5,commandDriveDiagnostic),
    CMD_INTERNAL_DIAG     -> ("ControllerInternalDiagnostic",5,commandControllerInternalDiagnostic),
    CMD_READ_LONG         -> ("ReadLong",5,commandUnimplemented),
    CMD_WRITE_LONG        -> ("WriteLong",5,commandUnimplemented)
  )

  private inline val NO_ERROR = 0b00_0000
  private inline val ERROR_NOT_READY = 0b00_0100 // After the controller selected the drive, the drive did not respond with a ready signal.

  private val drives = Array(
    new DiskDrive(diskIDOffset, DiskImage.geoHD10M, FDC_CLOCK, RPM = RPM ,isFixedDrive = true),
    new DiskDrive(diskIDOffset + 1, DiskImage.geoHD10M, FDC_CLOCK, RPM = RPM, isFixedDrive = true)
  )
  /*
    Active after SEL has been sent
  */
  private inline val STATUS_R1_BUSY   = 0b00001000 // BUSY BIT
  /*
    High when information on the bus consist of data bytes
  */
  private inline val STATUS_R1_BUS    = 0b00000100 // COMMAND/DATA BIT
  /*
    High when the controller is sending data back to the host
  */
  private inline val STATUS_R1_IOMODE = 0b00000010 // MODE BIT
  private inline val STATUS_R1_REQ    = 0b00000001 // REQUEST BIT
  private inline val STATUS_R1_INT    = 0b00100000 // INTERRUPT BIT


  private inline val PATTERN_DMA_MASK = 0b00000001
  private inline val PATTERN_IRQ_MASK = 0b00000010

  private val trackStepMillis = millisToCycles(3) // 3 ms

  /*
    00 = 20M (306,8,17) Drive table 3
    01 = 20M (615,4,17) Drive table 2
    10 = 20M (612,4,17) Drive table 1
    11 = 10M (306,4,17) Drive table 0
  */
  private val dipSwitches = new DipSwitches(1,0b1111)
  private val driveChars = DriveCharacteristics(0,0)
  private var state : State = Reset
  private var pendingCommand : Command = noCommand
  private var executingCommand : Command = noCommand
  private var dmaIrqRegister = 0
  private val lastError = Error(false)
  private var irqActive = false
  private val sectorBuffer = Array.ofDim[Byte](DiskImage.SECTOR_SIZE)
  
  MessageBus.add(this)

  final def getDrives: Array[DiskDrive] = drives

  private inline def millisToCycles(millis:Int): Int = (FDC_CLOCK / 1000.0 * millis).toInt

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.Shutdown(_) =>
        drives.flatMap(_.getDiskInserted).foreach(_.closeAndFlush())
      case _ =>
  end onMessage

  override final protected def reset(): Unit =
    resetState()
    for d <- drives do
      d.reset()
  end reset

  private def resetState(): Unit =
    state = Reset
    dmaIrqRegister = 0
    pendingCommand = noCommand
    executingCommand = noCommand
    lastError.flag = false
    log.info("%s controller reset",componentName)

  private def createCommand(byte:Int): Option[Command] =
    COMMAND_INFO.get(byte) match
      case Some((label,len,cmd)) =>
        Some(Command(byte,len,label,cmd))
      case None =>
        None

  private def triggerIRQ(on: Boolean): Unit =
    if on then
      irq(false)
      irq(true)
    else
      irq(false)
    irqActive = on
  end triggerIRQ

  private def commandCompleted(sendIrq:Boolean,nextState:State = CommandStatus): Unit =
    if sendIrq && (dmaIrqRegister & PATTERN_IRQ_MASK) != 0 then
      triggerIRQ(true)

    state = nextState
  end commandCompleted
  // ================= I/O HANDLING =================================
  final def readData: Int =
    var byte = 0
    state match
      case CommandStatus =>
        if lastError.flag then byte |= (1 << 1)
        byte |= (lastError.drive & 1) << 5
        triggerIRQ(false) // clear interrupt
        state = WaitCommand
      case ResponseSenseBytes =>
        if executingCommand.hasNextSenseStatusByte then
          byte = executingCommand.nextSenseStatusByte()
        if !executingCommand.hasNextSenseStatusByte then
          state = CommandStatus
      case _ =>
    byte
  end readData

  final def readDIPSwitches: Int = dipSwitches.switches
  final def writeData(value:Int): Unit =
    log.info("%s write command data reg = %02X", componentName, value)
    if state == CommandStatus then // a new command has been issued without reading the status of the previous one
      state = WaitCommand
      triggerIRQ(false)
    state match
      case WaitCommand =>
        createCommand(value) match
          case Some(command) =>
            log.info("%s issued command %s, expecting %d bytes", componentName, command.label, command.dcbLen)
            //printf("%s issued command %s, expecting %d bytes\n", componentName, command.label, command.dcbLen)
            pendingCommand = command
            state = ReceivingCommandBytes
          case None =>
            log.warning("%s invalid command %02X",componentName,value)
      case ReceivingCommandBytes =>
//        log.info("%s adding byte %02X to command %s",componentName,value,pendingCommand.label)
//        printf("%s adding byte %02X to command %s\n",componentName,value,pendingCommand.label)
        if pendingCommand.addCMDByte(value) then
          log.info("%s command %s completed %s. Executing ...",componentName,pendingCommand.getCMDBytes.mkString("[",",","]"),pendingCommand.label)
          //printf("%s command %s completed %s. Executing ...\n",componentName,pendingCommand.getCMDBytes.mkString("[",",","]"),pendingCommand.label)
          executingCommand = pendingCommand
          pendingCommand = noCommand
          state = CommandExecution
      case _ =>
        log.warning("%s writing data while in %s state",componentName,state)
  end writeData

  final def readStatus: Int =
    var status = state match
      case Reset =>
        0
      case CommandStatus =>
        if revision3 then
          STATUS_R1_REQ | STATUS_R1_IOMODE | STATUS_R1_BUS | STATUS_R1_BUSY
        else
          STATUS_R1_IOMODE | STATUS_R1_BUS | STATUS_R1_BUSY
      case WaitCommand =>
        STATUS_R1_REQ | STATUS_R1_BUS | STATUS_R1_BUSY
      case ReceivingCommandBytes =>
        // We are still receiving command bytes, so a status register read is generally unexpected.
        // There is one command that stops and checks the status register after sending the DBC,
        // Initialize Drive Characteristics. It looks for the 0b0000_1001 in the status register
        // before sending the 8 remaining bytes of the command.
        //
        // All other reads of the status register during the command receive phase should be invalid.
        if pendingCommand.id == CMD_INIT_DRIVE_CHAR && pendingCommand.getCMDByteSize == 5 && !pendingCommand.getInitDriveCharsFlag then
          pendingCommand.setInitDriveCharsFlag()
          STATUS_R1_BUSY
        else
          STATUS_R1_REQ | STATUS_R1_BUSY
      case CommandExecution =>
        STATUS_R1_BUS | STATUS_R1_BUSY
      case ResponseSenseBytes =>
        STATUS_R1_REQ | STATUS_R1_IOMODE | STATUS_R1_BUSY
    if irqActive then
      status |= STATUS_R1_INT
    status
  end readStatus

  final def controllerReset(): Unit =
    resetState()
  final def generateControllerSelectPulse(value:Int): Unit =
    log.info("%s generate controller select pulse %02X", componentName, value)
  final def writePatternDmaIrq(value:Int): Unit =
    dmaIrqRegister = value
    state = WaitCommand
    log.info("%s write pattern DMA/IRQ register %02X", componentName, value)
  // ============== DMA HANDLING ====================================
  override def dack(): Unit =
    dma.DREQ(active = false, channel = dmaChannel, this)
  override def dmaRead(): Int =
    executingCommand.setDMAReady()
    if executingCommand.dataQueueSize > 0 then
      executingCommand.dataDequeue() & 0xFF
//      println(s"DMA read ${r.toHexString} remaining in queue ${executingCommand.dataQueueSize}")
//      r
    else
      log.error("%s DMA reading from an empty queue. Command is %s", componentName, executingCommand.label)
      0
  override def dmaWrite(value: Int): Unit =
    executingCommand.dataWriteEnqueue(value.toByte)
    //printf("DMA write byte %02X (%d)\n",value,executingCommand.dataQueueSize)
    executingCommand.setDMAReady()
  override def tc(): Unit = executingCommand.terminalCount()
  // ================================================================

  // ============= COMMANDS =========================================
  private def commandUnimplemented(cmd:Command): Unit =
    log.warning("Command %s not implemented DCB=%s",cmd.label,cmd.getDCB)
    printf("Command %s not implemented DCB\n=%s",cmd.label,cmd.getDCB)
    sys.exit(1)

  private def commandRAMDiagnostic(cmd:Command): Unit =
    log.info("%s RAM diagnostic executed",componentName)
    commandCompleted(sendIrq = true)

  private def commandControllerInternalDiagnostic(cmd: Command): Unit =
    log.info("%s controller internal diagnostic executed", componentName)
    commandCompleted(sendIrq = true)

  private def commandDriveDiagnostic(cmd: Command): Unit =
    log.info("%s drive diagnostic executed", componentName)
    commandCompleted(sendIrq = true)

  private def commandRecalibrate(cmd: Command): Unit =
    val dcb = cmd.getDCB
    val drive = drives(dcb.drive)
    cmd.state match
      case 0 =>
        log.info("%s recalibrate drive %d executing ...", componentName,dcb.drive)
        if drive.getCurrentTrack == 0 then
          cmd.state = 2
        else
          cmd.waitCycles(trackStepMillis)
          drive.moveOnTrack(0)
          cmd.state = 1
      case 1 =>
        if cmd.decWaitCycles() then
          if drive.stepTrack() then
            cmd.state = 2
          else
            cmd.waitCycles(trackStepMillis)
      case 2 =>
        commandCompleted(sendIrq = true)

  private def commandTestDriveReady(cmd: Command): Unit =
    val drive = cmd.getDCB.drive
    log.info("%s test drive %d ready", componentName,drive)
    if drive < numberOfDrives then
      lastError.setError(NO_ERROR,drive)
    else
      lastError.setError(ERROR_NOT_READY,drive)

    commandCompleted(sendIrq = true)

  private def commandSenseStatus(cmd:Command): Unit =
    cmd.setSenseStatusBytes(
      (lastError.errorType & 3) << 4 | (lastError.errorCode & 0xF),     // byte 0
      (lastError.drive & 1) <<  5 | lastError.dcb.head & 0x1F,          // byte 1
      (lastError.dcb.track & 0x700) >> 3 | lastError.dcb.sector & 0x1F, // byte 2
      lastError.dcb.track & 0xFF                                        // byte 3
    )
    log.info("%s sending sense bytes status",componentName)
    commandCompleted(sendIrq = true, nextState = ResponseSenseBytes)
  end commandSenseStatus

  private def commandInitializeDriveCharacteristics(cmd:Command): Unit =
    val bytes = cmd.getCMDBytes
    driveChars.maxTracks = bytes(5) << 8 | bytes(6)
    driveChars.maxHeads = bytes(7)
    log.info("%s initialized drive characteristics: maxTracks=%d maxHeads=%d",componentName,driveChars.maxTracks,driveChars.maxHeads)
    //printf("%s initialized drive characteristics: maxTracks=%d maxHeads=%d\n",componentName,driveChars.maxTracks,driveChars.maxHeads)

    commandCompleted(sendIrq = true)
  end commandInitializeDriveCharacteristics

  private def commandWriteBuffer(cmd:Command): Unit =
    cmd.state match
      case 0 =>
        log.info("%s write to buffer executing ...",componentName)
        //printf("%s write to buffer executing ...\n",componentName)
        cmd.state = 1
        cmd.setWriteReady(1024,data => {
          //printf("Write data buffer received %d",data.length)
          System.arraycopy(data,0,sectorBuffer,0,data.length)
        })
      case 1 =>
        if cmd.isTerminalCount then
          cmd.state = 2
      case 2 =>
        log.info("%s write to buffer completed",componentName)
        //printf("%s write to buffer completed\n",componentName)
        commandCompleted(sendIrq = true)
  end commandWriteBuffer

  private def goToNextSector(dcb:DCB,geometry:DiskGeometry): Unit =
    import dcb.*
    import geometry.*
    if sector < sectorsPerTrack - 1 then
      sector += 1
    else if head < heads - 1 then
      head += 1
      sector = 0
    else if track < tracks - 1 then
      track += 1
      head = 0
      sector = 0
    else
      track = tracks
      head = 0
      sector = 0
  end goToNextSector

  private def commandReadWrite(cmd:Command): Unit =
    val dcb = cmd.getDCB
    val drive = drives(dcb.drive)

    cmd.state match
      case 0 =>
        if cmd.id == CMD_READ then
          log.info("%s read executing on drive %d track %d head % d sector %d...",componentName,dcb.drive,dcb.track,dcb.head,dcb.sector)
          //printf("%s read executing on drive %d track %d head %d sector %d...\n",componentName,dcb.drive,dcb.track,dcb.head,dcb.sector)
        else
          log.info("%s write executing on drive %d track %d head % d sector %d...",componentName,dcb.drive,dcb.track,dcb.head,dcb.sector)
          //printf("%s write executing on drive %d track %d head %d sector %d...\n",componentName,dcb.drive,dcb.track,dcb.head,dcb.sector)
        if dcb.drive < numberOfDrives then
          drive.setMotorOn(true)
          drive.setHead(dcb.head)
          if drive.getCurrentTrack != dcb.track then
            drive.moveOnTrack(dcb.track)
            cmd.waitCycles(trackStepMillis)
          else
            cmd.waitCycles(0)
          cmd.state = 1
        else
          lastError.setError(ERROR_NOT_READY,dcb.drive)
          commandCompleted(sendIrq = true)
      case 1 => // seek track
        if cmd.decWaitCycles() then
          if drive.stepTrack() then
            //println(s"Track ${drive.getCurrentTrack} stepped")
            // ok, track reached
            cmd.state = cmd.id match
              case CMD_READ => 2
              case CMD_WRITE => 3
          else
            cmd.waitCycles(trackStepMillis)
      case 2 => // read seek sector
        if cmd.isTerminalCount then // all bytes transferred?
          cmd.state = 4
        else
          if drive.getSector == dcb.sector && cmd.dataQueueSize == 0 then
            val bytes = drive.readSector(dcb.track,dcb.head,dcb.sector).toArray

            System.arraycopy(bytes,0,sectorBuffer,0,bytes.length)
            //println(s"Reading from track ${drive.getCurrentTrack} head ${drive.getHead} sector ${drive.getSector}")
            cmd.dataReadEnqueue(bytes)
            // ok, go to next sector
            val lastTrack = dcb.track
            val lastHead = dcb.head
            goToNextSector(dcb,drive.geometry)
            if lastHead != dcb.head then
              //println("Head changed while reading...")
              drive.setHead(dcb.head)
            if lastTrack != dcb.track then
              //println("Track changed while reading...")
              drive.moveOnTrack(dcb.track)
              cmd.waitCycles(trackStepMillis)
              cmd.state = 1
      case 3 => // write seek sector
        if cmd.isTerminalCount then // all bytes transferred?
          cmd.state = 4
        else
          if drive.getSector == dcb.sector && !cmd.isWriteReady then
            //println(s"Writing to track ${drive.getCurrentTrack} head ${drive.getHead} sector ${drive.getSector}")

            val targetDCB = DCB(track = dcb.track,head = dcb.head,sector = dcb.sector)
            cmd.setWriteReady(DiskImage.SECTOR_SIZE,data => {
              //println(s"Writing data to sector ${drive.getSector} ...")
              System.arraycopy(data,0,sectorBuffer,0,data.length)
              drive.writeSector(targetDCB.track,targetDCB.head,targetDCB.sector,data)
              cmd.setWriteNotReady()
            })
            // ok, go to next sector
            val lastTrack = dcb.track
            val lastHead = dcb.head
            goToNextSector(dcb, drive.geometry)
            if lastHead != dcb.head then
              drive.setHead(dcb.head)
            if lastTrack != dcb.track then
              //println("Track changed while writing...")
              drive.moveOnTrack(dcb.track)
              cmd.waitCycles(trackStepMillis)
              cmd.state = 1
      case 4 =>
          if cmd.id == CMD_READ then
            log.info("%s read sector completed", componentName)
            //printf("%s read sector completed\n", componentName)
          else
            log.info("%s write sector completed", componentName)
            //printf("%s write sector completed\n", componentName)
          drives(dcb.drive).setMotorOn(false)
          commandCompleted(sendIrq = true)
  end commandReadWrite

  private def commandReadyVerify(cmd: Command): Unit =
    val drive = cmd.getDCB.drive
    log.info("%s ready verify %d", componentName, drive)
    if drive < numberOfDrives then
      lastError.setError(NO_ERROR,drive)
    else
      lastError.setError(ERROR_NOT_READY,drive)

    commandCompleted(sendIrq = true)
  end commandReadyVerify

  private def commandFormatDrive(cmd:Command): Unit =
    val dcb = cmd.getDCB
    log.info("%s format drive #%d command...",componentName,dcb.drive)
    val drive = drives(dcb.drive)
    val geo = drive.geometry
    val dataBuffer = if (dcb.controlField & 0x20) != 0 then sectorBuffer else Array.fill[Byte](DiskImage.SECTOR_SIZE)(0x6C)
    for h <- 0 until geo.heads do
      for t <- 0 until geo.tracks do
        drive.moveOnTrack(t)
        while !drive.stepTrack() do {}
        for s <- 0 until geo.sectorsPerTrack do
          drive.formatTrackSector(h,s,dataBuffer)
          log.info("%s formatted track %d head %d sector %d",componentName,t,h,s)
    commandCompleted(sendIrq = true)
  end commandFormatDrive

  // =================================================================
  // TODO check clock frequency
  // =================== Clock 8Mhz ===========================
  final def clock(_cycles: Int): Unit =
    var cycles = _cycles

    // rotate disks
    var d = 0
    while d < 2 do
      drives(d).rotate()
      d += 1

    state match
      case CommandExecution =>
        // Check DMA
        if (dmaIrqRegister & PATTERN_DMA_MASK) != 0 then
          executingCommand.id match
            case CMD_READ =>
              if executingCommand.dataQueueSize > 0 && executingCommand.isDMAReady then
                executingCommand.setDMABusy()
                dma.DREQ(active = true,channel = dmaChannel,this)
            case CMD_WRITE_TO_BUFFER | CMD_WRITE =>
              if executingCommand.isWriteReady && executingCommand.isDMAReady then
                executingCommand.setDMABusy()
                dma.DREQ(active = true,channel = dmaChannel,this)
            case _ =>
        executingCommand.command(executingCommand)
      case _ =>

    cycles -= 1

  end clock
