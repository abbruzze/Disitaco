package ucesoft.disitaco.serial

import ucesoft.disitaco.chips.INS8250
import ucesoft.disitaco.chips.INS8250.SerialMaster

import java.io.{BufferedInputStream, File, FileInputStream, FileOutputStream, IOException, InputStream}
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 10/06/2025 10:02  
 */
class HostFileTransferSerialDevice extends INS8250.SerialDevice:
  private enum State:
    case StatusCodeReady, WaitingCommand, WaitingParameters, ReceivingBytes, SendingBytes
  private enum StatusCode:
    case OK, CommandNotFound, LocalDirNotFound, PutError, GetError

  private case class Command(name:String, parametersCount:Int,executeState:State,execute: () => StatusCode):
    private val parameters = new ListBuffer[String]
    private var executed = false
    def isReady: Boolean = parameters.size == parametersCount
    def addParameters(param:String): Unit = parameters += param
    def getParameters : List[String] = parameters.toList
    def reset(): Unit = parameters.clear()
    def isExecuted: Boolean = executed
    def setExecuted(): Unit = executed = true
  end Command

  import State.*
  import StatusCode.*

  override val name = "HostFileTransfer"
  private var master : SerialMaster = uninitialized
  private var state = WaitingCommand
  private val sb = new StringBuilder
  private var cmd : Command = uninitialized
  private var pendingStatusCode = OK
  private var currentDir = new File(System.getProperty("user.dir","./"))
  private var out : FileOutputStream = uninitialized
  private var in : InputStream = uninitialized
  private var sendingCompleted = false
  private var rts = false

  private val commands = Map(
    "chdir" -> Command("chdir",1,WaitingCommand,chdir),
    "put" -> Command("put",1,ReceivingBytes,put),
    "get" -> Command("get",1,SendingBytes,get),
  )

  override def setMaster(master: SerialMaster): Unit = this.master = master

  override def dtr(on: Boolean): Unit =
    if !on then
      state match
        case ReceivingBytes =>
          out.close()
          pendingStatusCode = OK
          state = StatusCodeReady
        case SendingBytes =>
          in.close()
          pendingStatusCode = OK
          state = StatusCodeReady
        case _ =>
  end dtr

  override def rts(on: Boolean): Unit = rts = on

  override def setTXByte(byte: Int): Unit =
    state match
      case WaitingCommand =>
        if byte != 10 then
          sb.append(byte.toChar)
        else
          commands.get(sb.toString()) match
            case Some(c) =>
              cmd = c.copy()
              pendingStatusCode = OK
              state = StatusCodeReady
              println(s"Received command: ${cmd.name}")
            case None =>
              pendingStatusCode = CommandNotFound
              state = StatusCodeReady
          sb.clear()
      case WaitingParameters =>
        if byte != 10 then
          sb.append(byte.toChar)
        else
          cmd.addParameters(sb.toString)
          println(s"Received parameter: ${sb.toString}")
          sb.clear()
          if cmd.isReady then
            executeCommand()
      case ReceivingBytes =>
        out.write(byte)
      case _ =>
  end setTXByte

  private def executeCommand(): Unit =
    pendingStatusCode = cmd.execute()
    state = StatusCodeReady

  override def checkRXByte(): Unit =
    state match
      case StatusCodeReady =>
        master.setRXByte(pendingStatusCode.ordinal)
        if cmd != null && cmd.isReady then
          if cmd.getParameters.isEmpty then
            executeCommand()
          else if pendingStatusCode == OK && !cmd.isExecuted then
            state = cmd.executeState
            cmd.setExecuted()
          else
            state = WaitingCommand
        else if pendingStatusCode == OK then
          state = WaitingParameters
        else
          state = WaitingCommand
      case SendingBytes =>
        if !sendingCompleted && rts then
          val b = in.read()
          if in.available() == 0 then
            master.cts(on = false)
            sendingCompleted = true

          master.setRXByte(b)
      case _ =>

  private def chdir(): StatusCode =
    println(s"Executing chdir command with parameters: ${cmd.getParameters.mkString(",")}")
    val newDir = new File(cmd.getParameters.head)
    if newDir.exists() && newDir.isDirectory then
      currentDir = newDir
      println(s"Changed directory to: ${currentDir.getAbsolutePath}")
      OK
    else
      println(s"Directory does not exist: ${newDir.getAbsolutePath}")
      LocalDirNotFound
  end chdir

  private def put(): StatusCode =
    println(s"Executing put command with parameters: ${cmd.getParameters.mkString(",")}")
    val file = new File(currentDir,cmd.getParameters.head)
    try
      out = new FileOutputStream(file)
      OK
    catch
      case err:IOException =>
        println(s"Cannot write to file $file")
        PutError
  end put

  private def get(): StatusCode =
    println(s"Executing get command with parameters: ${cmd.getParameters.mkString(",")}")
    val file = new File(currentDir, cmd.getParameters.head)
    try
      in = new BufferedInputStream(new FileInputStream(file))
      sendingCompleted = false
      master.cts(on = true)
      OK
    catch
      case err: IOException =>
        println(s"Cannot read file $file")
        GetError