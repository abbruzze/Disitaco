package ucesoft.disitaco.serial

import ucesoft.disitaco.chips.INS8250
import ucesoft.disitaco.chips.INS8250.SerialMaster

import java.io.{BufferedInputStream, InputStream, OutputStream}
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/06/2025 14:27  
 */
class TCPSerialDevice extends INS8250.SerialDevice:
  override val name: String = "TCP Serial Device"
  private var connectionString = ""
  private var master: SerialMaster = uninitialized
  private var out : OutputStream = uninitialized
  private var in : InputStream = uninitialized

  master.setState("Disconnected")

  def disconnect(): Unit =
    try
      if in != null then in.close()
    catch
      case _:Throwable =>
    try
      if out != null then out.close()
    catch
      case _: Throwable =>
    master.setState("Disconnected")

  def connect(host:String,port:Int): Option[String] =
    connectionString = s"$host:$port"
    if in != null then
      in.close()
      out.close()
    try
      val socket = new java.net.Socket(host, port)
      out = socket.getOutputStream
      in = new BufferedInputStream(socket.getInputStream)
      println(s"Connected to $connectionString")
      master.setState(s"Connected to $connectionString")
      None
    catch
      case e: Exception =>
        println(s"Error while connecting to $connectionString: $e")
        master.setState(s"Error: $e")
        Some(e.toString)
  end connect

  override def setTXByte(byte: Int): Unit =
    if out != null then
      try
        out.write(byte)
        out.flush()
      catch
        case _:Throwable =>
          master.setState("Error while writing..")

  override def checkRXByte(): Unit =
      try
        if in != null && in.available() > 0 then
          val byte = in.read()
          master.setRXByte(byte)
      catch
        case _: Throwable =>
          master.setState("Error while reading..")

  override def setMaster(master:SerialMaster): Unit =
    this.master = master

