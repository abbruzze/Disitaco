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

      None
    catch
      case e: Exception =>
        println(s"Error while connecting to $connectionString: $e")
        Some(e.toString)
  end connect

  override def setTXByte(byte: Int): Unit =
    if out != null then
      try
        out.write(byte)
        out.flush()
      catch
        case _:Throwable =>

  override def checkRXByte(): Unit =
      try
        if in != null && in.available() > 0 then
          val byte = in.read()
          master.setRXByte(byte)
      catch
        case _: Throwable =>

  override def setMaster(master:SerialMaster): Unit =
    this.master = master

