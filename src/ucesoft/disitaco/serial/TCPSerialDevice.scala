package ucesoft.disitaco.serial

import org.apache.commons.net.telnet.TelnetClient
import ucesoft.disitaco.chips.INS8250
import ucesoft.disitaco.chips.INS8250.SerialMaster

import java.io.{BufferedInputStream, InputStream, OutputStream}
import java.net.Socket
import javax.swing.SwingUtilities
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/06/2025 14:27  
 */
class TCPSerialDevice(telnet:Boolean = true) extends INS8250.SerialDevice:
  override val name: String = "TCP Serial Device"
  private var connectionString = ""
  private var master: SerialMaster = uninitialized
  private var out : OutputStream = uninitialized
  private var in : InputStream = uninitialized
  private val client = new TelnetClient()

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
      if client == null then
        in.close()
        out.close()
      else
        client.disconnect()
    try
      if telnet then
        client.connect(host,port)
        out = client.getOutputStream
        in = new BufferedInputStream(client.getInputStream)
      else
        val socket = new Socket(host,port)
        out = socket.getOutputStream
        in = socket.getInputStream
      //println(s"Connected to $connectionString")
      SwingUtilities.invokeLater(() => master.setState(s"Connected to $connectionString"))
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
    master.setState("Disconnected")

