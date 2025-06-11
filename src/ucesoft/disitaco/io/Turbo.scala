package ucesoft.disitaco.io

import ucesoft.disitaco.MessageBus

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 15:07  
 */
class Turbo(port:Int) extends IODevice:
  override protected val componentName = "Turbo"
  
  log.info("Turbo port %d configured",port)

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(List(port),this)

  override def in8(port: Int): Int =
    MessageBus.send(MessageBus.WarpMode(this,false))
    0xFF

  override def out8(port: Int, byte: Int): Unit =
    MessageBus.send(MessageBus.WarpMode(this,true))
