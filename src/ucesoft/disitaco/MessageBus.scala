package ucesoft.disitaco

import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/01/2024 16:48  
 */
object MessageBus:
  sealed trait Message:
    val source: AnyRef

  // ======== Messages ==========================
  case class VideoModeChanged(override val source:AnyRef,description:String,width:Int,height:Int) extends Message
  case class DisplaySizeChanged(override val source:AnyRef,widthFactor:Double,heightFactor:Double) extends Message
  case class WarpMode(override val source:AnyRef,enabled:Boolean) extends Message
  case class Shutdown(override val source:AnyRef) extends Message
  // ============================================
  trait MessageListener:
    def onMessage(msg:Message): Unit

  private val listeners = new ListBuffer[MessageListener]

  def add(l:MessageListener): Unit =
    if !listeners.contains(l) then
      listeners += l
  def remove(l:MessageListener): Unit =
    listeners -= l

  def send(msg:Message): Unit =
    for l <- listeners.toList do
      l.onMessage(msg)

