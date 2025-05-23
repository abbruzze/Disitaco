package ucesoft.disitaco.mouse

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.chips.INS8250

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import javax.swing.{JComponent, SwingUtilities}
import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 20:01  
 */
class SerialMouse(target:JComponent,logitech3Buttons:Boolean = false) extends PCComponent with INS8250.SerialDevice with MouseListener with MouseMotionListener:
  override protected val componentName = "SerialMouse"
  override val name = "Serial Mouse"

  private var master : INS8250.SerialMaster = uninitialized
  private val pendingBytes = new mutable.Queue[Int]()
  private var lastMouseEvent : MouseEvent = uninitialized
  private var RTS = false
  private var DTR = false

  def enable(enabled:Boolean): Unit =
    target.removeMouseListener(this)
    target.removeMouseMotionListener(this)
    if enabled then
      target.addMouseListener(this)
      target.addMouseMotionListener(this)

  override protected def reset(): Unit =
    pendingBytes.clear()

  override def setMaster(master: INS8250.SerialMaster): Unit =
    this.master = master

  private def checkMouseOn(): Unit =
    if DTR && RTS then
      sendByte('M'.toInt)
      if logitech3Buttons then
        sendByte('Z'.toInt)

  override def dtr(on: Boolean): Unit =
    if on then master.dsr(on = true)
    DTR = on
    checkMouseOn()

  override def rts(on: Boolean): Unit =
    if on then master.cts(on = true)
    RTS = on
    checkMouseOn()

  private def sendByte(byte:Int): Unit =
    pendingBytes.enqueue(byte)

  override def checkRXByte(): Unit =
    if pendingBytes.nonEmpty then
      master.setRXByte(pendingBytes.dequeue())
  end checkRXByte

  private def getDeltaXY(e: MouseEvent): Int =
    if lastMouseEvent == null then
      lastMouseEvent = e

    var x = e.getX - lastMouseEvent.getX
    var y = e.getY - lastMouseEvent.getY
    if x > 127 then x = 127
    else if x < -128 then x = -128
    if y > 127 then y = 127
    else if y < -128 then y = -128

    lastMouseEvent = e

    x  & 0xFF | (y & 0xFF) << 8
  end getDeltaXY

  /*
              D7      D6      D5      D4      D3      D2      D1      D0

    Byte 1    X       1       LB      RB      Y7      Y6      X7      X6
    Byte 2    X       0       X5      X4      X3      X2      X1      X0
    Byte 3    X       0       Y5      Y4      Y3      Y2      Y1      Y0

    LB is the state of the left button (1 means down)
    RB is the state of the right button (1 means down)
    X7-X0 movement in X direction since last packet (signed byte)
    Y7-Y0 movement in Y direction since last packet (signed byte)
  */
  private def makeEvent(lbPressed:Boolean,rbPressed:Boolean,cbPressed:Boolean,e:MouseEvent): Unit =
    if DTR && RTS then
      val deltaXY = getDeltaXY(e)
      val x = deltaXY & 0xFF
      val y = (deltaXY >> 8) & 0xFF
      val lb = if lbPressed then 0x20 else 0
      val rb = if rbPressed then 0x10 else 0
      val byte1 = 0x40 | lb | rb | (y & 0xC0) >> 4 | (x >> 6) & 3
      val byte2 = x & 0x3F
      val byte3 = y & 0x3F
      sendByte(byte1)
      sendByte(byte2)
      sendByte(byte3)
      if logitech3Buttons then
        val mb = if cbPressed then 0x20 else 0
        val byte4 = mb
        sendByte(byte4)
  end makeEvent

  override def mouseClicked(e: MouseEvent): Unit = {}
  override def mousePressed(e: MouseEvent): Unit = makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseReleased(e: MouseEvent): Unit = makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}
  override def mouseDragged(e: MouseEvent): Unit = makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseMoved(e: MouseEvent): Unit = makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
