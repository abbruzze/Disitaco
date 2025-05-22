package ucesoft.disitaco.mouse

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.chips.INS8250

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import javax.swing.SwingUtilities
import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 20:01  
 */
class SerialMouse extends PCComponent with INS8250.SerialDevice with MouseListener with MouseMotionListener:
  override protected val componentName = "SerialMouse"
  override val name = "Serial Mouse"

  private var master : INS8250.SerialMaster = uninitialized
  private var bitCount = 0
  private var sending = false
  private val pendingBytes = new mutable.Queue[Int]()
  private var lastMouseEvent : MouseEvent = uninitialized
  private var RTS = false
  private var DTR = false

  override protected def reset(): Unit =
    bitCount = 0
    sending = false
    pendingBytes.clear()

  override def setMaster(master: INS8250.SerialMaster): Unit =
    this.master = master

  override def dtr(on: Boolean): Unit =
    if on then
      sendByte('M'.toInt)
    DTR = on

  override def rts(on: Boolean): Unit =
    if on then master.cts(on = true)
    RTS = on
    if !RTS then // it should be 100ms...
      reset()

  private def sendByte(byte:Int): Unit =
    if RTS && DTR then
      sending = true
      bitCount = 0
      pendingBytes.enqueue(byte)

  override def tick(byteLen: Int): Unit =
    if sending then
      bitCount += 1
      if bitCount == byteLen then
        bitCount = 0
        master.setRXByte(pendingBytes.dequeue())
        sending = pendingBytes.nonEmpty
  end tick

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
    val deltaXY = getDeltaXY(e)
    val x = deltaXY & 0xFF
    val y = (deltaXY >> 8) & 0xFF
    val lb = if lbPressed then 0x20 else 0
    val rb = if rbPressed then 0x10 else 0
    val byte1 = 0x40 | lb | rb | (y >> 6) & 3 | (x >> 6) & 3
    val byte2 = x & 0x3F
    val byte3 = y & 0x3F
    sendByte(byte1)
    sendByte(byte2)
    sendByte(byte3)
  end makeEvent

  override def mouseClicked(e: MouseEvent): Unit = {}
  override def mousePressed(e: MouseEvent): Unit = makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseReleased(e: MouseEvent): Unit = makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}
  override def mouseDragged(e: MouseEvent): Unit = makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseMoved(e: MouseEvent): Unit = makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
