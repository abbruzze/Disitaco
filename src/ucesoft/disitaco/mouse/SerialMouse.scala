package ucesoft.disitaco.mouse

import ucesoft.disitaco.PCComponent
import ucesoft.disitaco.chips.INS8250

import java.awt.{Point, Robot, Toolkit}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener, MouseWheelEvent, MouseWheelListener, WindowAdapter, WindowEvent}
import javax.swing.{JComponent, SwingUtilities}
import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/05/2025 20:01  
 */
class SerialMouse(target:JComponent,mouseCaptureOffAction:() => Unit,logitech3Buttons:Boolean = false) extends PCComponent with INS8250.SerialDevice with MouseListener with MouseMotionListener with MouseWheelListener:
  override protected val componentName = "SerialMouse"
  override val name = "Serial Mouse"

  private inline val QUEUE_THRESHOLD = 24

  private var master : INS8250.SerialMaster = uninitialized
  private val pendingBytes = new mutable.Queue[Int]()
  private var lastMouseEvent : MouseEvent = uninitialized
  private var RTS = false
  private var DTR = false

  private var scaleX = 1.0
  private var scaleY = 1.0

  private val robot = new Robot()
  private var captureOn = false
  private var componentActive = true
  private val emptyCursor = {
    val cursor = new java.awt.image.BufferedImage(16, 16, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    Toolkit.getDefaultToolkit.createCustomCursor(cursor, new Point(0, 0), "null")
  }
  private val windowAdapter = new WindowAdapter:
    override def windowActivated(e: WindowEvent): Unit =
      componentActive = true
      target.setCursor(emptyCursor)

    override def windowDeactivated(e: WindowEvent): Unit =
      componentActive = false
      target.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))

  def setScaleXY(sx:Double,sy:Double): Unit =
    scaleX = sx
    scaleY = sy

  def setCapture(on: Boolean): Unit =
    captureOn = on
    target.removeMouseListener(this)
    target.removeMouseMotionListener(this)
    target.removeMouseWheelListener(this)

    if on then
      target.setCursor(emptyCursor)
      SwingUtilities.getWindowAncestor(target).addWindowListener(windowAdapter)
      target.addMouseListener(this)
      target.addMouseMotionListener(this)
      target.addMouseWheelListener(this)
    else
      target.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
      SwingUtilities.getWindowAncestor(target).removeWindowListener(windowAdapter)
  end setCapture

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

    var x = ((e.getX - lastMouseEvent.getX) * scaleX).toInt
    var y = ((e.getY - lastMouseEvent.getY) * scaleY).toInt

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
    if DTR && RTS && pendingBytes.size < QUEUE_THRESHOLD then
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
        val mb = if cbPressed then 0x10 else 0
        val w = e match
          case we:MouseWheelEvent =>
            var mov = we.getWheelRotation
            if mov < -8 then mov = -8
            else if mov > 7 then mov = 7
            mov
          case _ => 0

        val byte4 = mb | w & 0xF
        sendByte(byte4)
  end makeEvent

  override def mouseClicked(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    if SwingUtilities.isMiddleMouseButton(e) && e.isControlDown then
      setCapture(false)
      mouseCaptureOffAction()
  override def mousePressed(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseReleased(e: MouseEvent): Unit =
    makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    val center = target.getLocationOnScreen
    val dim = target.getSize
    center.x += dim.width / 2
    center.y += dim.height / 2
    robot.mouseMove(center.x, center.y)

    lastMouseEvent = null
  override def mouseDragged(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    makeEvent(lbPressed = SwingUtilities.isLeftMouseButton(e),rbPressed = SwingUtilities.isRightMouseButton(e),cbPressed = SwingUtilities.isMiddleMouseButton(e),e)
  override def mouseMoved(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
  override def mouseWheelMoved(e: MouseWheelEvent): Unit =
    makeEvent(lbPressed = false,rbPressed = false,cbPressed = false,e)
