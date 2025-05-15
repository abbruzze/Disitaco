package ucesoft.disitaco.video

import ucesoft.disitaco.{Display, PCComponent}
import ucesoft.disitaco.debugger.Debugger.VideoFrameListener

import java.awt.Dimension
import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/03/2025 10:35  
 */
abstract class VideoCard6845 extends VideoCard:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/monitor.png"))
  import VideoCard.*

  protected enum DrawMode:
    case TEXT, BITMAP
  end DrawMode

  private inline val debug = true
  private var frameCount = 0

  protected var address_reg = 0
  /*
  * 00/$00 Total number of horizontal character positions
  * 01/$01 Number of visible horizontal character positions
  * 02/$02 Horizontal sync position
  * 03/$03 Horizontal and vertical sync width
  * 04/$04 Total number of screen rows
  * 05/$05 Vertical fine adjustment
  * 06/$06 Number of visible screen rows
  * 07/$07 Vertical sync position
  * 08/$08 Interlace mode control register
  * 09/$09 Number of scan lines per character
  * 10/$OA Cursor mode control
  * ll/$0B Ending scan line for cursor
  * 12/$0C Screen memory starting address (high byte)
  * 13/$0D Screen memory starting address (low byte)
  * 14/$0E Cursor position address (high byte)
  * 15/$0F Cursor position address (low byte)
  * 16/$10 Light pen vertical position
  * 17/$11 Light pen horizontal position
  */
  protected val regs : Array[Int] = Array.ofDim[Int](18)

  /*
      Counter name	              Abbr	      Compared with	  Comment
    Horizontal Character Counter	HCC	        R0, R1, R2	    Increments on every clock cycle
    Horizontal Sync Counter	      HSC	        R3l
    Vertical Character Counter	  VCC	        R4, R6, R7
    Vertical Sync Counter	        VSC	        R3h	            R3h is fixed to 0 on CRTCs 1/2. This gives 16 lines of VSYNC
    Vertical Line Counter	        VLC	        R9, R10, R11	  If not in IVM mode, this counter is exposed on CRTC pins RA0..RA4
    Vertical Total Adjust Counter	VTAC	      R5	            This counter does not exist on CRTCs 0/3/4. C9 is reused instead
    Frame Counter	                FC			                    Used to alternate frames in interlace and for CRTC cursor blinking
    Memory Address	              MA		      R14/R15	        This counter is exposed on CRTC pins MA0..MA13
    */
  private var hcc = 0
  private var hsc = 0
  private var vcc = 0
  private var vsc = 0
  private var vlc = 0
  private var vtac = 0
  private var vtacFlag = false
  protected var vma = 0 // REG 12/13 ram adr

  private var vblank = false
  private var hblank = false
  private var _isInDisplayArea = false

  private var display: Display = uninitialized
  protected var bitmap: Array[Int] = uninitialized
  private var drawMode = DrawMode.TEXT
  private var pendingDrawModeChanged = false
  private var pendingDrawMode = DrawMode.TEXT

  private var updateGeometryOnNextFrame = false

  private var videoEnabled = false

  private var xchars_total = 0 // REG 0 Horizontal Total
  private var ychars_total = 0 // REG 9 Character Total Vertical
  private var cursor_pos = 0 // REG 14-5 Cursor location HI/LO
  private var cursorOn = true
  private var ypos = 0
  private var xpos = 0


  private var rasterLine = 0
  private var currentCharScanLine = 0 // char scan line
  private var visibleScreenHeightPix = 0 // total Y pixels
  private var visibleTextRows = 0 // total rows
  private var borderWidth = 0

  private var textBlinkModeEnabled = false

  private var frameBit = 0
  protected var charBlinkOn = true

  protected var screenHeight = 0
  protected var screenWidth = 0

  protected var hsyncManualPos = 0
  protected var vsyncManualPos = 0

  private var clippingOn = false

  private var modeListener : VideoCardListener  = new VideoCardListener:
    override def modeChanged(mode: String,w:Int,h:Int): Unit = {}
    override def charFrequencyChanged(charFrequencyInHz: Double): Unit = {}

  private var frameListeners : List[VideoFrameListener] = Nil


  protected final val gfxBuffer = Array.ofDim[Int](256) // Max 256 columns
  protected final val attrBuffer = Array.ofDim[Int](256)

  // ==========================================================
  def setClippingOn(on:Boolean): Unit =
    clippingOn = on
    updateGeometry()

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    val props = new ListBuffer[Property]
    props += Property("Video enabled",videoEnabled.toString)
    props += Property("Raster line",rasterLine.toString)
    props += Property("H blank",hblank.toString)
    props += Property("V blank",vblank.toString)
    props += Property("Raster char pos",hcc.toString)
    props += Property("RAM address",vma.toString)
    props += Property("RAM base address",(regs(12) << 8 | regs(13)).toString)
    props += Property("Cursor address",cursor_pos.toString)
    props += Property("H chars total",xchars_total.toString)
    props += Property("H visible chars",visibleTextRows.toString)
    props += Property("V visible lines",visibleScreenHeightPix.toString)
    for (r,i) <- regs.zipWithIndex do
      props += Property(s"Reg $i",r.toString)

    props.toList
  override protected def reset(): Unit =
    hblank = false
    vblank = false
    textBlinkModeEnabled = false
    display.setInterlaceMode(false)
    bitmap = display.displayMem
    val initRegValues = getInitialRegValues
    for i <- regs.indices do
      if i < initRegValues.length then
        address_reg = i
        write6845DataReg(initRegValues(i))
    address_reg = 0

  final def setHSyncManualPos(pos:Int): Unit = hsyncManualPos = pos
  final def setVSyncManualPos(pos:Int): Unit = vsyncManualPos = pos

  override def addVideoFrameListener(vfl: VideoFrameListener): Unit =
    frameListeners ::= vfl
  override def removeVideoFrameListener(vfl: VideoFrameListener): Unit =
    frameListeners = frameListeners.filterNot(_ == vfl)

  private def notifyFrameListeners(): Unit =
    var fl = frameListeners
    while fl.nonEmpty do
      fl.head.newFrame()
      fl = fl.tail

  override def setModeListener(listener: VideoCardListener): Unit = modeListener = listener

  override def setDisplay(display: Display): Unit =
    this.display = display
    bitmap = display.displayMem

  protected def setMode(mode:DrawMode): Unit =
    //    if mode != drawMode then
    //      pendingDrawMode = mode
    //      pendingDrawModeChanged = true
    drawMode = mode

  protected def getMode: DrawMode = drawMode

  protected def isVBlank: Boolean = vblank
  protected def isHBlank: Boolean = hblank
  protected def isInDisplayArea: Boolean = _isInDisplayArea

  protected def getRasterCharPos:Int = hcc
  protected def getCurrentCharScanLine: Int = currentCharScanLine
  protected def getXPos: Int = xpos
  protected def getYPos: Int = ypos
  protected def getRasterLine: Int = rasterLine
  protected def getVisibleTextRows: Int = visibleTextRows
  protected def getBorderWidth: Int = borderWidth
  protected def getXCharsTotal: Int = xchars_total
  protected def getYCharsTotal: Int = ychars_total
  protected def getCursorPos: Int = cursor_pos
  protected def isCursorOn: Boolean = cursorOn

  protected def read6845DataReg: Int =
    address_reg match
      case 14|15|16|17 => regs(address_reg)
      case _ => 0

  protected def write6845AddressReg(value:Int): Unit = address_reg = value & 0x3F
  protected def write6845DataReg(value:Int): Unit =
    if address_reg > 15 then return

    regs(address_reg) = value
    address_reg match
      case 0 => // REG 0 Horizontal Total
        xchars_total = value + 1
        if (debug) println(s"6845: REG 0 Horizontal Total: $xchars_total")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 1 => // REG 1 Horizontal Displayed
        if (debug) println(s"6845: REG 1 Horizontal Displayed: $value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 2 => // REG 2 Horizontal Sync Pos
        if (debug) println(s"6845: REG 2 Horizontal Sync Pos: $value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 3 => // REG 3 Horizontal/Vertical Sync widths
        if (debug) println(s"6845: REG 3 Horizontal/Vertical Sync widths: $value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 4 => // REG 4 Vertical Total
        regs(address_reg) &= 0x7F // (R4) 7 bit write only
        if (debug) println(s"6845: REG 4 Vertical Total :$value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 5 => // REG 5 Vertical Total Fine Adjust
        regs(address_reg) &= 0x1F // (R5) 5 bit write only
        if (debug) println(s"6845: REG 5 Vertical Total Fine Adjust :$value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 6 => // REG 6 Vertical Displayed
        regs(address_reg) &= 0x7F // (R6) 7 bit write only
        if (debug) println(s"6845: REG 6 Vertical Displayed: $value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 7 => // REG 7 Vertical Sync Position
        regs(address_reg) &= 0x7F // (R7) 7 bit write only
        if (debug) println(s"6845: REG 7 Vertical Sync Position: $value")
      case 8 => // REG 8 Interlace
        if (debug) println(s"6845: REG 8 Interlace: $value")
      case 9 => // REG 9 Character Total Vertical
        ychars_total = value & 0x1F // (R9) 5 bit write only
        if (debug) println(s"6845: REG 9 Character Total Vertical: $value")
        //updateGeometry()
        updateGeometryOnNextFrame = true
      case 10 => // R10  Cursor Mode, Start Scan
        if (debug) println(s"6845: R10  Cursor Mode, Start Scan: ${value & 0x1F} mode: ${(value & 0x60) >> 5}")
      case 11 => // R11 Cursor End Scan
        if (debug) println(s"6845 R11 Cursor End Scan: $value")
      case 12|13 => // R12 Display Start Address hi, R13 Display Start Address lo
        if address_reg == 12 then
          regs(address_reg) &= 0x3F // (R12) 6 bit write only
        if (debug) println(s"6845: new Screen Address($address_reg): ${Integer.toHexString(regs(12) << 8 | regs(13))} rasterLine=$rasterLine vblank=$vblank #$frameCount")
      case 14|15 =>  // REG 14-5 Cursor location HI/LO
        cursor_pos = (regs(14) & 0x3F) << 8 | regs(15)
  //if (debug) println(s"6845: new cursor position: $cursor_pos")
  end write6845DataReg

  protected def updateGeometry(): Unit =
    visibleTextRows = regs(6)

    visibleScreenHeightPix = visibleTextRows * (ychars_total + 1)
    val charWidth = getCharWidth

    val htotal = xchars_total
    val hdisplayed = regs(1)
    val hsync = regs(2)
    val hsyncWidth = (regs(3) & 0x0F) - 1

    if htotal == 0 then
      return

    var lborder = htotal - (hsync + hsyncWidth) + getHSyncOffset + hsyncManualPos
    var rborder = htotal - hdisplayed - lborder

    if rborder < 0 then rborder = 0
    if lborder < 0 then lborder = 0

    borderWidth = lborder * charWidth

    val totalLines = (regs(4) + 1) * (ychars_total + 1) + regs(5)

    modeListener.charFrequencyChanged(getPixelClockFrequencyHz / charWidth)

    val newScreenWidth = htotal * charWidth
    if newScreenWidth != screenWidth || totalLines != screenHeight then
      screenWidth = newScreenWidth
      screenHeight = totalLines
      display.setNewResolution(screenHeight, screenWidth)
      bitmap = display.displayMem

      if debug then
        println(s"New screen res. width=$screenWidth height=$screenHeight htotal=$htotal hdisplayed=$hdisplayed hsync=$hsync hsync_width=${(regs(3) & 0x0F) - 1} rborder=$rborder lborder=$lborder raster=$rasterLine")
    end if

    if borderWidth < 0 then
      borderWidth = 0

    if clippingOn then
      val clippedWidth = screenWidth - (lborder + rborder - 2) * charWidth // we show 1 border char on the left and on the right
      display.setClipArea(borderWidth - charWidth, 0, borderWidth - charWidth + clippedWidth, screenHeight)
      modeListener.modeChanged(getModeListenerNotification,clippedWidth,screenHeight)
    else
      modeListener.modeChanged(getModeListenerNotification,screenWidth,screenHeight)
  end updateGeometry

  protected def getModeListenerNotification: String = ""

  protected def latch_addresses(): Unit =
    vma = regs(12) << 8 | regs(13)

  protected def textModeBlinkModeEnabled(enabled:Boolean): Unit = textBlinkModeEnabled = enabled
  protected def isBlinkingModeEnabled: Boolean = textBlinkModeEnabled

  protected def setVideoEnabled(enabled:Boolean): Unit =
    videoEnabled = enabled

  private def nextFrame(): Unit =
    if updateGeometryOnNextFrame then
      updateGeometryOnNextFrame = false
      updateGeometry()

    frameCount += 1
    frameBit ^= 1
    display.showFrame()
    notifyFrameListeners()

    /*
    regs(10)
    bit 6 5
    00: normal blinking (off and on periods equal)
    01: no blinking
    10: no blinking
    11: slower blinking (off period longer than on period)
    */
    // char blinking
    if textBlinkModeEnabled then
      val change = if (regs(10) & 0x20) == 0 then (display.getFrameCounter & 7) == 0 // 1/16
      else (display.getFrameCounter & 0xF) == 0 // 1/32
      if (change) charBlinkOn = !charBlinkOn
    // cursor blinking
    val cursorMode = (regs(10) & 0x60) >> 5
    val cursor_change = if cursorMode == 0 then (display.getFrameCounter & 7) == 0 // 1/16
    else if cursorMode == 3 then (display.getFrameCounter % 0xF) == 0 // 1/32
    else false
    if cursor_change then cursorOn = !cursorOn
  end nextFrame

  protected def vsync(): Unit =
    rasterLine = 0
    nextFrame()

  override final def clockChar(): Unit =
    // Draw current char position ====================================================
    if vblank || vcc >= visibleTextRows then
      drawTextCharLine(vsync = true)
    else
      val offLine = xpos > regs(1)
      drawMode match
        case DrawMode.TEXT =>
          xpos = drawTextCharLine(vsync = false,videoEnabled = videoEnabled && !offLine)
        case DrawMode.BITMAP =>
          drawBitmapCharLine(videoEnabled && !offLine)
    // ===============================================================================
    // advance on next char position
    hcc = (hcc + 1) & 0xFF  // 8 bit

    if hblank then
      hsc = (hsc + 1) & 0xF
      if hsc == (regs(3) & 0xF) then // == hsync width ?
        hblank = false
    end if

    // hblank check
    if hcc == regs(2) then // == hsync position ?
      hblank = true
      hsc = 0
    end if

    _isInDisplayArea = !vblank && !hblank

    if hcc == xchars_total then // row finished ?
      hcc = 0
      xpos = 0

      // NEXT RASTER LINE =====================================================
      rasterLine += 1
      vlc = (vlc + 1) & 0x1F

      if !vblank then
        currentCharScanLine = (currentCharScanLine + 1) & 0x7F
      else
        var vsyncWidth = regs(3) >> 4
        // vsync width is fixed to 16 lines if 0
        if vsyncWidth == 0 then vsyncWidth = 15
        // is vblank ended ?
        if vsc == vsyncWidth then
          vblank = false
          vsync()
          currentCharScanLine = 0
        else
          vsc += 1
      end if

      if currentCharScanLine == ychars_total + 1 then
        currentCharScanLine = 0
        ypos += 1
      // current char row finished ?
      if vlc == ychars_total + 1 then
        vlc = 0
        vcc = (vcc + 1) & 0xFF
        // vsync reached ?
        val vsyncPos = regs(7) + getYSyncOffset + vsyncManualPos
        if vcc == vsyncPos then
          vsc = 0
          vblank = true
        if vcc == regs(4) + 1 then // vertical total reached ?
          vtac = 0
          vtacFlag = true
      end if

      if vtacFlag then
        // we are counting vertical adjust lines
        if vtac == regs(5) then // have we finished ?
          latch_addresses()
          currentCharScanLine = 0
          vtacFlag = false
          vlc = 0
          vcc = 0
          hcc = 0
          vtac = 0
          ypos = 0
        else
          vtac += 1
        end if
      end if
    end if

  end clockChar

  // Abstract methods =========================================
  def getPixelClockFrequencyHz: Double
  def getPreferredSize: Dimension

  protected def getCharWidth: Int

  protected def getHSyncOffset: Int
  protected def getYSyncOffset: Int

  protected def getInitialRegValues: Array[Int]

  protected def drawTextCharLine(vsync:Boolean, videoEnabled:Boolean = true): Int
  protected def drawBitmapCharLine(videoEnabled:Boolean = true): Unit


