package ucesoft.disitaco.video

import ucesoft.disitaco.{Config, PCComponent}
import ucesoft.disitaco.io.IOHandler

import java.awt.Dimension

/**
 * @author Alessandro Abbruzzetti
 *         Created on 28/03/2025 19:05  
 */
class MDA extends VideoCard6845:
  override protected val componentName = "MDA"
  protected val ram = Array.ofDim[Byte](4 * 1024)
  private final val charRom = Config.getMdaCgaCharROM
  // COLOR PALETTE =======================================
  protected inline val BACKGROUND_COLOR = 0xFF000000
  protected inline val FOREGROUND_COLOR = 0xFF189929
  protected inline val FOREGROUND_INTENSITY_COLOR = 0xFF6FEA1A

  private inline val TEXT_CHAR_WIDTH = 9
  private inline val TEXT_CHAR_HEIGHT = 14

  protected var lastBit = false
  private var ram_base_address = 0

  override protected def getModeListenerNotification: String = s"MDA TEXT MODE (${regs(1)} x $getVisibleTextRows)"

  override def getProperties: List[PCComponent.Property] = PCComponent.Property("Last bit",lastBit.toString) :: super.getProperties

  // ========================= Borders & clips ================================
  override protected def getHSyncOffset: Int = 7
  override protected def getYSyncOffset: Int = 0
  // ==========================================================================
  override protected def getCharWidth: Int = 9
  override protected def getInitialRegValues: Array[Int] = Array(
    /* Text mode (MDA) */
    // Each character is 9 pixels
    // Each row is 14 scanlines
    0x61,	// Horizontal total:          97 characters ('minus one', 98*9 = 882 pixels)
    0x50,	// Horizontal displayed:      80 characters (80*9 = 720 pixels)
    0x52,	// Horizontal sync position:  82 characters (82*9 = 738 pixels)
    0x0F,	// Horizontal sync width:     15 characters (15*9 = 135 pixels)
    0x19,	// Vertical total:            25 rows ('minus one': 26*14 = 364 scanlines)
    0x06,	// Vertical total adjust:     6 scanlines (364+6 = 370)
    0x19,	// Vertical displayed:        25 rows (25*14 = 350 scanlines)
    0x19,	// Vertical sync position:    25 rows (25*14 = 350 scanlines)
    0x02,	// Interlace mode:            2
    0x0D,	// Maximum scan line address: 13 scanlines ('minus one', set character height to 14)
    0x0B,	// Cursor start:              11 scanlines
    0x0C	// Cursor end:                12 scanlines
  )
  // ========================= Card Info ======================================
  override def getPixelClockFrequencyHz: Double = 16_257_000
  override def getCardInfo: VideoCard.CardInfo = VideoCard.CardInfo(ram,mainMemoryOffset = 0xB_0000,dipSwitch54 = 0b11,supportColors = false)
  override def getPreferredSize: Dimension = new Dimension(882,370)
  // ========================= Drawing ========================================
  private def fetchGFXAndAttrs(): Unit =
    val ram = this.ram
    val xpos = getXPos
    if xpos == 0 then
      ram_base_address = vma
    gfxBuffer(xpos) = ram(vma) & 0xFF
    vma += 1
    attrBuffer(xpos) = ram(vma) & 0xFF
    vma += 1
  end fetchGFXAndAttrs

  override final protected def drawTextCharLine(vsync: Boolean, videoEnabled: Boolean): Int =
    val bitmapOffset = getRasterLine * screenWidth
    val bitmap = this.bitmap
    val blinkModeEnabled = isBlinkingModeEnabled
    val charWidth = TEXT_CHAR_WIDTH
    val rightBorderPix = getBorderWidth + regs(1) * charWidth
    var colPix = getRasterCharPos * charWidth
    val currentCharScanLine = getCurrentCharScanLine
    var xpos = getXPos

    val hborder = colPix < getBorderWidth || colPix >= rightBorderPix
    if vsync || !videoEnabled || hborder then
      var x = 0
      while x < charWidth && colPix < screenWidth do
        // check used to avoid index error when screen height is adjusting
        if (bitmapOffset + colPix < bitmap.length) bitmap(bitmapOffset + colPix) = BACKGROUND_COLOR
        x += 1
        colPix += 1
    else
      val firstRowLine = currentCharScanLine == 0
      if firstRowLine then
        fetchGFXAndAttrs()
        
      var reverse = false
      var blink = false
      var underline = false
      var fg = FOREGROUND_COLOR

      val charCode = gfxBuffer(xpos)
      val attr = attrBuffer(xpos)
      val cursorMode = regs(10) & 0x60
      val cursorTopLine = regs(10) & 0x1F
      val cursorBottomLine = regs(11) & 0x1F
      val showCursor = ram_base_address + (xpos << 1) == (getCursorPos << 1)

      val bf = (attr & 0x70) >> 1 | (attr & 7)

      /*
        Attibute:
        | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
        =================================
        |BL | BR| BG| BB| I | FR| FG| FB|
        BL = Blink
        B(RGB) = background
        I = intensity
        F(RGB) = foreground

        | BRGB | FRGB | Function        |
        =================================
        |  000 |  000 | non-display     |
        |  000 |  001 | underline       |
        |  000 |  111 | normal          |
        |  111 |  000 | reverse         |
      */
      blink = (attr & 0x80) != 0 && blinkModeEnabled
      if (attr & 0x8) != 0 then fg = FOREGROUND_INTENSITY_COLOR

      bf match
        case 0b000_000 => fg = BACKGROUND_COLOR
        case 0b000_001 => underline = currentCharScanLine == TEXT_CHAR_HEIGHT - 1
        case 0b000_111 =>
        case 0b111_000 => reverse = true
        case _ =>

      if showCursor then
        val isCursorLine = 
          if cursorTopLine == 0 && cursorBottomLine == 0 then false
          else if cursorTopLine <= cursorBottomLine then
            currentCharScanLine >= cursorTopLine && currentCharScanLine <= cursorBottomLine
          else
            currentCharScanLine < cursorBottomLine || currentCharScanLine > cursorTopLine
        
        reverse ^= isCursorLine
        if isCursorLine then reverse ^= isCursorOn // cursor blinking

      // the lines 8-13 have an offset of 2K
      val char_ptr = (charCode << 3) + (if currentCharScanLine < 8 then 0 else 2040) + currentCharScanLine
      val charBitmap = charRom(char_ptr).toInt
      var showChar = true

      if (blink) showChar ^= charBlinkOn

      var bitmapPtr = bitmapOffset + colPix
      var gfx = charBitmap
      var x = 0
      // Although the characters are 9 pixels wide, the bitmaps in the ROM are only 8 pixels.
      // For characters C0h-DFh, the ninth pixel column is a duplicate of the eighth; for others, it's blank.
      val codeC0DF = charCode >= 0xC0 && charCode < 0xE0
      lastBit = false

      while x < TEXT_CHAR_WIDTH && colPix < rightBorderPix do
        val gfxBit = if x < 8 then
          (gfx & 0x80) != 0
        else if codeC0DF then lastBit else false

        val bit = showChar && (gfxBit || underline)
        lastBit = bit ^ reverse
        val color = if lastBit then fg else BACKGROUND_COLOR

        gfx <<= 1

        // check used to avoid index error when screen height is adjusting
        if bitmapPtr < bitmap.length then
          bitmap(bitmapPtr) = color

        bitmapPtr += 1
        colPix += 1
        x += 1
      end while
      xpos += 1
    end if
    xpos
  end drawTextCharLine

  override protected def drawBitmapCharLine(videoEnabled:Boolean): Unit = {/* bitmap N/A */}
  // ========================= IN/OUT =========================================
  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(0x3B0 to 0x3BA,this)
  override def in8(port: Int): Int =
    port match
      case 0x3B1 | 0x3B3 | 0x3B5 | 0x3B7 =>
        read6845DataReg
      case 0x3BA => // CRT status
        // Bit 3: Video. This is 1 if a green or bright green pixel is being drawn on the screen at this moment.
        // Bit 0: Retrace. This is 1 if the horizontal retrace is active.
        var status = 0xF0 // On a real IBM MDA, bits 7-4 are always 1, and bits 2-1 are always 0
        if isHBlank then status |= 1
        if lastBit then
          status |= 0x8
        status
      case _ =>
        log.warning("MDA I/O reading from a %d",port)
        0xFF
  override def out8(port: Int, value: Int): Unit =
    port match
      case 0x3B0 | 0x3B2 | 0x3B4 | 0x3B6 => // CRTC address register
        write6845AddressReg(value)
      case 0x3B1 | 0x3B3 | 0x3B5 | 0x3B7 => // CRTC data register
        write6845DataReg(value)
      case 0x3B8 => // Mode Control Register
        // Bit 5: 1 to enable blinking, 0 to disable it: If bit 5 is 1, characters with attribute bit 7 set will blink. If not, they will have high intensity background.
        // Bit 3: 1 to enable video output, 0 to disable it.
        // Bit 1: 1 for black and white.
        // Bit 0: 1 for high resolution mode.
        textModeBlinkModeEnabled((value & 0x20) != 0)
        setVideoEnabled((value & 0x09) == 0x09)
        log.info("MDA mode control register: %02X [video enabled=%b]",value,(value & 0x09) == 0x09)
      case _ =>
        log.warning("MDA writing unknown port: %d = %d",port,value)

