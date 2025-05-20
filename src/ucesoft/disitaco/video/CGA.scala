package ucesoft.disitaco.video

import ucesoft.disitaco.{Config, PCComponent}
import ucesoft.disitaco.io.IOHandler

import java.awt.Dimension

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/04/2025 10:54  
 */
class CGA extends VideoCard6845:
  private enum Resolution:
    case LOW_RES, MEDIUM_RES, HIGH_RES

  import Resolution.*

  private inline val RAM_SIZE = 16 * 1024
  private val ram = Array.ofDim[Byte](RAM_SIZE)
  private inline val RAM_SIZE_MASK = RAM_SIZE - 1
  private final val charRom = Config.getHomeResource("/rom/mda/char_rom.bin").getOrElse(Config.getResource("/resources/rom/IBM_5788005_AM9264_1981_CGA_MDA_CARD.BIN").getOrElse(Array[Byte]()))

  private inline val CHAR_WIDTH = 8
  private inline val CHAR_HEIGHT = 8

  private var ram_base_address = 0

  private var resolution = MEDIUM_RES
  private var _40ColMode = false

  private var local_ram_ptr = 0
  private var hborder = false

  /*
  Two character fonts are used on the Color/Graphics Monitor Adapter: a 7-high by 7-wide double-dot font and a
  7-high by 5-wide single-dot font. The font is selected by a jumper (P3).
  The single-dot font is selected by inserting the jumper; the double-dot font is selected by removing the jumper.
  */
  private var fontROMOffset = 0x1000 + 2048

  // registers
  private var modeControlRegister = 0
  private var colorSelectRegister = 0
  // COLOR PALETTE =======================================
  private final val PALETTE = Array(
    /*00*/0xFF000000, // black
    /*01*/0xFF0000AA, // blue
    /*02*/0xFF00AA00, // green
    /*03*/0xFF00AAAA, // cyan
    /*04*/0xFFAA0000, // red
    /*05*/0xFFAA00AA, // magenta
    /*06*/0xFFAA5500, // brown
    /*07*/0xFFAAAAAA, // white
    /*08*/0xFF555555, // gray
    /*09*/0xFF5555FF, // light blue
    /*10*/0xFF55FF55, // light green
    /*11*/0xFF55FFFF, // light cyan
    /*12*/0xFFFF5555, // light red
    /*13*/0xFFFF55FF, // light magenta
    /*14*/0xFFFFFF55, // yellow
    /*15*/0xFFFFFFFF, // white high intensity
  )
  private final val LOW_RES_PALETTE = Array( // TODO
    /*00*/0xFF000000, // black
    /*01*/0xFF0000AA, // blue
    /*02*/0xFF00AA00, // green
    /*03*/0xFF00AAAA, // cyan
    /*04*/0xFFAA0000, // red
    /*05*/0xFFAA00AA, // magenta
    /*06*/0xFFAA5500, // brown
    /*07*/0xFFAAAAAA, // white
    /*08*/0xFF555555, // gray
    /*09*/0xFF5555FF, // light blue
    /*10*/0xFF55FF55, // light green
    /*11*/0xFF55FFFF, // light cyan
    /*12*/0xFFFF5555, // light red
    /*13*/0xFFFF55FF, // light magenta
    /*14*/0xFFFFFF55, // yellow
    /*15*/0xFFFFFFFF, // white high intensity
  )
  private final val PALETTE_320_200 = Array(
    Array(PALETTE(2),PALETTE(4),PALETTE(6)), // Color Set 1 = Green, Red, Brown
    Array(PALETTE(3),PALETTE(5),PALETTE(7)), // Color Set 2 = Cyan, Magenta, White
    Array(PALETTE(4),PALETTE(3),PALETTE(7)), // Color Set 3 (Undocumented) = Red, Cyan, White
  )
  private final val PALETTE_320_200_HIGH_INTENSITY = Array(
    Array(PALETTE(2+8), PALETTE(4+8), PALETTE(6+8)), // Color Set 1 = Green, Red, Brown
    Array(PALETTE(3+8), PALETTE(5+8), PALETTE(7+8)), // Color Set 2 = Cyan, Magenta, White
    Array(PALETTE(4+8), PALETTE(3+8), PALETTE(7+8)), // Color Set 3 (Undocumented) = Red, Cyan, White
  )

  // ==========================================================================
  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    Property("Mode control register","%02X".format(modeControlRegister)) :: Property("Color select register","%02X".format(colorSelectRegister)) :: super.getProperties
  final def setROMJumper(insert:Boolean): Unit =
    fontROMOffset = 0x1000 + (if insert then 0 else 2048)
  override protected def getCharWidth: Int =
    resolution match
      case MEDIUM_RES => 8
      case HIGH_RES => if getMode == DrawMode.BITMAP then 16 else 8
      case LOW_RES => 16
  override protected def getInitialRegValues: Array[Int] = Array(
    /* Text mode (CGA) 80x25 */
    // Each character is 8 pixels
    // Each row is 8 scanlines
    0x71,	// Horizontal total:          113 characters ('minus one', 114*8 = 912 pixels)
    0x50,	// Horizontal displayed:      80 characters (80*8 = 640 pixels)
    0x5A,	// Horizontal sync position:  90 characters (90*8 = 720 pixels)
    0x0A,	// Horizontal sync width:     10 characters (10*8 = 80 pixels)
    0x1F,	// Vertical total:            31 rows ('minus one': 32*8 = 256 scanlines)
    0x06,	// Vertical total adjust:     6 scanlines (256+6 = 262)
    0x19,	// Vertical displayed:        25 rows (25*8 = 200 scanlines)
    0x1C,	// Vertical sync position:    28 rows (28*8 = 224 scanlines)
    0x02,	// Interlace mode:            2
    0x07,	// Maximum scan line address: 7 scanlines ('minus one', set character height to 8)
    0x06,	// Cursor start:              6 scanlines
    0x07	// Cursor end:                7 scanlines
  )

  override protected def getModeListenerNotification: String =
    if getMode == DrawMode.TEXT then
      s"CGA TEXT MODE (${regs(1)} x $getVisibleTextRows)"
    else
      s"CGA BITMAP MODE (${regs(1) * getCharWidth} x ${getVisibleTextRows * (getYCharsTotal + 1)})"

  // ========================= Borders & clips ================================
  override protected def getHSyncOffset: Int = if _40ColMode || getMode == DrawMode.BITMAP then 5 else 2
  override protected def getYSyncOffset: Int = 0
  // ========================= Card Info ======================================
  //  In 320x200 and 40x25 text mode (which has the equivalent resolution), a 7.16MHz dot clock is used
  override def getPixelClockFrequencyHz: Double = if _40ColMode || resolution == MEDIUM_RES then 14_318_000 >> 1 else 14_318_000
  override def getCardInfo: VideoCard.CardInfo = VideoCard.CardInfo(ram, mainMemoryOffset = 0xB_8000,dipSwitch54 = 0b10,supportColors = true)
  override def getPreferredSize: Dimension = new Dimension(912,262)
  // ========================= Drawing ========================================
  private def fetchGFXAndAttrs(): Unit =
    val ram = this.ram
    val xpos = getXPos
    if xpos == 0 then
      ram_base_address = ((vma << 1) + local_ram_ptr) & RAM_SIZE_MASK
    gfxBuffer(xpos) = ram(((vma << 1) + local_ram_ptr) & RAM_SIZE_MASK) & 0xFF
    local_ram_ptr += 1
    attrBuffer(xpos) = ram(((vma << 1) + local_ram_ptr) & RAM_SIZE_MASK) & 0xFF
    local_ram_ptr += 1
  end fetchGFXAndAttrs

  override final protected def drawTextCharLine(vsync: Boolean, videoEnabled: Boolean): Int =
    val bitmapOffset = getRasterLine * screenWidth
    val bitmap = this.bitmap
    val blinkModeEnabled = isBlinkingModeEnabled
    val charWidth = getCharWidth
    val rightBorderPix = getBorderWidth + regs(1) * charWidth
    var colPix = getRasterCharPos * charWidth
    val currentCharScanLine = getCurrentCharScanLine
    var xpos = getXPos

    hborder = colPix < getBorderWidth || colPix >= rightBorderPix

    if vsync || hborder then
      val borderColor = if getMode == DrawMode.BITMAP then
        val borderColorIndex = resolution match
          case MEDIUM_RES =>
            colorSelectRegister & 0xF
          case _ =>
            0
        PALETTE(borderColorIndex)
      else
        if (modeControlRegister & 0x10) != 0 then 0 else PALETTE(colorSelectRegister & 0xF)
      var x = 0
      while x < charWidth && colPix < screenWidth do
        // check used to avoid index error when screen height is adjusting
        if (bitmapOffset + colPix < bitmap.length) bitmap(bitmapOffset + colPix) = borderColor
        x += 1
        colPix += 1
    else
      val firstRowLine = currentCharScanLine == 0
      if firstRowLine then
        fetchGFXAndAttrs()

      var reverse = false
      var blink = false

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
      */
      blink = (attr & 0x80) != 0 && blinkModeEnabled
      val fg = PALETTE(attr & 0xF)
      val bg = if blinkModeEnabled then PALETTE((attr >> 4) & 7) else PALETTE((attr >> 4) & 0xF)

      if showCursor then
        val isCursorLine =
          if cursorTopLine == 0 && cursorBottomLine == 0 then false
          else if cursorTopLine <= cursorBottomLine then
            currentCharScanLine >= cursorTopLine && currentCharScanLine <= cursorBottomLine
          else
            currentCharScanLine < cursorBottomLine || currentCharScanLine > cursorTopLine

        reverse ^= isCursorLine
        if isCursorLine then reverse ^= isCursorOn // cursor blinking

      val char_ptr = (charCode << 3) + currentCharScanLine
      val charBitmap = charRom(fontROMOffset | char_ptr).toInt
      var showChar = true

      if (blink) showChar ^= charBlinkOn

      var bitmapPtr = bitmapOffset + colPix
      var gfx = charBitmap
      var x = 0

      while x < charWidth && colPix < rightBorderPix do
        val gfxBit = (gfx & 0x80) != 0
        val bit = showChar && gfxBit
        val color = if (bit ^ reverse) fg else bg

        gfx <<= 1

        // check used to avoid index error when screen height is adjusting
        if bitmapPtr < bitmap.length then
          if videoEnabled then
            bitmap(bitmapPtr) = color
          else
            bitmap(bitmapPtr) = PALETTE(0) // black

        bitmapPtr += 1
        colPix += 1
        x += 1
      end while
      xpos += 1
    end if
    xpos
  end drawTextCharLine

  override protected def vsync(): Unit =
    super.vsync()
    local_ram_ptr = 0

  override protected def drawBitmapCharLine(videoEnabled:Boolean): Unit =
    val backgroundColorIndex = resolution match
      case MEDIUM_RES =>
        colorSelectRegister & 0xF
      case _ =>
        0
    val backgroundColor = PALETTE(backgroundColorIndex)
    val bitmapOffset = getRasterLine * screenWidth
    val bitmap = this.bitmap
    val charWidth = getCharWidth
    val rightBorderPix = getBorderWidth + regs(1) * charWidth
    var colPix = getRasterCharPos * charWidth
    val hborder = colPix < getBorderWidth || colPix >= rightBorderPix

    if hborder then
      val borderColorIndex = resolution match
        case MEDIUM_RES =>
          colorSelectRegister & 0xF
        case _ =>
          0
      val borderColor = PALETTE(borderColorIndex)
      var x = 0
      while x < charWidth && colPix < screenWidth do
        // check used to avoid index error when screen height is adjusting
        if (bitmapOffset + colPix < bitmap.length) bitmap(bitmapOffset + colPix) = borderColor
        x += 1
        colPix += 1
    else
      var bitmapPtr = bitmapOffset + colPix
      var cx = 0
      val _320x200 = resolution == MEDIUM_RES
      val _160x200 = resolution == LOW_RES
      val bytesWidth = regs(1) << 1
      val row = (((vma << 1) + local_ram_ptr) & RAM_SIZE_MASK) / bytesWidth
      val col = (((vma << 1) + local_ram_ptr) & RAM_SIZE_MASK) % bytesWidth
      var ramPtr = (row & 1) << 13 // 0x2000 offset for odd rows
      ramPtr += (row >> 1) * bytesWidth + col
      var gfx = 0
      val cxLimit = (charWidth >> 1) - 1
      while cx < charWidth && colPix < rightBorderPix do
        if (cx & cxLimit) == 0 then
          gfx = ram(ramPtr) & 0xFF
          local_ram_ptr += 1
          ramPtr += 1
        var color = 0
        if _320x200 then
          val colorBits = (gfx >> 6) & 3
          gfx <<= 2
          if colorBits == 0 then
            color = backgroundColor
          else
            val paletteIndex = /*if (modeControlRegister & 4) != 0 then 2 else*/ if (colorSelectRegister & 0x20) != 0 then 1 else 0
            val paletteSet = if (colorSelectRegister & 0x10) == 0 then PALETTE_320_200 else PALETTE_320_200_HIGH_INTENSITY
            val palette = paletteSet(paletteIndex)
            color = palette(colorBits - 1)
        else if _160x200 then
          val colorBit = (gfx & 0x80) != 0
          gfx <<= 1
          color = if colorBit then PALETTE(colorSelectRegister & 0xF) else PALETTE(0)
        else
          val colorBit = (gfx & 0x80) != 0
          gfx <<= 1
          color = if colorBit then PALETTE(colorSelectRegister & 0xF) else PALETTE(0)
        end if
        // check used to avoid index error when screen height is adjusting
        if bitmapPtr < bitmap.length then
          if videoEnabled then
            bitmap(bitmapPtr) = color
          else
            bitmap(bitmapPtr) = PALETTE(0)
        cx += 1
        colPix += 1
        bitmapPtr += 1
  end drawBitmapCharLine

  // ========================= IN/OUT =========================================
  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(Seq(0x3D4,0x3D5,0x3D8,0x3D9,0x3DA,0x3DB,0x3DC), this)

  override def in8(port: Int): Int =
    port match
      case 0x3D5 =>
        read6845DataReg
      case 0x3DA => // CRT status
        // Bit 3: A 1 indicates that the raster is in a vertical retrace mode. Screen-buffer updating can be performed at this time.
        // Bit 2: The light pen switch is reflected in this bit.
        // Bit 1: A 1 indicates that a positive-going edge from the light pen has set the light pen's trigger.
        // Bit 0: A 1 indicates that a regen-buffer memory access can be made without interfering with the display.
        var status = 0xF0
        if isVBlank then status |= 8
        if isHBlank || isVBlank then status |= 1
        status
      case _ =>
        log.warning("CGA I/O reading from a %d", port)
        0xFF
  end in8

  override def out8(port: Int, value: Int): Unit =
    port match
      case 0x3D4 => // CRTC address register
        write6845AddressReg(value)
      case 0x3D5 => // CRTC data register
        write6845DataReg(value)
      case 0x3D8 => // Mode Control Register
        modeControlRegister = value
        // Bit 5: In text modes, if bit 5 is 1, characters with attribute bit 7 set will blink. If not, they will have high intensity background. This has no effect in graphics modes.
        // Bit 4: High-resolution graphics. If this bit is set, it selects 2-colour graphics (640 pixels wide) rather than 4-colour (320 pixel wide).
        //        In text mode, setting this bit has the following effects:
        //        1. The border is always black.
        //        2. The characters displayed are missing columns - as if the bit pattern has been ANDed with another value.
        // Bit 3: 1 to enable video output, 0 to disable it.
        // Bit 2: Black and white. On an RGB monitor it has no effect except in the 320x200 graphics mode, when it selects a third palette (black/red/cyan/white). This palette is not documented, and not all of IBM's later CGA-compatible cards support it.
        // Bit 1: Graphics mode. If this bit is set, the display RAM will be treated as bitmap graphics rather than as text.
        // Bit 0: This bit should only be set in the 80-column text mode. It changes various timing parameters (to display 160 bytes per row instead of 80); the CRTC will need to be reprogrammed accordingly.
        textModeBlinkModeEnabled((value & 0x20) != 0)
        val de = value & 0x08
        setVideoEnabled(de != 0)

        setMode(if (value & 2) == 0 then DrawMode.TEXT else DrawMode.BITMAP)
        /*
        Mode name	Mode bit 4-2-1-0  Notes
        ============================================================================================================================================================
          0	BW40	0	1	0	0	          40x25 text. Displays in greyscale on a composite monitor, colour on an RGB monitor. Some clone BIOSes treat this the same as CO40 below.
          1	CO40	0	0	0	0	          40x25 text. Displays in colour on a composite monitor, colour on an RGB monitor.
          2	BW80	0	1	0	1	          80x25 text. Displays in greyscale on a composite monitor, colour on an RGB monitor. Some clone BIOSes treat this the same as CO80 below.
          3	CO80	0	0	0	1	          80x25 text. Displays in colour on a composite monitor, colour on an RGB monitor.
          4		    0	0	1	0	          320x200 graphics. Displays in colour on a composite monitor, colour on an RGB monitor.
          5		    0	1	1	0	          320x200 graphics. Displays in greyscale on a composite monitor, in alternative palette on an RGB monitor.
          6		    1	1	1	0	          640x200 graphics. Displays in mono on a composite monitor, colour-on-black on an RGB monitor. Some clone BIOSes don't set bit 2, resulting in colour composite mode.
        Undocumented modes
        160x200   1 0 1 0           On an RGB monitor, this displays the same as the 640x200 graphics mode. On a composite monitor, this can display as a low-resolution colour mode.
        160x100   0 0 0 1           This is really the 80x25 text mode. The CRTC is reprogrammed to decrease character height to two pixels, giving an 80x100 text mode.
                                    Then all character cells are filled with character 0xDE, so that the left-hand side is drawn using the background colour and the right-hand side with the foreground colour.
                                    The attribute bytes are then used to create the screen graphics. The high 4 bits of a byte give the colour of the left-hand "pixel", and the low 4 bits give the colour of the right-hand "pixel".
        */
        val mode = value & 7 | (value & 0x10) >> 1
        resolution = mode match
          case 0b0100|0b0000|0b0010|0b0110 => MEDIUM_RES
          case 0b0101|0b0001|0b1110 => HIGH_RES
          case 0b1010 => LOW_RES
          case _ => MEDIUM_RES // ???
        _40ColMode = mode == 0b0100 || mode == 0b0000
        updateGeometry()
        log.info("CGA mode control register: %02X [video enabled=%b] resolution=%s", value, (value & 0x08) == 0x08,resolution)
        println(s"resolution=$resolution mode=$mode")
      case 0x3D9 => // Color Select Register
        /*
         7 Not used
         6 Not used
         5 Selects active color set in 320 by 200 graphics mode.
           When bit 5 is set to 0, colors are determined as follows: 00 = background (Defined by bits 0-3 of port hex 3D9), 01 = green, 10 = red, 11 = brown
           When bit 5 is set to 1, colors are determined as follows: 00 = background (Defined by bits 0-3 of port hex 3D9), 01 = cyan, 10 = magenta, 11 = white
         4 Selects alternate, intensified set of colors in the graphics mode. Selects background colors in the alphanumeric mode.
         3 Selects intensified border color in 40 by 25 alphanumeric mode.
           Selects intensified background color (CO-C1) in 320 by 200 graphics mode.
           Selects intensified foreground color in 640 by 200 graphics mode.
         2 Selects red border color in 40 by 25 alphanumeric mode.
           Selects red background color (CO-C1) in 320 by 200 graphics mode.
           Selects red foreground color in 640 by 200 graphics mode.
         1 Selects green border color in 40 by 25 alphanumeric mode.
           Selects green background color (CO-C1) in 320 by 200 graphics mode.
           Selects green foreground color in 640 by 200 graphics mode.
         0 Selects blue border color in 40 by 25 alphanumeric mode.
           Selects blue background color (CO-C1) in 320 by 200 graphics mode.
           Selects blue foreground color in 640 by 200 graphics mode.
         */
        colorSelectRegister = value
      case 0x3DB|0x3DC =>
        log.info("CGA writing into light pen registers. Ignored.")
      case _ =>
        log.warning("CGA writing unknown port: %d = %d", port, value)
  end out8