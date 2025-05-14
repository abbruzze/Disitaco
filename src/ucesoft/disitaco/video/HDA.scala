package ucesoft.disitaco.video

/**
 * @author Alessandro Abbruzzetti
 *         Created on 03/04/2025 16:43  
 */
class HDA extends MDA:
  override protected val componentName = "HDA"
  protected override val ram = Array.ofDim[Byte](64 * 1024)

  private inline val GRAPHIC_CHAR_WIDTH = 16
  private inline val GRAPHIC_CHAR_HEIGHT = 4
  
  private var pageOffset = 0

  override protected def getModeListenerNotification: String = 
    if getMode == DrawMode.TEXT then
      s"HDA TEXT MODE (${regs(1)} x $visibleTextRows)"
    else
      s"HDA BITMAP MODE (${regs(1) * GRAPHIC_CHAR_WIDTH} x ${visibleTextRows * GRAPHIC_CHAR_HEIGHT})"

  override final protected def getCharWidth: Int =
    import DrawMode.*
    getMode match
      case TEXT => super.getCharWidth
      case BITMAP => GRAPHIC_CHAR_WIDTH

  // ========================= Borders & clips ================================
  override final protected def getHSyncOffset: Int = if getMode == DrawMode.BITMAP then 4 else super.getHSyncOffset
  // ========================= Drawing ========================================
  override protected def drawBitmapCharLine(videoEnabled:Boolean): Unit =
    val bitmapOffset = rasterLine * screenWidth
    val bitmap = this.bitmap
    val ram = this.ram
    val charWidth = GRAPHIC_CHAR_WIDTH
    val rightBorderPix = borderWidth + regs(1) * charWidth
    var colPix = rasterLineCharPos * GRAPHIC_CHAR_WIDTH
    val hborder = colPix < borderWidth || colPix >= rightBorderPix
    if hborder then
      var x = 0
      while x < charWidth && colPix < screenWidth do
        // check used to avoid index error when screen height is adjusting
        if (bitmapOffset + colPix < bitmap.length) bitmap(bitmapOffset + colPix) = BACKGROUND_COLOR
        x += 1
        colPix += 1
    else
      var bitmapPtr = bitmapOffset + colPix
      var cx = 0
      var x = colPix - borderWidth
      val y = ypos * (ychars_total + 1) + currentCharScanLine
      lastBit = false
      while cx < GRAPHIC_CHAR_WIDTH && colPix < rightBorderPix do
        // The offset (into the page) of the byte containing dot (x,y) in each page is:
        // [2000H * (Y MOD 4)] + [90 * INTEGER (Y/4)] + [INTEGER (X/8)]
        // and the bit in the byte that stores the dot is bit position
        // 7 - (X MOD 8)
        val ptr = ((ram_ptr + pageOffset) & 0xFFFF) + 0x2000 * (y & 3) + 90 * (y >> 2) + (x >> 3)
        val gfx = ram(ptr)
        val bit = 7 - (x & 7)
        lastBit = (gfx & (1 << bit)) != 0
        val color = if lastBit then FOREGROUND_COLOR else BACKGROUND_COLOR
        // check used to avoid index error when screen height is adjusting
        if bitmapPtr < bitmap.length then 
          if videoEnabled then
            bitmap(bitmapPtr) = color
          else
            bitmap(bitmapPtr) = BACKGROUND_COLOR
        cx += 1
        colPix += 1
        x += 1
        bitmapPtr += 1
  end drawBitmapCharLine
  // ========================= IN/OUT =========================================
  override final def in8(port: Int): Int =
    port match
      case 0x3BA => // CRT status
        // Bit 7: Vertical retrace, 0 = vertical retrace (Screen is temporarily blanked.) 1 = active display.
        // Bit 3: Video. This is 1 if a green or bright green pixel is being drawn on the screen at this moment.
        // Bit 0: Retrace. This is 1 if the horizontal retrace is active.
        var status = 0x0
        if !isVBlank then status |= 0x80
        if isHBlank then status |= 1
        if lastBit then status |= 0x8
        status
      case _ =>
        super.in8(port)
  override final def out8(port: Int, value: Int): Unit =
    import DrawMode.*
    port match
      case 0x3B8 => // Mode Control Register
        // Bit 7: 0 = Page 0 (Power on default. Start display at B0000) 1 = Page 1 (Start display at B8000). This bit selects the active display buffer on the Graphics Card.)
        // Bit 5: 1 to enable blinking, 0 to disable it: If bit 5 is 1, characters with attribute bit 7 set will blink. If not, they will have high intensity background.
        // Bit 3: 1 to enable video output, 0 to disable it.
        // Bit 1: 0 = text mode (Power on default), 1 = graphics mode
        setMode(if (value & 2) == 0 then TEXT else BITMAP)
        latch_addresses()
        textModeBlinkModeEnabled((value & 0x20) != 0)
        setVideoEnabled((value & 0x08) == 0x08)
        updateGeometry()
        pageOffset = if (value & 0x80) == 0 then 0 else 0x8000
        log.info("HDA mode control register: %02X [video enabled=%b] mode=%s pageOffset=%d", value, (value & 0x09) == 0x09,if (value & 2) == 0 then TEXT else BITMAP,pageOffset)
      case _ =>
        super.out8(port,value)