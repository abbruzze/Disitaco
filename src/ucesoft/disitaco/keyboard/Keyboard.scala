package ucesoft.disitaco.keyboard

import ucesoft.disitaco.{Clock, PCComponent}

import java.awt.event.{KeyEvent, KeyListener}
import javax.swing.{ImageIcon, JPanel}
import scala.collection.mutable

object Keyboard:
  def main(args: Array[String]): Unit =
    val f = new javax.swing.JFrame()
    f.setSize(300, 300)

    val p = new JPanel()
    f.getContentPane.add("Center",p)
    p.setFocusable(true)
    p.setFocusTraversalKeysEnabled(false)
    p.grabFocus()

    p.addKeyListener(new KeyListener:
      override def keyPressed(e: KeyEvent): Unit =
        println(s"extendedCode=${e.getExtendedKeyCode} location=${e.getKeyLocation} keyText=${KeyEvent.getKeyText(e.getKeyCode)}")

      override def keyReleased(e: KeyEvent): Unit = {}
      override def keyTyped(e: KeyEvent): Unit = {}
    )
    f.setVisible(true)
  import KeyEvent.*
  private case class K(key:Int,location:Int = KEY_LOCATION_STANDARD)

  private final val ITA_KEYBOARD : Map[K,Int] = Map(
    /*
           ____  ____      ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  __________  __________  __________
          | 59 || 60 |    |  1 ||  2 ||  3 ||  4 ||  5 ||  6 ||  7 ||  8 ||  9 || 10 || 11 || 12 || 13 ||    14    ||    69    ||    70    |
          |____||____|    |____||____||____||____||____||____||____||____||____||____||____||____||____||__________||__________||__________|
     */
    K(VK_F1) -> 59,K(VK_F2) -> 60,K(VK_ESCAPE) -> 1,K(VK_1) -> 2,K(VK_2) -> 3,K(VK_3) -> 4,K(VK_4) -> 5,K(VK_5) -> 6,K(VK_6) -> 7,K(VK_7) -> 8,K(VK_8) -> 9,
    K(VK_9) -> 10,K(VK_0) -> 11,K(VK_QUOTE) -> 12,K(16777452) -> 13/*ì*/,K(VK_BACK_SPACE) -> 14,K(VK_NUM_LOCK,KEY_LOCATION_NUMPAD) -> 69,K(VK_SCROLL_LOCK) -> 70, K(3/*break*/) -> 70,
    /*
           ____  ____      _______  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  _____  ______  ____  ____  ____  ____
          | 61 || 62 |    |   15  || 16 || 17 || 18 || 19 || 20 || 21 || 22 || 23 || 24 || 25 || 26 || 27  || 28   || 71 || 72 || 73 || 74 |
          |____||____|    |_______||____||____||____||____||____||____||____||____||____||____||____||_____||      ||____||____||____||____|
    */
    K(VK_F3) -> 61,K(VK_F4) -> 62,K(VK_TAB) -> 15,K(VK_Q) -> 16,K(VK_W) -> 17,K(VK_E) -> 18,K(VK_R) -> 19,K(VK_T) -> 20,K(VK_Y) -> 21,K(VK_U) -> 22,
    K(VK_I) -> 23,K(VK_O) -> 24,K(VK_P) -> 25,K(16777448) -> 26/*è*/,K(VK_PLUS) -> 27,K(VK_ENTER) -> 28,K(VK_HOME) -> 71,K(VK_HOME,KEY_LOCATION_NUMPAD) -> 71,
    K(VK_NUMPAD7,KEY_LOCATION_NUMPAD) -> 71,K(VK_UP) -> 72,K(VK_UP,KEY_LOCATION_NUMPAD) -> 72,K(VK_NUMPAD8,KEY_LOCATION_NUMPAD) -> 72,K(VK_PAGE_UP) -> 73,
    K(VK_PAGE_UP,KEY_LOCATION_NUMPAD) -> 73,K(VK_NUMPAD9,KEY_LOCATION_NUMPAD) -> 73,K(VK_SUBTRACT,KEY_LOCATION_NUMPAD) -> 74,
    /*
           ____  ____      ________  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____ |      | ____  ____  ____  ____
          | 63 || 64 |    |   29   || 30 || 31 || 32 || 33 || 34 || 35 || 36 || 37 || 38 || 39 || 40 || 41 ||      || 75 || 76 || 77 || 78 |
          |____||____|    |________||____||____||____||____||____||____||____||____||____||____||____||____||______||____||____||____||    |
    */
    K(VK_F5) -> 63,K(VK_F6) -> 64,K(VK_CONTROL,KEY_LOCATION_LEFT) -> 29,K(VK_A) -> 30,K(VK_S) -> 31,K(VK_D) -> 32,K(VK_F) -> 33,K(VK_G) -> 34,K(VK_H) -> 35,K(VK_J) -> 36,
    K(VK_K) -> 37,K(VK_L) -> 38,K(16777458) -> 39/*ò*/,K(16777440) -> 40/*à*/,K(16777465) -> 41/*ù*/,K(VK_LEFT) -> 75,K(VK_LEFT,KEY_LOCATION_NUMPAD) -> 75,
    K(VK_NUMPAD4,KEY_LOCATION_NUMPAD) -> 75,K(VK_NUMPAD5,KEY_LOCATION_NUMPAD) -> 76,K(0xC/*5*/,KEY_LOCATION_NUMPAD) -> 76,K(VK_RIGHT) -> 77,K(VK_RIGHT,KEY_LOCATION_NUMPAD) -> 77,
    K(VK_NUMPAD6,KEY_LOCATION_NUMPAD) -> 77,K(VK_ADD,KEY_LOCATION_NUMPAD) -> 78,
    /*
           ____  ____      _____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  _______  ______  ____  ____  ____ |    |
          | 65 || 66 |    |  42 || 43 || 44 || 45 || 46 || 47 || 48 || 49 || 50 || 51 || 52 || 53 ||   54  ||  55  || 79 || 80 || 81 ||    |
          |____||____|    |_____||____||____||____||____||____||____||____||____||____||____||____||_______||______||____||____||____||    |
    */
    K(VK_F7) -> 65,K(VK_F8) -> 66,K(VK_SHIFT,KEY_LOCATION_LEFT) -> 42,K(VK_LESS) -> 43,K(VK_Z) -> 44,K(VK_X) -> 45,K(VK_C) -> 46,K(VK_V) -> 47,
    K(VK_B) -> 48,K(VK_N) -> 49,K(VK_M) -> 50,K(VK_COMMA) -> 51,K(VK_PERIOD) -> 52,K(VK_MINUS) -> 53,K(VK_SHIFT,KEY_LOCATION_RIGHT) -> 54,K(VK_PRINTSCREEN) -> 55,
    K(VK_DECIMAL,KEY_LOCATION_NUMPAD) -> 55,K(VK_END) -> 79,K(VK_END,KEY_LOCATION_NUMPAD) -> 79,K(VK_NUMPAD1,KEY_LOCATION_NUMPAD) -> 79,
    K(VK_NUMPAD2,KEY_LOCATION_NUMPAD) -> 80,K(VK_DOWN,KEY_LOCATION_NUMPAD) -> 80,K(VK_DOWN) -> 80,K(VK_PAGE_DOWN) -> 81,K(VK_PAGE_DOWN,KEY_LOCATION_NUMPAD) -> 81,
    K(VK_NUMPAD3,KEY_LOCATION_NUMPAD) -> 81,
    /*
           ____  ____      __________  _________________________________________________________  ___________  __________  __________ |    |
          | 67 || 68 |    |    56    ||                         57                              ||    58     ||    82    ||    83    ||    |
          |____||____|    |__________||_________________________________________________________||___________||__________||__________||____|
     */
    K(VK_F9) -> 67,K(VK_F10) -> 68,K(VK_ALT,KEY_LOCATION_LEFT) -> 56,K(VK_ALT,KEY_LOCATION_RIGHT) -> 56,K(VK_SPACE) -> 57,K(VK_CAPS_LOCK) -> 58,
    K(VK_INSERT) -> 82,K(VK_INSERT,KEY_LOCATION_NUMPAD) -> 82,K(VK_NUMPAD0,KEY_LOCATION_NUMPAD) -> 82,K(VK_DELETE) -> 83,K(VK_DELETE,KEY_LOCATION_NUMPAD) -> 83
  )
/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/03/2025 11:43
 *
 *          ____  ____      ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  __________  __________  __________
 *         | 59 || 60 |    |  1 ||  2 ||  3 ||  4 ||  5 ||  6 ||  7 ||  8 ||  9 || 10 || 11 || 12 || 13 ||    14    ||    69    ||    70    |
 *         |____||____|    |____||____||____||____||____||____||____||____||____||____||____||____||____||__________||__________||__________|
 *          ____  ____      _______  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  _____  ______  ____  ____  ____  ____
 *         | 61 || 62 |    |   15  || 16 || 17 || 18 || 19 || 20 || 21 || 22 || 23 || 24 || 25 || 26 || 27  || 28   || 71 || 72 || 73 || 74 |
 *         |____||____|    |_______||____||____||____||____||____||____||____||____||____||____||____||_____||      ||____||____||____||____|
 *          ____  ____      ________  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____ |      | ____  ____  ____  ____
 *         | 63 || 64 |    |   29   || 30 || 31 || 32 || 33 || 34 || 35 || 36 || 37 || 38 || 39 || 40 || 41 ||      || 75 || 76 || 77 ||    |
 *         |____||____|    |________||____||____||____||____||____||____||____||____||____||____||____||____||______||____||____||____||    |
 *          ____  ____      _____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  ____  _______  ______  ____  ____  ____ | 78 |
 *         | 65 || 66 |    |  42 || 43 || 44 || 45 || 46 || 47 || 48 || 49 || 50 || 51 || 52 || 53 ||   54  ||  55  || 79 || 80 || 81 ||    |
 *         |____||____|    |_____||____||____||____||____||____||____||____||____||____||____||____||_______||______||____||____||____||    |
 *          ____  ____      __________  _________________________________________________________  ___________  __________  __________ |    |
 *         | 67 || 68 |    |    56    ||                         57                              ||    58     ||    82    ||    83    ||    |
 *         |____||____|    |__________||_________________________________________________________||___________||__________||__________||____|
 *
 * 8088 IRQ1(interrupt 09h)
 * KB_INT routine is called on each IRQ1 and it clears IRQ1 and shift register by hi(1) pulse of PB7 on 8255A.
 * Scan code 0xFF indicates overrun error and the routine just makes beep for that.(p5-50)
 *
 * Keyboard soft reset
 * KBD_RESET resets keyboard with pulling clock line low for 20ms(p5-98), keyboard is expected to return 0xAA(p5-32).
 *
 * And Data line should be Hi probably. Keyboard checks if Data line is Hi for a while. See around 0287-02BE. https://web.archive.org/web/20210307022734/http://halicery.com/Hardware/Intel%208042%20and%208048/8048_XT_INTERN.TEXT
 *
 * PORT_B, KB_CTL(61h)
 * Asserting PortB bit7(PB7) of 8255A clears shift register and IRQ1. PORTB bit6(PB6) of 8255A controls clock line, which is 1 normally.
 *
 * With the exception of the Pause key, all keys are makeI break.
 * The make scan code of a key is sent to the keyboard controller
 * when the key is pressed. When the key is released, its break scan
 * code is sent.
 *
 * Additionally, except for the Pause key, all keys are typematic.
 * When a key is pressed and held down, the keyboard sends the
 * make code for that key, delays 500 milliseconds ± 20%, and
 * begins sending a make code for that key at a rate of 10.9
 * characters per second ± 20%.
 * If two or more keys are held down, only the last key pressed
 * repeats at the typematic rate. Typematic operation stops when
 * the last key pressed is released, even if other keys are still held
 * down. If a key is pressed and held down while keyboard
 * transmission is inhibited, only the first make code is stored in the
 * buffer. This prevents buffer overflow as a result of typematic action.
 */
class Keyboard(clock:Clock,irq1:Boolean => Unit) extends PCComponent with KeyListener:
  import Keyboard.*
  override val componentName = "Keyboard"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/keyboard.png"))

  private inline val SOFT_RESET_MILLIS = 20
  private inline val REPETITION_MILLIS = 500
  private inline val REPETITION_STEP_MILLIS = 90 // 10.9 chars/sec
  private inline val BUFFER_SIZE = 16
  private inline val OVERRUN_SCANCODE = 0xFF
  private inline val RESET_DELAY_CYCLES = 100 // TODO set minimum value

  private var clockLineLowCycles = 0L
  private var enabled = false
  private val buffer = new mutable.Queue[Int]()
  private var lastKeyCodePressed = 0
  private var lastKeyCyclePressed = 0L
  private var repetitionModeOn = false

  private var softResetPending = false
  private var irqEnabled = false
  private var ctrlAltDelOn = false

  private final val keyMap = ITA_KEYBOARD

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    List(
      Property("Enabled",enabled.toString),
      Property(s"Buffer [${buffer.size}]",buffer.map(k => "%02X".format(k)).mkString(","))
    )

  override protected def reset(): Unit =
    buffer.clear()
    repetitionModeOn = false
    enabled = false
    softResetPending = false
    softReset()
    triggerIRQ()

  private def enqueueKey(code:Int): Unit =
    if buffer.length == BUFFER_SIZE then
      buffer.dropRight(1)
      buffer += OVERRUN_SCANCODE
    else
      buffer += code

  private def triggerIRQ(): Unit =
    if irqEnabled then
      irq1(false)
      irq1(true)

  override final def keyPressed(e: KeyEvent): Unit =
    if !enabled then
      return
    val key = K(e.getExtendedKeyCode,e.getKeyLocation)
    keyMap.get(key) match
      case Some(scanCode) =>
        e.consume()
        //println(s"Key pressed $key scanCode=$scanCode repetitionModeOn=$repetitionModeOn bufferSize=${buffer.size}")
        if scanCode != lastKeyCodePressed then
          repetitionModeOn = false
          lastKeyCodePressed = scanCode
          lastKeyCyclePressed = clock.cycles
          enqueueKey(scanCode)
          triggerIRQ()
        else if repetitionModeOn then
          if clock.clocksToMillisTillNow(lastKeyCyclePressed) >= REPETITION_STEP_MILLIS then
            lastKeyCyclePressed = clock.cycles
            enqueueKey(scanCode)
            triggerIRQ()
        else if clock.clocksToMillisTillNow(lastKeyCyclePressed) >= REPETITION_MILLIS then
          repetitionModeOn = true
          lastKeyCyclePressed = clock.cycles
          enqueueKey(scanCode)
          triggerIRQ()
      case None =>
        e.getExtendedKeyCode match
          case KeyEvent.VK_F12 =>
            buffer.clear()
            irq1(false)
            println("Keyboard buffer cleared")
          case KeyEvent.VK_F11 =>
            println("CTRL-ALT-DEL")
            ctrlAltDelOn = true
            val keys = Array(29,56,83)
            for k <- keys do buffer += k
            clock.scheduleMillis(1000,_ => ctrlAltDelOn = false) // to be sure that if no keys are read we reset the flag
            triggerIRQ()
          case _ =>
            println(s"Key not recognized: $e")
  end keyPressed
  override final def keyTyped(e: KeyEvent): Unit = {}
  override final def keyReleased(e: KeyEvent): Unit =
    if !enabled then return
    val key = K(e.getExtendedKeyCode, e.getKeyLocation)
    keyMap.get(key) match
      case Some(scanCode) =>
        repetitionModeOn = false
        lastKeyCodePressed = -1
        enqueueKey(scanCode | 0x80)
        triggerIRQ()
      case None =>
  end keyReleased

  private var lastKey = 0

  def readScanCode: Int =
    if buffer.isEmpty then
      val r = if softResetPending then 0x00 else lastKey // no data
      softResetPending = false
      r
    else
      lastKey = buffer.dequeue()
      if ctrlAltDelOn && buffer.isEmpty then
        softReset()
        triggerIRQ()
        ctrlAltDelOn = false
      lastKey

  def clear(clearOn:Boolean): Unit =
    irqEnabled = !clearOn
    if clearOn then
      irq1(false)
      irq1(buffer.nonEmpty)
  def clockLine(low:Boolean): Unit =
    if low then
      clockLineLowCycles = clock.cycles
    else // low -> high
      val elapsed = clock.clocksToMillisTillNow(clockLineLowCycles)
      if !enabled && elapsed >= SOFT_RESET_MILLIS then
        softReset()
        log.info("Keyboard soft reset elapsed=%f",elapsed)
        clock.schedule(RESET_DELAY_CYCLES,_ => {
          triggerIRQ()
          log.info("Keyboard IRQ1")
        })
    enabled = !low

  private def softReset(): Unit =
    log.info("Keyboard soft reset")
    buffer.clear()
    buffer += 0xAA // reset response code
    repetitionModeOn = false
    softResetPending = true
