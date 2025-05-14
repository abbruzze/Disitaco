package ucesoft.disitaco.chips

import ucesoft.disitaco.{Clock, PCComponent}

import java.time.{Instant, LocalDateTime, ZoneId}
import javax.swing.ImageIcon
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 03/05/2025 19:11  
 */
class MC146818(clock:Clock) extends PCComponent with Clock.Clockable:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/clock.png"))
  override val componentName = "MC146818"

  private inline val SECONDS        = 0
  private inline val ALARM_SECONDS  = 1
  private inline val MINUTES        = 2
  private inline val ALARM_MINUTES  = 3
  private inline val HOURS          = 4
  private inline val ALARM_HOURS    = 5
  private inline val DAY_OF_WEEK    = 6
  private inline val DAY_OF_MONTH   = 7
  private inline val MONTH          = 8
  private inline val YEAR           = 9
  private inline val CENTURY        = 0x32

  private inline val STATUS_A       = 10
  private inline val STATUS_B       = 11
  private inline val STATUS_C       = 12
  private inline val STATUS_D       = 13

  private final val ram = Array.ofDim[Int](64)
  private var addressRegister = 0
  private var ts = System.currentTimeMillis()
  private var clockID : Clock.EventID = uninitialized

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    val bcd = (ram(STATUS_B) & 0x04) == 0
    val _12Mode = (ram(STATUS_B) & 0x02) == 0
    updateTime()
    List(
      Property("BCD mode",bcd.toString),
      Property("12 mode",_12Mode.toString),
      Property("Ts",ts.toString),
      Property("Hours",ram(HOURS).toString),
      Property("Minutes",ram(MINUTES).toString),
      Property("Seconds",ram(SECONDS).toString),
      Property("Century",ram(CENTURY).toString),
      Property("Year",ram(YEAR).toString),
      Property("Month",ram(MONTH).toString),
      Property("Day",ram(DAY_OF_MONTH).toString),
      Property("Day of week",ram(DAY_OF_WEEK).toString),
    )

  override protected def reset(): Unit =
    ts = System.currentTimeMillis()

    if clockID == null then
      clockID = clock.scheduleMillis(1000,this,isPeriodic = true)
      log.info("%s scheduled 1 sec update callback",componentName)
    ram(STATUS_A) = 0b010 << 4 | 0b0110
    ram(STATUS_B) = 2
    ram(STATUS_C) = 0
    ram(STATUS_D) = 0x80 // power is good

  private def updateTime(): Unit =
    val ldt = Instant.ofEpochMilli(ts).atZone(ZoneId.systemDefault()).toLocalDateTime
    ram(YEAR) = ldt.getYear % 100
    ram(CENTURY) = ldt.getYear / 100
    ram(SECONDS) = ldt.getSecond
    ram(MINUTES) = ldt.getMinute
    val hoursCutoff = if (ram(STATUS_B) & 0x02) == 0 then 12 else 24
    ram(HOURS) = ldt.getHour % hoursCutoff
    ram(MONTH) = ldt.getMonthValue
    ram(DAY_OF_MONTH) = ldt.getDayOfMonth
    ram(DAY_OF_WEEK) = ldt.getDayOfWeek.getValue

  private def writeTime(): Unit =
    val ldt = LocalDateTime.of(ram(CENTURY) * 100 + ram(YEAR),ram(MONTH),ram(DAY_OF_MONTH),ram(HOURS),ram(MINUTES),ram(SECONDS))
    log.info("%s date updated to %s",componentName,ldt)
    ts = ldt.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli

  private def toBCD(v:Int,conv:Boolean): Int =
    if !conv then return v

    val d = (v / 10) & 0xF
    val u = (v % 10) & 0xF
    d << 4 | u
  private def fromBCD(v:Int,conv:Boolean): Int =
    if !conv then
      v
    else
      (((v >> 4) & 0xF) * 10) + (v & 0xF)

  final def clock(cycles:Long): Unit =
    if (ram(STATUS_B) & 0x80) == 0 then
      ts += 1000

  def writeAddressRegister(address:Int): Unit =
    addressRegister = address & 0x7F
    log.info("%s write address register %02X",componentName,address)

  def writeDataRegister(value:Int): Unit =
    if addressRegister < 10 || addressRegister == CENTURY then
      writeTimeRegister(addressRegister,value)
    else if addressRegister < 0xE then
      writeStatusRegister(addressRegister,value)
    else
      ram(addressRegister) = value
      log.info("%s write ram address %02X = %02X",componentName,addressRegister,value)
  def readDataRegister(): Int =
    if addressRegister < 10 || addressRegister == CENTURY then
      readTimeRegister(addressRegister)
    else if addressRegister < 0xE then
      readStatusRegister(addressRegister)
    else
      log.info("%s reading ram address %02X",componentName,addressRegister)
      if addressRegister == 0x14 then
        0b01 << 6 | 0b10 << 4
      else
        ram(addressRegister)

  private def writeTimeRegister(reg:Int,value: Int): Unit =
    val bcd = (ram(STATUS_B) & 0x04) == 0
    ram(reg) = fromBCD(value,bcd)
    log.info("%s write time reg %d = %02X (%02X) BCD=%b",componentName,reg,ram(reg),value,bcd)
    printf("%s write time reg %d = %02X (%02X) BCD=%b\n",componentName,reg,ram(reg),value,bcd)

  private def writeStatusRegister(reg:Int,value:Int): Unit =
    reg match
      case STATUS_A =>
        log.info("%s writing STATUS A = %02X",componentName,value)
        ram(STATUS_A) = value
      case STATUS_B =>
        log.info("%s writing STATUS B = %02X",componentName,value)
        if (ram(STATUS_B) & 0x80) != 0 && (value & 0x80) == 0 then
          log.info("%s updating time ...",componentName)
          writeTime()
        ram(STATUS_B) = value
        // TODO INTERRUPT STUFF
      case _ =>
        log.warning("%s writing to a non writeable status reg %d = %02X",reg,value)

  private def readTimeRegister(reg: Int): Int =
    updateTime()
    val bcd = (ram(STATUS_B) & 0x04) == 0
    val value = toBCD(ram(reg),bcd)
    reg match
      case HOURS | ALARM_HOURS =>
        val _12mode = (ram(STATUS_B) & 0x02) == 0
        if _12mode then value | 0x80 else value
      case _ =>
        value


  private def readStatusRegister(reg:Int): Int =
    reg match
      case STATUS_C =>
        val status = ram(STATUS_C)
        ram(STATUS_C) = 0
        // TODO INTERRUPT RESET
        status
      case _ =>
        ram(reg)