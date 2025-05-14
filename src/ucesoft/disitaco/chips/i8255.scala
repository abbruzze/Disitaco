package ucesoft.disitaco.chips

import ucesoft.disitaco.PCComponent

object i8255:
  trait Port:
    def read: Int
    def write(value:Int): Unit
/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/03/2025 18:53
 *
 * Modes 1 and 2 are not emulated !!
 */
class i8255 extends PCComponent:
  override val componentName = "8255"
  import i8255.*
  inline private val A = 0
  inline private val B = 1
  inline private val C = 2

  private object NullPort extends Port:
    override def read: Int = 0xFF
    override def write(value: Int): Unit = {}

  private final val ports : Array[Port] = Array(NullPort,NullPort,NullPort)
  private final val outputLatches = Array.ofDim[Int](3)
  private var controlWord = 0

  override protected def reset(): Unit =
    java.util.Arrays.fill(outputLatches,0)
    controlWord = 0

  final def setPort(number:Int,port:Port): Unit = ports(number) = port

  /*
    Control word format

    |D7|D6|D5|D4|D3|D2|D1|D0|

    D7 = 1 MODE SET
    =====================================================
    GROUP B
    D0 = PORT C LOWER (1 = INPUT)
    D1 = PORT B (1 = INPUT)
    D2 = MODE (0 = MODE 0, 1 = MODE 1)
    GROUP A
    D3 = PORT C UPPER (1 = INPUT)
    D4 = PORT A (1 = INPUT)
    D56 = MODE (00 = MODE 0, 01 = MODE 1, 1X = MODE 2)

    D7 = 0 SET/RESET BIT
    =====================================================
    D0 = 1 = SET, 0 = RESET
    D123 = BIT 0-7 of port C
    D456 = ignored
  */
  final def writeControlWord(cw:Int): Unit =
    log.info("%s writing control word = %02X",componentName,cw)
    if (cw & 0x80) == 0x80 then // select modes
      if (cw & 0x4) != 0 || ((cw >> 5) & 3) != 0 then
        log.error("Modes 1 and 2 are not emulated!!")
      controlWord = cw
    else // bit set/reset format
      val bit = (cw >> 1) & 7
      val set = (cw & 1) != 0

      if (bit < 4 && (controlWord & 1) == 0) || (bit > 3 && (controlWord & 8) == 0) then
        if set then
          outputLatches(C) |= 1 << bit
        else
          outputLatches(C) &= ~(1 << bit)

        ports(C).write(outputLatches(C))
  end writeControlWord

  final def read(port:Int): Int =
    port match
      case A =>
        if (controlWord & 0x10) != 0 then ports(A).read else outputLatches(A)
      case B =>
        if (controlWord & 0x2) != 0 then ports(B).read else outputLatches(B)
      case C =>
        var pc = ports(C).read
        if (controlWord & 1) == 0 then
          pc = (pc & 0xF0) | (outputLatches(C) & 0x0F)
        if (controlWord & 8) == 0 then
          pc = (pc & 0x0F) | (outputLatches(C) & 0xF0)
        pc
      case _ =>
        0xFF

  final def write(port:Int,value:Int): Unit =
    //log.info("%s writing port %s = %02X",componentName,('A' + port).toChar,value)
    port match
      case A =>
        if (controlWord & 0x10) == 0 then
          ports(A).write(value)
          outputLatches(A) = value
      case B =>
        if (controlWord & 0x2) == 0 then
          ports(B).write(value)
          outputLatches(B) = value
      case C =>
        if (controlWord & 0x9) == 0 then
          ports(C).write(value)
          outputLatches(C) = value
        else
          var w = value
          if (controlWord & 0x1) != 0 then w = (w & 0xF0) | (outputLatches(C) & 0x0F)
          if (controlWord & 0x8) != 0 then w = (w & 0x0F) | (outputLatches(C) & 0xF0)
          outputLatches(C) = w
          ports(C).write(w)
      case _ =>







