package ucesoft.disitaco.chips

import ucesoft.disitaco.PCComponent

import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/03/2025 17:46  
 */
object i8253:
  trait CounterOutListener:
    def outChanged(value:Boolean): Unit

class i8253 extends PCComponent:
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/timer.png"))
  override val componentName = "8253"
  import i8253.*
  private enum ReadWriteMode:
    case CounterLatchMode, LSB, MSB, LSB_MSB

  private class Counter(val counterID: Int):
    import ReadWriteMode.*

    private inline val low = false
    private inline val high = true

    private var rwCounter = 0
    private var rCounter = 0
    private var controlWord = 0
    private var rwMode : ReadWriteMode = CounterLatchMode
    private var latch = 0
    private var latchedValue = 0
    private var latchedStatus = 0
    private var statusIsLacthed = false
    private var counterIsLatched = false
    private var counterLoaded = false
    private var counter = 0
    private var gate = false
    private var out = false
    private var mode : Mode = new Mode0
    private var outListener = new CounterOutListener:
      override def outChanged(value: Boolean): Unit = {}

    private final val MODES = Array(new Mode0,new Mode1, new Mode2, new Mode3, new Mode4, new Mode5)

    def getProperties: List[PCComponent.Property] =
      import PCComponent.Property
      val list = new ListBuffer[Property]
      list += Property(s"Counter #$counterID counter",counter.toString)
      list += Property(s"Counter #$counterID latch",latch.toString)
      list += Property(s"Counter #$counterID f/f",rCounter.toString)
      list += Property(s"Counter #$counterID rw mode",rwMode.toString)
      list += Property(s"Counter #$counterID mode",mode.id.toString)
      list += Property(s"Counter #$counterID gate",gate.toString)
      list += Property(s"Counter #$counterID out",out.toString)
      list.toList

    def reset(): Unit =
      rCounter = 0
      controlWord = 0
      rwMode = CounterLatchMode
      latch = 0
      counter = 0
      statusIsLacthed = false
      counterIsLatched = false
      out = false
      //outListener.outChanged(false)
      mode = new Mode0
    end reset

    final def clock(): Unit = mode.clock()

    final def performReadBackCommand(cw:Int): Unit =
      if (cw & 0x20) == 0 && !counterIsLatched then
        counterIsLatched = true
        latchedValue = counter // latch counter
      if (cw & 0x10) == 0 && !statusIsLacthed then
        statusIsLacthed = true
        latchedStatus = (if out then 0x80 else 0x00) | (if counterLoaded then 0x40 else 0x00) | (controlWord & 0x3F)

    final def writeCounter(value:Int): Unit =
      counterLoaded = false
      mode.writeCounter(value)

    final def readCounter: Int =
      val toBeRead = if statusIsLacthed then
        statusIsLacthed = false
        latchedStatus
      else if counterIsLatched then
        counterIsLatched = false
        latchedValue
      else
        counter
      rwMode match
        case LSB =>
          toBeRead & 0xFF
        case MSB =>
          (toBeRead >> 8) & 0xFF
        case LSB_MSB =>
          val value = rCounter match
            case 0 =>
              toBeRead & 0xFF
            case 1 =>
              (toBeRead >> 8) & 0xFF
          rCounter = (rCounter + 1) & 1
          value
        case _ =>
          0
    end readCounter

    final def setGate(g:Boolean): Unit =
      if !gate && g then mode.gateRisingEdge()
      else if gate && !g then mode.gateFallingEdge()
      gate = g

    final def setControlWord(cw:Int): Unit =
      if ((cw >> 4) & 3) == 0 then // COUNTER LATCH COMMAND
        /*
          If a Counter is latched and then, some time later,
          latched again before the count is read, the second
          Counter Latch Command is ignored. The count read
          will be the count at the time the first Counter Latch
          Command was issued.
         */
        if !counterIsLatched then
          counterIsLatched = true
          latchedValue = counter // latch counter
          //log.info("Counter[%d] latch command %04X",id,counter)
      else
        controlWord = cw
        rwMode = ReadWriteMode.fromOrdinal((cw >> 4) & 3)
        rwCounter = 0
        val modeValue = (cw >> 1) & 7
        if modeValue > 5 then
          log.error("Counter[%d] controlWord=%02X invalid mode=%d",counterID,controlWord,modeValue)
        else
          mode = MODES(modeValue)
          mode.init()

        log.info("Counter[%d] controlWord=%02X rwMode=%s mode=%s",counterID,controlWord,rwMode,mode.id)
    end setControlWord

    def setCounterListener(cl:CounterOutListener): Unit = outListener = cl

    private def decrementCounter(): Int =
      if (controlWord & 1) == 0 then
        counter = (counter - 1) & 0xFFFF
      else // BCD mode
        var nibble = counter & 0xF
        if nibble > 0 then counter = (counter & 0xFFF0) | (nibble - 1)
        else
          counter = (counter & 0xFFF0) | 9
          nibble = (counter >> 4) & 0xF
          if nibble > 0 then counter = (counter & 0xFF0F) | (nibble -1) << 4
          else
            counter = (counter & 0xFF0F) | 9 << 4
            nibble = (counter >> 8) & 0xF
            if nibble > 0 then counter = (counter & 0xF0FF) | (nibble - 1) << 8
            else
              counter = (counter & 0xF0FF) | 9 << 8
              nibble = (counter >> 12) & 0xF
              if nibble > 0 then counter = (counter & 0x0FFF) | (nibble - 1) << 12
              else
                counter = (counter & 0x0FFF) | 9 << 12
      counter
    end decrementCounter

    private def loadCounterFromLatch(): Unit =
      counter = latch
      counterLoaded = true

    private def setOut(newOut:Boolean): Unit =
      if newOut != out then
        out = newOut
        outListener.outChanged(out)

    private abstract class Mode(val id:Int):
      def gateRisingEdge(): Unit = {}
      def gateFallingEdge(): Unit = {}

      def init(): Unit = {}

      final def writeCounter(value:Int): Unit =
        rwMode match
          case LSB =>
            latch = (latch & 0xFF00) | (value & 0xFF)
            newCounterWritten()
            log.info("Counter[%d](mode=%d) set(LSB) counter to %04X",counterID,id,latch)
          case MSB =>
            latch = (latch & 0x00FF) | (value & 0xFF) << 8
            newCounterWritten()
            log.info("Counter[%d](mode=%d) set(MSB) counter to %04X",counterID,id,latch)
          case LSB_MSB =>
            rwCounter match
              case 0 =>
                latch = (latch & 0xFF00) | (value & 0xFF)
                firstByteOfNewCounterWritten()
              case 1 =>
                latch = (latch & 0x00FF) | (value & 0xFF) << 8
                newCounterWritten()
                log.info("Counter[%d](mode=%d) set(LSB_MSB) counter to %04X",counterID,id,latch)
            rwCounter ^= 1
          case _ =>
      end writeCounter

      protected def newCounterWritten(): Unit = {}
      protected def firstByteOfNewCounterWritten(): Unit = {}

      def clock(): Unit
    /*
      Mode 0 is typically used for event counting. After the
      Control Word is written, OUT is initially low, and will
      remain low until the Counter reaches zero. OUT then
      goes high and remains high until a new count or a
      new Mode 0 Control Word is written into the Counter.
    */
    private class Mode0 extends Mode(0):
      private var loadCounterOnNextClock = false

      override def init(): Unit =
        newCounterWritten()

      // Writing the first byte does not disable counting. OUT is set low immediately (no clock pulse required).
      override final protected def firstByteOfNewCounterWritten(): Unit = setOut(low)
      // After the Control Word and initial count are written to a Counter, the initial count will be loaded on the next CLK pulse.
      override final protected def newCounterWritten(): Unit =
        loadCounterOnNextClock = true
        setOut(low)

      /*
      After the Control Word and initial count are written to
      a Counter, the initial count will be loaded on the next
      CLK pulse. This CLK pulse does not decrement the
      count, so for an initial count of N, OUT does not go
      high until N a 1 CLK pulses after the initial count is
      written.

      GATE = 1 enables counting; GATE = 0 disables counting. GATE has no effect on OUT.
      */
      override final def clock(): Unit =
        if loadCounterOnNextClock then
          loadCounterOnNextClock = false
          loadCounterFromLatch()
        else if gate && decrementCounter() == 0 then
          setOut(high)
    end Mode0
    /*
    OUT will be initially high. OUT will go low on the CLK
    pulse following a trigger to begin the one-shot pulse,
    and will remain low until the Counter reaches zero.
    OUT will then go high and remain high until the CLK
    pulse after the next trigger.

    GATE has no effect on OUT.
    */
    private class Mode1 extends Mode(1):
      private var outGoLowOnNextClock = false
      private var counting = false

      override def init(): Unit =
        setOut(high)
        outGoLowOnNextClock = false
        counting = false

      override final def gateRisingEdge(): Unit =
        loadCounterFromLatch()
        outGoLowOnNextClock = true

      override final def clock(): Unit =
        if outGoLowOnNextClock then
          outGoLowOnNextClock = false
          counting = true
          setOut(low)
        else if counting then
          if decrementCounter() == 0 then
            counting = false
            setOut(high)
        else
          decrementCounter()
    end Mode1

    /*
    This Mode functions like a divide-by-N counter. It is
    typically used to generate a Real Time Clock interrupt.
    OUT will initially be high. When the initial count
    has decremented to 1, OUT goes low for one CLK
    pulse. OUT then goes high again, the Counter reloads
    the initial count and the process is repeated.
    Mode 2 is periodic; the same sequence is repeated
    indefinitely. For an initial count of N, the sequence
    repeats every N CLK cycles.

    GATE = 1 enables counting; GATE = 0 disables counting.
    */
    private class Mode2 extends Mode(2):
      override def init(): Unit =
        setOut(high)

      override def newCounterWritten(): Unit =
        loadCounterFromLatch()

      override final def gateRisingEdge(): Unit =
        loadCounterFromLatch()

      override final def clock(): Unit =
        if gate then
          val c = decrementCounter()
          if c == 1 then
            setOut(low)
          else if c == 0 then
            setOut(high)
            loadCounterFromLatch()
    end Mode2

    /*
    Mode 3 is typically used for Baud rate generation.
    Mode 3 is similar to Mode 2 except for the duty cycle
    of OUT. OUT will initially be high. When half the initial
    count has expired, OUT goes low for the remainder
    of the count. Mode 3 is periodic; the sequence
    above is repeated indefinitely. An initial count of N
    results in a square wave with a period of N CLK
    cycles.

    GATE = 1 enables counting; GATE = 0 disables counting.
    If GATE goes low while OUT is low, OUT is set high immediately
    */
    private class Mode3 extends Mode(3):
      private var evenCount = false
      private var oddReload = false

      override def init(): Unit =
        setOut(high)
        evenCount = false
        oddReload = false

      override final def gateRisingEdge(): Unit =
        loadCounterFromLatch()
        evenCount = (counter & 1) == 0
        oddReload = false
      override final def gateFallingEdge(): Unit =
        /*if out == low then*/ setOut(high)
      override def newCounterWritten(): Unit =
        loadCounterFromLatch()
        evenCount = (counter & 1) == 0
        oddReload = false

      /*
      Odd counts: OUT is initially high. The initial count
      minus one (an even number) is loaded on one CLK
      pulse and then is decremented by two on succeeding
      CLK pulses. One CLK pulse after the count expires,
      OUT goes low and the Counter is reloaded
      with the initial count minus one. Succeeding CLK
      pulses decrement the count by two. When the count
      expires, OUT goes high again and the Counter is
      reloaded with the initial count minus one. The above
      process is repeated indefinitely.
      */
      override final def clock(): Unit =
        if gate then
          if evenCount then
            decrementCounter()
            if decrementCounter() == 0 then
              setOut(!out)
              loadCounterFromLatch()
          else if oddReload then
            oddReload = false
            setOut(!out)
            loadCounterFromLatch()
          else
            decrementCounter()
            if decrementCounter() == 1 then
              oddReload = true
//        else
//          setOut(low)
    end Mode3

    /*
    OUT will be initially high. When the initial count expires,
    OUT will go low for one CLK pulse and then
    go high again. The counting sequence is ``triggered''
    by writing the initial count.
    GATE = 1 enables counting; GATE = 0 disables counting. GATE has no effect on OUT.
    */
    private class Mode4 extends Mode(4):
      private var loadCounterOnNextClock = false
      private var terminalCounter = 0
      private var counting = false

      override def init(): Unit =
        loadCounterOnNextClock = false
        terminalCounter = 0
        counting = false
        newCounterWritten()

      // After the Control Word and initial count are written to a Counter, the initial count will be loaded on the next CLK pulse.
      override final protected def newCounterWritten(): Unit =
        loadCounterOnNextClock = true
        setOut(high)

      /*
      After writing a Control Word and initial count, the
      Counter will be loaded on the next CLK pulse. This
      CLK pulse does not decrement the count, so for an
      initial count of N, OUT does not strobe low until
      N a 1 CLK pulses after the initial count is written.
      */
      override final def clock(): Unit =
        if loadCounterOnNextClock then
          loadCounterOnNextClock = false
          counting = true
          loadCounterFromLatch()
        else if gate then
          if terminalCounter > 0 then
            if terminalCounter == 2 then
              setOut(low)
            else
              setOut(high)
              counting = false
            terminalCounter -= 1
          else if counting then
            if decrementCounter() == 0 then
              terminalCounter = 2
          else
            decrementCounter()
    end Mode4

    /*
    OUT will initially be high. Counting is triggered by a
    rising edge of GATE. When the initial count has expired,
    OUT will go low for one CLK pulse and then
    go high again.
    After writing the Control Word and initial count, the
    counter will not be loaded until the CLK pulse after a
    trigger. This CLK pulse does not decrement the
    count, so for an initial count of N, OUT does not
    strobe low until N+1 CLK pulses after a trigger.
    */
    private class Mode5 extends Mode(5):
      private var startCountingOnNextClock = false
      private var terminalCounter = 0
      private var counting = false

      override def init(): Unit =
        setOut(high)
        startCountingOnNextClock = false
        terminalCounter = 0
        counting = false

      override final def gateRisingEdge(): Unit =
        startCountingOnNextClock = true

      override final def clock(): Unit =
        if startCountingOnNextClock then
          startCountingOnNextClock = false
          loadCounterFromLatch()
          counting = true
        else if terminalCounter > 0 then
          if terminalCounter == 2 then
            setOut(low)
          else
            setOut(high)
            counting = false
          terminalCounter -= 1
        else if counting then
          if decrementCounter() == 0 then
            terminalCounter = 2
        else
          decrementCounter()
    end Mode5
  end Counter

  // =================================================================================
  private val counters = Array(new Counter(0),new Counter(1),new Counter(2))

  override def getProperties: List[PCComponent.Property] =
    val list = new ListBuffer[PCComponent.Property]
    for c <- counters do
      list.addAll(c.getProperties)
    list.toList

  override protected def reset(): Unit =
    counters.foreach(_.reset())

  override protected def hardReset(): Unit =
    reset()
    counters.foreach(_.setGate(false))

  final def setCounterListener(counterIndex:Int,cl:CounterOutListener): Unit = counters(counterIndex).setCounterListener(cl)

  final def readCounter(index:Int): Int = counters(index).readCounter
  final def writeCounter(index:Int,value:Int): Unit =
    log.info("%s writing counter [%d] = %02X",componentName,index,value)  
    counters(index).writeCounter(value)
  final def writeControlWord(cw:Int): Unit =
    log.info("%s writing control word = %02X",componentName,cw)
    val counterIndex = (cw >> 6) & 3
    if counterIndex == 3 then // Read-back command
      if (cw & 0x8) != 0 then counters(2).performReadBackCommand(cw)
      if (cw & 0x4) != 0 then counters(1).performReadBackCommand(cw)
      if (cw & 0x2) != 0 then counters(0).performReadBackCommand(cw)
    else
      counters(counterIndex).setControlWord(cw)

  final def setGate(counterIndex:Int,value:Boolean): Unit = counters(counterIndex).setGate(value)

  final def clock(): Unit =
    var c = 0
    while c < 3 do
      counters(c).clock()
      c += 1

end i8253






