package ucesoft.disitaco.speaker

import ucesoft.disitaco.{MessageBus, PCComponent}

import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.*
import javax.swing.ImageIcon

class Speaker(val sampleRate:Int) extends Audio with Runnable:
  override protected val componentName = "Speaker"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/audio.png"))

  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize = 0

  private var buffer: Array[Byte] = Array()
  private var bufferPendingSize = -1

  private var bufferId = 0
  private var bufferPos = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this,s"AudioDevice")
  private var muted, lastMuted = false
  private var sourceLine : SourceDataLine = scala.compiletime.uninitialized
  private var volumeLine : FloatControl = scala.compiletime.uninitialized
  private var masterVolume = 0
  private var stopped = false
  private var lastPerformance = 0
  private var turnedOn = false

  private var sampleSum = 0
  private var sampleCount = 0

  setBufferMillisNow(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.WarpMode(_,enabled) =>
        if enabled then
          lastMuted = muted
          muted = true
        else
          muted = lastMuted
      case _ =>

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    List(
      Property("Muted",muted.toString),
      Property("Buffer millis",bufferInMillis.toString),
      Property("Master volume",masterVolume.toString),
      Property("Performance",lastPerformance.toString)
    )

  private def setBufferMillisNow(bim:Int): Unit =
    setBufferInMillis(bim)
    bufferSize = bufferPendingSize
    buffer = Array.ofDim[Byte](bufferSize)
    bufferPendingSize = -1

  def setBufferInMillis(bim:Int) : Unit =
    bufferInMillis = bim
    bufferPendingSize = 2 * (sampleRate * bim / 1000.0).toInt

  override protected def reset(): Unit =
    queue.clear()

  inline private def dequeue() : Array[Byte] = queue.take()

  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted : Boolean = muted

  def mute(muted:Boolean) : Unit =
    this.muted = muted

  def stop(): Unit =
    stopped = true

  inline private def getSourceLine: Option[SourceDataLine] =
    try
      val format = new AudioFormat(sampleRate.toFloat, 16 ,1, true, false)

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      try
        val name = sourceLine.getClass.getSuperclass.getCanonicalName
        if name == "com.sun.media.sound.DirectAudioDevice.DirectDL" then
          val f = sourceLine.getClass.getSuperclass.getDeclaredField("waitTime")
          f.setAccessible(true)
          f.set(sourceLine,1)
      catch
        case e:Exception =>
          println(s"Cannot initialize audio: $e")

      sourceLine.open(format)

      volumeLine = sourceLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
      setMasterVolume(70)

      sourceLine.start()
      Some(sourceLine)
    catch
      case t: Throwable =>
        t.printStackTrace()
        None

  override def setMasterVolume(v: Int): Unit =
    if volumeLine != null then
      val max = volumeLine.getMaximum
      val min = volumeLine.getMinimum / 2f
      volumeLine.setValue((v / 100.0f) * (max - min) + min)
      masterVolume = v

  override def getMasterVolume: Int = masterVolume
  override def available(): Int = if sourceLine == null then 0 else sourceLine.available()
  override def getLastPerformance: Int = lastPerformance

  override def run(): Unit =
    getSourceLine match
      case Some(sl) =>
        sourceLine = sl
        log.info("Audio System started")
        while !stopped do
          val samples = queue.take()
          val available = sourceLine.available()
          if available >= samples.length then
            sourceLine.write(samples, 0, samples.length)
            lastPerformance = 100
          else
            sourceLine.write(samples, 0, available)
            lastPerformance = (available / samples.length * 100.0).toInt
            //if lastPerformance != 100 then println(s"Perf $lastPerformance samples=${samples.length} available=$available")

        sourceLine.drain()
        sourceLine.close()
        sourceLine = null
        log.info("Audio System stopped")
      case None =>
        log.error("Cannot initialize audio system")

  override def turn(on: Boolean): Unit =
    log.info("Audio is %s",if on then "on" else "off")
    turnedOn = on

  override def addSample(sample: Boolean): Unit =
    sampleSum += (if !turnedOn || muted then -1 else if sample then 1 else -1)
    sampleCount += 1

  override def setOut(): Unit =
    val _sample = sampleSum.toDouble / sampleCount
    sampleCount = 0
    sampleSum = 0
    var sample = (_sample * 32768).toInt
    if sample > 32767 then sample = 32767
    else if sample < -32768 then sample = -32768

    buffer(bufferId) = (sample & 0xFF).toByte ; bufferId += 1
    buffer(bufferId) = (sample >> 8).toByte ; bufferId += 1
    //println(s"OUT=${_sample} = ${buffer(bufferId - 1)}")
    if bufferId == bufferSize then
      queue.put(buffer)
      if bufferPendingSize != -1 then
        bufferSize = bufferPendingSize
        bufferPendingSize = -1
      buffer = Array.ofDim[Byte](bufferSize)
      bufferId = 0