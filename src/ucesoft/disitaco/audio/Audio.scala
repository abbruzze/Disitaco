package ucesoft.disitaco.audio

import ucesoft.disitaco.{MessageBus, PCComponent}

import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, FloatControl, SourceDataLine}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 18:51  
 */
abstract class Audio(val sampleRate:Int,name:String) extends PCComponent with Runnable:
  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize = 0

  private var buffer: Array[Byte] = Array()
  private var bufferPendingSize = -1

  private var bufferId = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this, s"AudioDevice-$name")
  private var muted, lastMuted = false
  private var sourceLine: SourceDataLine = scala.compiletime.uninitialized
  private var volumeLine: FloatControl = scala.compiletime.uninitialized
  private var masterVolume = 0
  private var stopped = false
  private var lastPerformance = 0
  private var turnedOn = false

  setBufferMillisNow(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)
  MessageBus.add(this)
  
  protected def getPreferredVolume: Int = 100
  
  protected def getAudioFormat: AudioFormat

  override def getProperties: List[PCComponent.Property] =
    import PCComponent.Property
    List(
      Property("Muted", muted.toString),
      Property("Buffer millis", bufferInMillis.toString),
      Property("Master volume", masterVolume.toString),
      Property("Performance", lastPerformance.toString)
    )

  private def setBufferMillisNow(bim: Int): Unit =
    setBufferInMillis(bim)
    bufferSize = bufferPendingSize
    buffer = Array.ofDim[Byte](bufferSize)
    bufferPendingSize = -1

  def setBufferInMillis(bim: Int): Unit =
    bufferInMillis = bim
    bufferPendingSize = 2 * (sampleRate * bim / 1000.0).toInt

  override protected def reset(): Unit =
    queue.clear()
    if sourceLine != null then
      sourceLine.flush()

  protected def addSampleToBuffer(sample:Int): Unit =
    buffer(bufferId) = (sample & 0xFF).toByte;
    bufferId += 1
    buffer(bufferId) = (sample >> 8).toByte;
    bufferId += 1
    if bufferId == bufferSize then
      queue.put(buffer)
      if bufferPendingSize != -1 then
        bufferSize = bufferPendingSize
        bufferPendingSize = -1
      buffer = Array.ofDim[Byte](bufferSize)
      bufferId = 0

  protected def canAddSample: Boolean = turnedOn && !muted

  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted: Boolean = muted

  def mute(muted: Boolean): Unit =
    this.muted = muted

  def stop(): Unit =
    stopped = true

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.WarpMode(_, enabled) =>
        if enabled then
          lastMuted = muted
          muted = true
        else
          muted = lastMuted
      case _ =>  

  private def getSourceLine: Option[SourceDataLine] =
    try
      val format = getAudioFormat

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      try
        val name = sourceLine.getClass.getSuperclass.getCanonicalName
        if name == "com.sun.media.sound.DirectAudioDevice.DirectDL" then
          val f = sourceLine.getClass.getSuperclass.getDeclaredField("waitTime")
          f.setAccessible(true)
          f.set(sourceLine, 1)
      catch
        case e: Exception =>
          println(s"Cannot initialize audio: $e")

      sourceLine.open(format)

      volumeLine = sourceLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
      setMasterVolume(getPreferredVolume)
      
      sourceLine.start()
      Some(sourceLine)
    catch
      case t: Throwable =>
        t.printStackTrace()
        None

  def setMasterVolume(v: Int): Unit =
    if volumeLine != null then
      val max = volumeLine.getMaximum
      val min = volumeLine.getMinimum / 2f
      volumeLine.setValue((v / 100.0f) * (max - min) + min)
      masterVolume = v

  def getMasterVolume: Int = masterVolume
  def available(): Int = if sourceLine == null then 0 else sourceLine.available()
  def getLastPerformance: Int = lastPerformance

  override def run(): Unit =
    getSourceLine match
      case Some(sl) =>
        sourceLine = sl
        log.info("Audio-%s System started",name)
        while !stopped do
          val samples = queue.take()
          val available = sourceLine.available()
          if available >= samples.length then
            sourceLine.write(samples, 0, samples.length)
            lastPerformance = 100
          else
            sourceLine.write(samples, 0, available)
            lastPerformance = (available / samples.length * 100.0).toInt

        sourceLine.drain()
        sourceLine.close()
        sourceLine = null
        log.info("Audio-%s System stopped",name)
      case None =>
        log.error("Cannot initialize Audio-%s system",name)

  def turn(on: Boolean): Unit =
    log.info("Audio-%s is %s",name, if on then "on" else "off")
    turnedOn = on
