package ucesoft.disitaco.audio

import ucesoft.disitaco.Config

import javax.sound.sampled.*
import javax.swing.ImageIcon

class Speaker(override val sampleRate:Int) extends Audio(sampleRate,"Speaker"):
  override protected val componentName = "Speaker"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/audio.png"))

  private var sampleSum = 0
  private var sampleCount = 0
  
  override protected def getPreferredVolume: Int = Config.getSpeakerVolume

  override protected def reset(): Unit =
    sampleSum = 0
    sampleCount = 0
  
  override protected def getAudioFormat : AudioFormat = new AudioFormat(sampleRate.toFloat, 16, 1, true, false)

  def addSample(sample: Boolean): Unit =
    sampleSum += (if !canAddSample then -1 else if sample then 1 else -1)
    sampleCount += 1

  def setOut(): Unit =
    val _sample = sampleSum.toDouble / sampleCount
    sampleCount = 0
    sampleSum = 0
    var sample = (_sample * 32768).toInt
    if sample > 32767 then sample = 32767
    else if sample < -32768 then sample = -32768

    addSampleToBuffer(sample)