package ucesoft.disitaco.video

import ucesoft.disitaco.Display
import ucesoft.disitaco.debugger.Debugger.VideoFrameProducer
import ucesoft.disitaco.io.IODevice

import java.awt.Dimension

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/04/2025 16:34  
 */
object VideoCard:
  case class CardInfo(mainMemoryOffset:Int,dipSwitch54:Int,supportColors:Boolean)

  trait VideoCardListener:
    def modeChanged(mode:String,screenWidth:Int,screenHeight:Int): Unit
    def charFrequencyChanged(charFrequencyInHz:Double): Unit

trait VideoCard extends IODevice with VideoFrameProducer:
  def getCardInfo: VideoCard.CardInfo
  def clockChar(): Unit
  def setModeListener(listener: VideoCard.VideoCardListener): Unit
  def setDisplay(display: Display): Unit
  def enableCompositeMonitor(enable:Boolean): Unit = {}

  def getPixelClockFrequencyHz: Double
  def getPreferredSize: Dimension
  def getPreferredZoomX: Double = 1.0
  def getPreferredZoomY: Double = 1.0

  def setClippingOn(on:Boolean): Unit
  
  def writeVideoRAM(address:Int,value:Int): Unit
  def readVideoRAM(address:Int): Int
