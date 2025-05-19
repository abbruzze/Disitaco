package ucesoft.disitaco.speaker

import ucesoft.disitaco.PCComponent

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 18:51  
 */
trait Audio extends PCComponent:
  def turn(on:Boolean): Unit
  def setMasterVolume(v: Int): Unit
  def getMasterVolume: Int
  def getLastPerformance: Int
  def available(): Int
  def addSample(sample:Boolean): Unit
  def setOut(): Unit
