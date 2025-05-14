package ucesoft.disitaco

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/03/2025 11:24  
 */
class DipSwitches(val number:Int,initialValue:Int = 0):
  private var _switches = initialValue
  
  def switches: Int = _switches
  def switches_=(value:Int): Unit = _switches = value
