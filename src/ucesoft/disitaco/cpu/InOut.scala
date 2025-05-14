package ucesoft.disitaco.cpu

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/02/2025 09:48  
 */
trait InOut:
  def in(port:Int,size8:Boolean): Int
  def out(port:Int, value:Int,size8:Boolean): Unit
