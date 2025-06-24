package ucesoft.disitaco.util

import java.io.File

/**
 * @author Alessandro Abbruzzetti
 *         Created on 24/06/2025 10:22  
 */
object ZeroPad:
  def main(args:Array[String]): Unit =
    if args.length != 1 then
      println("Usage <disk image>")
      sys.exit(1)

    val file = new File(args(0))
    if !file.exists() then
      println(s"File $file does not exist")
      sys.exit(1)

    val len = file.length().toInt / 1024
    val padLen = if len < 360 then 360
    else if len < 720 then 720
    else if len < 1440 then 1440
    else
      println(s"File len $len seems to big")
      sys.exit(1)

    val bytes = java.nio.file.Files.readAllBytes(file.toPath)
    val target = Array.ofDim[Byte](padLen * 1024)
    System.arraycopy(bytes,0,target,0,bytes.length)
    java.nio.file.Files.write(file.toPath,target)
