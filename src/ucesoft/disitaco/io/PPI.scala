package ucesoft.disitaco.io

import ucesoft.disitaco.chips.i8255

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/03/2025 15:58  
 */
class PPI extends IODevice:
  override protected val componentName = "8255 PPI"
  final val ppi = new i8255

  override def register(ioHandler: IOHandler): Unit = ioHandler.registerDevice(0x60 to 0x64,this)

  add(ppi)

  override final def in8(port: Int): Int =
    port match
      // Windows 2.x will not work if 0x64 is not configured as an alias of 0x60
      // Also noteworthy for emulation is that, on original IBM PC hardware, I/O ports 60-63h decode only the low 2 bits across 60h-7Fh.
      // Port 64h is an alias for port 60h on the original IBM PC hardware.
      case 0x60|0x64 => ppi.read(0)
      case 0x61 => ppi.read(1)
      case 0x62 => ppi.read(2)
      case _ =>
        log.warning("PPI reading from unhandled port %d",port)
        0

  override final def out8(port: Int, value: Int): Unit =
    port match
      case 0x60 => ppi.write(0,value)
      case 0x61 => ppi.write(1,value)
      case 0x62 => ppi.write(2,value)
      case 0x63 => ppi.writeControlWord(value)
      case _ =>
        log.warning("PPI writing to unhandled port %d",port)