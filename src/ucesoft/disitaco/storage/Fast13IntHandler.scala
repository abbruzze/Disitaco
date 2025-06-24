package ucesoft.disitaco.storage

import ucesoft.disitaco.{Clock, Motherboard}
import ucesoft.disitaco.cpu.{Registers, i8088}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/05/2025 18:47  
 */
class Fast13IntHandler(mother:Motherboard) extends i8088.InterruptHandler:
  private val motorOnID : Array[Clock.EventID] = Array.ofDim[Clock.EventID](mother.floppyDrives + mother.hardDisks)

  override def interrupt(regs: Registers): Boolean =
    import regs.*
    ah match
      case 0x02 =>  // read
        val track = cx >> 8
        var sect = cx & 0xFF
        val head = dx >> 8
        var n = ax & 0xFF
        var addr = (es << 4) + bx
        var driveID = dx & 0xFF
        if (driveID & 0x80) == 0 && !mother.fdc.fdc.getDrives(driveID).hasDiskInserted then
          return false
        val drive = if (driveID & 0x80) == 0 then mother.fdc.fdc.getDrives(driveID) else mother.hdc.hdFdc.getDrives(driveID & 0x7F)
        if (driveID & 0x80) != 0 then
          sect -= 1
          driveID = (driveID & 0x7F) + mother.floppyDrives
        if driveID >= motorOnID.length then
          return false
        //println("Read sectors (driveID=" + driveID + ", track=" + track + ", sect=" + sect + ", head=" + head + ", n=" + n + ")")
        drive.getListener.onMotor(driveID,motorOn = true)
        if motorOnID(driveID) != null then
          motorOnID(driveID).cancel()
        motorOnID(driveID) = mother.clock.scheduleMillis(1000,_ => {
          drive.getListener.onMotor(driveID,motorOn = false)
        })
        drive.getListener.onPosition(driveID, track, head, sect)
        if sect >= drive.geometry.sectorsPerTrack || track > drive.geometry.tracks then
          setFlags(Registers.F_CARRY)
          return true
        while n > 0 do
          val bytes = drive.getDiskInserted.get.readSector(track, head, sect).toArray
          var offset = 0
          while offset < bytes.length do
            mother.memory.writeByte(addr, bytes(offset), abs = true)
            addr += 1
            offset += 1
          n -= 1
          sect += 1
        mother.memory.writeByte(0x441, 0, abs = true)
        clearFlags(Registers.F_CARRY)
        ah = 0
        true
      case _ =>
        false
  end interrupt

