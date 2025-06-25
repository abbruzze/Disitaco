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
        var track = cx >> 8
        var sect = cx & 0xFF
        var head = dx >> 8
        var n = ax & 0xFF
        var addr = (es << 4) + bx
        var driveID = dx & 0xFF
        val hd = (driveID & 0x80) != 0
        if !hd && !mother.fdc.fdc.getDrives(driveID).hasDiskInserted then
          return false
        val drive = if !hd then mother.fdc.fdc.getDrives(driveID) else mother.hdc.hdFdc.getDrives(driveID & 0x7F)
        if hd then
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

        if hd then
          if sect >= drive.geometry.sectorsPerTrack then
            setFlags(Registers.F_CARRY)
            return true
        else
          if sect > drive.geometry.sectorsPerTrack then
            setFlags(Registers.F_CARRY)
            return true
        val geo = drive.geometry
        while n > 0 do
          val bytes = drive.getDiskInserted.get.readSector(track, head, sect).toArray
          var offset = 0
          while offset < bytes.length do
            mother.memory.writeByte(addr, bytes(offset), abs = true)
            addr += 1
            offset += 1
          n -= 1

          if hd then
            if sect < geo.sectorsPerTrack - 1 then sect += 1
            else if head < geo.heads - 1 then
              head += 1
              sect = 0
            else if track < geo.tracks - 1 then
              track += 1
              head = 0
              sect = 0
            else
              track = geo.tracks
              head = 0
              sect = 0
          else
            if sect < geo.sectorsPerTrack then sect += 1
            else if head < geo.heads - 1 then
              head += 1
              sect = 1
            else if track < geo.tracks - 1 then
              track += 1
              head = 0
              sect = 1
            else
              track = geo.tracks
              head = 0
              sect = 1
        mother.memory.writeByte(0x441, 0, abs = true)
        clearFlags(Registers.F_CARRY)
        ah = 0
        true
      case _ =>
        false
  end interrupt

