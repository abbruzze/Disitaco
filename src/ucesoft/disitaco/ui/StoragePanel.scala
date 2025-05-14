package ucesoft.disitaco.ui

import ucesoft.disitaco.storage.{DiskDrive, DiskImage}

import java.awt.{Color, FlowLayout, Font}
import javax.swing.*

object StoragePanel:
  trait StorageListener:
    def openImage(diskId:Int): Unit
    def ejectImage(diskId:Int): Unit
/**
 * @author Alessandro Abbruzzetti
 *         Created on 02/12/2024 14:31  
 */
class StoragePanel extends JPanel with DiskDrive.DiskDriveListener:
  import StoragePanel.*
  private val disketteDisabled = new ImageIcon(getClass.getResource("/resources/disk_disabled.png"))
  private val disketteOff = new ImageIcon(getClass.getResource("/resources/disk_off.png"))
  private val disketteRead = new ImageIcon(getClass.getResource("/resources/disk_read.png"))
  private val disketteWrite = new ImageIcon(getClass.getResource("/resources/disk_write.png"))
  private val hdOff = new ImageIcon(getClass.getResource("/resources/hd_off.png"))
  private val hdRead = new ImageIcon(getClass.getResource("/resources/hd_read.png"))
  private val hdWrite = new ImageIcon(getClass.getResource("/resources/hd_write.png"))

  private var listener : StorageListener = new StorageListener:
    override def openImage(diskId: Int): Unit = println(s"Load#$diskId")
    override def ejectImage(diskId: Int): Unit = println(s"Eject$diskId")


  private class Diskette(val index:Int,val hardDisk:Boolean) extends JPanel:
    private val info = new JLabel(getInfo)
    val eject = new JMenuItem("Eject image", new ImageIcon(getClass.getResource("/resources/eject-button.png")))

    val icon = new JLabel(if hardDisk then hdOff else disketteDisabled)
    var head = 0
    var track = 0
    var sector = 0
    var doubleSide = false
    var writing = false

    init()

    private def getInfo: String =
      if doubleSide then
        if hardDisk then
          "[%d] %d-%03d/%02d".format(index, head, track, sector)
        else
          "[%d] %d-%02d/%02d".format(index, head, track, sector)
      else
        "[%d] %02d/%02d".format(index, track, sector)


    private def init(): Unit =
      setLayout(new FlowLayout(FlowLayout.LEFT))
      add(icon)
      info.setFont(Font.getFont(Font.MONOSPACED))
      add(info)

      if !hardDisk then
        val popup = new JPopupMenu()
        val load = new JMenuItem("Open image",new ImageIcon(getClass.getResource("/resources/open-folder.png")))
        load.addActionListener(_ => listener.openImage(index))
        eject.addActionListener(_ => listener.ejectImage(index))
        eject.setEnabled(false)

        popup.add(load)
        popup.add(eject)

        setComponentPopupMenu(popup)

    def updateInfo(): Unit = info.setText(getInfo)
  end Diskette

  private var diskette : Array[Diskette] = Array()

  def setDiskette(disketteSize:Int,hdSize:Int,listener:StorageListener): Unit =
    removeAll()
    diskette = (0 until disketteSize + hdSize).map(i => new Diskette(i,i >= disketteSize)).toArray
    diskette.foreach(add(_))
    this.listener = listener

  private def init(): Unit =
    setLayout(new FlowLayout(FlowLayout.LEFT))
    setBorder(BorderFactory.createLineBorder(Color.BLACK))

  private def swing(event: => Unit): Unit =
    SwingUtilities.invokeLater(() => event)
  
  override def onPosition(id:Int,track:Int,head:Int,sector:Int): Unit =
    if id < diskette.length then
      swing {
        diskette(id).head = head
        diskette(id).track = track
        diskette(id).sector = sector
        diskette(id).updateInfo()
      }
  override def onMotor(driveIndex: Int, on: Boolean): Unit =
    if driveIndex < diskette.length then
      swing {
        if diskette(driveIndex).hardDisk then
          diskette(driveIndex).icon.setIcon(if on then hdRead else hdOff)
        else
          diskette(driveIndex).icon.setIcon(if on then disketteRead else disketteOff)
      }
  override def onDiskInserted(driveIndex: Int, image: DiskImage): Unit =
    swing {
      diskette(driveIndex).eject.setEnabled(true)
      diskette(driveIndex).doubleSide = image.diskGeometry.heads > 1
      diskette(driveIndex).icon.setIcon(if diskette(driveIndex).hardDisk then hdOff else disketteOff)
      diskette(driveIndex).head = 0
      diskette(driveIndex).track = 0
      diskette(driveIndex).sector = 0
      diskette(driveIndex).updateInfo()
      diskette(driveIndex).setToolTipText(image.diskName)
      if image.isReadOnly then
        diskette(driveIndex).setBorder(BorderFactory.createLineBorder(Color.RED))
      else
        diskette(driveIndex).setBorder(BorderFactory.createEmptyBorder())
    }
  override def onDiskEjected(driveIndex: Int, image: DiskImage): Unit =
    swing {
      diskette(driveIndex).eject.setEnabled(false)
      diskette(driveIndex).icon.setIcon(disketteDisabled)
      diskette(driveIndex).head = 0
      diskette(driveIndex).track = 0
      diskette(driveIndex).sector = 0
      diskette(driveIndex).updateInfo()
      diskette(driveIndex).setToolTipText(null)
      diskette(driveIndex).setBorder(BorderFactory.createEmptyBorder())
    }
  override def onModeChanged(id:Int,isWriting:Boolean): Unit =
    swing {
      if diskette(id).hardDisk then
        diskette(id).icon.setIcon(if isWriting then hdWrite else hdRead)
      else
        diskette(id).icon.setIcon(if isWriting then disketteWrite else disketteRead)
      diskette(id).writing = isWriting
    }