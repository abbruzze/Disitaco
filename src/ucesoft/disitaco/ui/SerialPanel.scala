package ucesoft.disitaco.ui

import ucesoft.disitaco.chips.INS8250

import java.awt.{BorderLayout, FlowLayout}
import javax.swing.border.BevelBorder
import javax.swing.{BorderFactory, ImageIcon, JLabel, JPanel}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 12/06/2025 11:14  
 */
class SerialPanel(name:String) extends JPanel with INS8250.SignalListener:
  private final val redLed = new ImageIcon(getClass.getResource("/resources/redLed.png"))
  private final val greenLed = new ImageIcon(getClass.getResource("/resources/greenLed.png"))

  private class Led(ledName:String,withCounter:Boolean = false) extends JPanel:
    private var counter = 0
    private val counterLabel = new JLabel
    private val ledLabel = new JLabel(redLed)
    init()

    private def updateCounterLabel(): Unit =
      counterLabel.setText("%06d".format(counter))

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
      northPanel.add(new JLabel(ledName))
      val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      if withCounter then
        updateCounterLabel()
        southPanel.add(counterLabel)
      southPanel.add(ledLabel)
      add("North", northPanel)
      add("South", southPanel)

    def setLed(on:Boolean): Unit =
      if on then ledLabel.setIcon(greenLed) else ledLabel.setIcon(redLed)
    def incCounter(): Unit =
      counter += 1
      updateCounterLabel()

  end Led

  private val dtrLed = new Led("DTR")
  private val dsrLed = new Led("DSR")
  private val rtsLed = new Led("RTS")
  private val ctsLed = new Led("CTS")
  private val rxLed = new Led("RX",withCounter = true)
  private val txLed = new Led("TX",withCounter = true)
  private val stateLabel = new JLabel(name)

  init()

  override def dtrChanged(dtr: Boolean): Unit = dtrLed.setLed(dtr)
  override def dsrChanged(dsr: Boolean): Unit = dsrLed.setLed(dsr)
  override def rtsChanged(rts: Boolean): Unit = rtsLed.setLed(rts)
  override def ctsChanged(cts: Boolean): Unit = ctsLed.setLed(cts)
  override def rxChanged(rx: Boolean): Unit =
    if rx then rxLed.incCounter()
    rxLed.setLed(rx)
  override def txChanged(tx: Boolean): Unit =
    if tx then txLed.incCounter()
    txLed.setLed(tx)

  override def setState(state: String): Unit =
    stateLabel.setText(s"$name - $state")

  private def init(): Unit =
    setLayout(new BorderLayout())
    val northPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    northPanel.add(stateLabel)
    northPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    southPanel.add(dtrLed)
    southPanel.add(dsrLed)
    southPanel.add(rtsLed)
    southPanel.add(ctsLed)
    southPanel.add(rxLed)
    southPanel.add(txLed)
    add("North",northPanel)
    add("South",southPanel)
