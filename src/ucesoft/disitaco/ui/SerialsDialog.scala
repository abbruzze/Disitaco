package ucesoft.disitaco.ui

import java.awt.GridLayout
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.{JDialog, JFrame, WindowConstants}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 12/06/2025 11:36  
 */
class SerialsDialog(frame:JFrame,closeOperation:() => Unit):
  final val com1 = new SerialPanel("COM1")
  final val com2 = new SerialPanel("COM2")
  final val dialog = new JDialog(frame,"Serials")

  init()

  private def init(): Unit =
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = closeOperation()
    })
    dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    dialog.getContentPane.setLayout(new GridLayout(2,0))
    dialog.getContentPane.add(com1)
    dialog.getContentPane.add(com2)
    dialog.pack()
    dialog.setResizable(false)

