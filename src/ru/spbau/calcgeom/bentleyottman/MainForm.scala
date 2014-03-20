package ru.spbau.calcgeom.bentleyottman


import javax.swing._
import java.awt._

import java.awt.event._

class MainForm extends JFrame("Bentley Ottman") {

  setLayout(new BorderLayout)

  setSize(FORM_SIZE)
  setMinimumSize(FORM_SIZE)

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val drawer = new DrawingPanel

  add(drawer)

  setVisible(true)

  addKeyListener(new KeyListener {
    def keyTyped(e: KeyEvent): Unit = {}

    def keyPressed(e: KeyEvent): Unit = e.getKeyCode match {
      case KeyEvent.VK_UP => drawer.moveUp()
      case KeyEvent.VK_DOWN => drawer.moveDown()
      case KeyEvent.VK_RIGHT => drawer.moveRight()
      case KeyEvent.VK_LEFT => drawer.moveLeft()
      case KeyEvent.VK_Z => drawer.zoomOut()
      case KeyEvent.VK_X => drawer.zoomIn()

      case _ =>
    }

    def keyReleased(e: KeyEvent): Unit = {}
  })

  addMouseListener(new MouseListener {
    def mouseExited(e: MouseEvent): Unit = {}

    def mouseClicked(e: MouseEvent): Unit = {
      val p = e.getLocationOnScreen
      SwingUtilities.convertPointFromScreen(p, drawer)
      println(s"${e.getX} ${e.getY}, pic pos : $p")
      drawer.addDot((p.x, p.y))
    }

    def mouseEntered(e: MouseEvent): Unit = {}

    def mousePressed(e: MouseEvent): Unit = {}

    def mouseReleased(e: MouseEvent): Unit = {}
  })
}


object Starter extends App {
  EventQueue.invokeLater(new Runnable() {
    def run() {
      new MainForm().setVisible(true)
    }
  })
}


