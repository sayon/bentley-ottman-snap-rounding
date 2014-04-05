package ru.spbau.calcgeom.bentleyottman

import ru.spbau.calcgeom.bentleyottman.World._
import javax.swing.JPanel
import scala.Some
import java.awt.{Color, Graphics2D, Graphics}
import java.awt.event.{ComponentEvent, ComponentListener}


class DrawingPanel extends JPanel {

  implicit var plane: CartesianPlane =
    new Dot(0.0, 0.0) :: new Dot(1.0, 1.0) :: new Dot(1.0, 0.0) :: new Dot(0.0, 1.0) :: Nil
  implicit var window = new WindowFrame(new Frame((0.0, 6.0), (8.0, 6.0)), (800, 600))

  val segmentBuilder = new SegmentBuilder

  var segments: scala.List[Segment] = Nil

  var intersections: List[Dot] = Nil

  setSize(DRAWING_AREA_SIZE)

  protected[this] override def paintComponent(graphics: Graphics) {
    implicit val g = graphics.asInstanceOf[Graphics2D]
    super.paintComponent(graphics)
    g.setColor(Color.BLACK)

    implicit var color = Color.BLACK
    implicit var radius = 2
    val sequence = plane.toStream ++ segments
    sequence.foreach(Drawer(_))

    //pale ones
    radius = 2
    color = Color.GRAY
    window.allCoordDots.foreach(Drawer.drawDot)

    radius = 4
    color = Color.BLUE
    val accented = segmentBuilder.pending ::: intersections
    accented.foreach(Drawer(_))

  }


  def addDot(d: (Int, Int)) = {
    val newDot = window.unproject(d)
    segmentBuilder.pushDot(newDot) match {
      case None =>
      case Some(seg) =>
        segments = seg :: segments
        if (segments.size >= 2) {
          println( "for segments " + segments.foldLeft("")(_ + " " + _))
//          intersections = Intersections(segments) map ( _.position )
          //intersections = SegmentIntersectionsSolver(segments).map(_.d)
          intersections map {
            x => printf(x + " ")
          }
          println(s"intersections $intersections")
        }

    }
    repaint()
  }

  def moveUp() = {
    window = window.moveUp
    repaint()
  }

  def moveDown() = {
    window = window.moveDown
    repaint()
  }

  def moveRight() = {
    window = window.moveRight
    repaint()
  }

  def moveLeft() = {
    window = window.moveLeft
    repaint()
  }

  def zoomIn() = {
    window = window.zoomIn
    repaint()
  }

  def zoomOut() = {
    window = window.zoomOut
    repaint()
  }

  addComponentListener(new ComponentListener {
    def componentShown(e: ComponentEvent): Unit = {}

    def componentHidden(e: ComponentEvent): Unit = {}

    def componentMoved(e: ComponentEvent): Unit = {}

    def componentResized(e: ComponentEvent): Unit = {
      window = window.resize(e.getComponent.getWidth, e.getComponent.getHeight)
    }
  })
}
