package ru.spbau.calcgeom.bentleyottman

import java.awt.{Color, Graphics2D}
import ru.spbau.calcgeom.bentleyottman.World.{PlaneObject, Segment, Dot, Circle}

object Drawer {
  private[this] def inNewContext(g: Graphics2D)(f: () => Unit) = {
    val (c, b) = (g.getColor, g.getBackground)
    f()
    g.setColor(c)
    g.setBackground(b)
  }

  def drawCircle(c: Circle)(implicit g: Graphics2D, frame: WindowFrame, color: Color) = inNewContext(g)(() => {
    val leftTop = frame.project(c.center +(-c.radius, c.radius))
    val diam = frame.project(c.radius) * 2
    g.setColor(color)
    g.drawOval(leftTop._1, leftTop._2, diam, diam)
  })

  private[this] val DOT_RADIUS = 2

  def drawDotStamp(d: Dot)(implicit g: Graphics2D, frame: WindowFrame, color: Color, size: Int = DOT_RADIUS) = inNewContext(g)(() => {
    val p = frame.project(d)
    g.drawString(d.toString, p._1 + 2 * size, p._2 + 2 * size)
  })

  def drawDot(d: Dot)(implicit g: Graphics2D, frame: WindowFrame, color: Color, size: Int = DOT_RADIUS) = inNewContext(g)(() => {
    val p = frame.project(d)
    println(p)
    g.setColor(color)
    g.fillOval(p._1 - size, p._2 - size, 2 * size, 2 * size)
  })

  def drawSegment(s: Segment)(implicit g: Graphics2D, frame: WindowFrame, color: Color, size: Int = DOT_RADIUS) = inNewContext(g)(() => {
    val (from, to) = (frame.project(s.left), frame.project(s.right))
    g.setColor(color)
    drawDotStamp(s.left)
    drawDotStamp(s.right)
    g.drawLine(from._1, from._2, to._1, to._2)
  })

  def apply(o: PlaneObject)(implicit g: Graphics2D, frame: WindowFrame, color: Color, size: Int = DOT_RADIUS) = o match {
    case d: Dot => drawDot(d); drawDotStamp(d) //; println("Drawing a dot")
    case c: Circle => drawCircle(c) //; println("Drawing a circle")
    case s: Segment => drawSegment(s) //; println("Drawing a segment")
    case other => throw new NotImplementedError(s"Need to implement drawing method for $other")
  }

}
