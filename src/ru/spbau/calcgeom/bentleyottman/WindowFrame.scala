package ru.spbau.calcgeom.bentleyottman

import World._
/**
 * Created by Sayon on 16.03.14.
 */
sealed class WindowFrame(val frame: Frame, val size: (Int, Int)) {
  def project(d: Dot): (Int, Int) = {
    val p = frame.project(d)
    val (x, y) = ((p.x / frame.width) * size._1, (1 - p.y / frame.height) * size._2)
    (x.toInt, y.toInt)
  }

  def project(d: Double): Int = project(frame.leftBot +(d, 0.0))._1


  def unproject(screenCoords: (Int, Int)) = new Dot(screenCoords._1 * 1.0 / size._1 * frame.width, (1.0 - screenCoords._2 * 1.0 / size._2) * frame.height) + frame.leftBot

  lazy val allCoordDots =
    for (x <- frame.left.floor.toInt to frame.right.ceil.toInt; y <- frame.bot.floor.toInt to frame.top.ceil.toInt)
    yield new Dot(x, y)

  private[this] val getTransposeDeltaX = frame.size._1 / 10

  private[this] val getTransposeDeltaY = frame.size._2 / 10


  def moveUp: WindowFrame = new WindowFrame(new Frame(frame.leftTop +(0.0, getTransposeDeltaY), frame.size), size)

  def moveDown: WindowFrame = new WindowFrame(new Frame(frame.leftTop +(0.0, -getTransposeDeltaY), frame.size), size)

  def moveRight: WindowFrame = new WindowFrame(new Frame(frame.leftTop +(getTransposeDeltaX, 0.0), frame.size), size)

  def moveLeft: WindowFrame = new WindowFrame(new Frame(frame.leftTop +(-getTransposeDeltaX, 0.0), frame.size), size)

  def resize(newsize: (Int, Int)) = new WindowFrame(
    frame.resize(newsize._1 * 1.0 / size._1, newsize._2 * 1.0 / size._2),
    newsize
  )

  val ZOOM_FACTOR = 1.3

  def zoomIn: WindowFrame = new WindowFrame(frame.scale(ZOOM_FACTOR, ZOOM_FACTOR), size)

  def zoomOut: WindowFrame = new WindowFrame(frame.scale(1.0 / ZOOM_FACTOR, 1.0 / ZOOM_FACTOR), size)
}
