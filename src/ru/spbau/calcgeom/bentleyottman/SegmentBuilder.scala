package ru.spbau.calcgeom.bentleyottman
import World._
/**
 * Created by Sayon on 16.03.14.
 */
sealed class SegmentBuilder {
  private[this] var dot: Option[Dot] = None

  def pushDot(d: Dot): Option[Segment] = dot match {
    case None => dot = Some(d); None
    case Some(start) => val res = new Segment(start, d); dot = None; Some(res)
  }

  def pending: List[Dot] = dot match {
    case None => Nil
    case Some(start) => start :: Nil
  }
}
