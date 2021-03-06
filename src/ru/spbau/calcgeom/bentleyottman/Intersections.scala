package ru.spbau.calcgeom.bentleyottman

import ru.spbau.calcgeom.bentleyottman.World._
import scala.collection.mutable
import scala.annotation.tailrec

object Intersections {

  abstract class Event(val position: Dot) extends Ordered[Event] {
    def compare(that: Event) = - ( position.x compare that.position.x )
  }

  case class Left(segment: Segment) extends Event(segment.left)

  case class Right(segment: Segment) extends Event(segment.right)

  case class Intersection(above: Segment, below: Segment, override val position: Dot) extends Event(position)

  private[this] def enqueueIntersection(seg: Option[Segment], that: Option[Segment])(implicit queue: mutable.PriorityQueue[Event]): Unit = seg match {
    case None =>
    case Some(s) => enqueueIntersection(s, that)
  }

  private[this] def enqueueIntersection(seg: Segment, that: Option[Segment])(implicit queue: mutable.PriorityQueue[Event]): Unit = {
    that match {
      case None =>
      case Some(that) => seg intersect that match {
        case None =>
        case Some(i) =>
          val aboveBelow = seg aboveBelow that
          queue enqueue Intersection(aboveBelow._1, aboveBelow._2, i)
      }
    }
  }

  def apply(segments: List[Segment]): List[Intersection] = {

    implicit val queue = new mutable.PriorityQueue[Event]
    segments foreach {
      s => queue enqueue Left(s); queue enqueue Right(s)
    }
    val sweepLine = new SweepLine

    @tailrec
    def process(res: List[Intersection] = Nil): List[Intersection] = if (queue.nonEmpty) {
      queue.dequeue() match {
        case e@Left(seg) =>
          println("Got an event: " + e)
          sweepLine cross seg
          enqueueIntersection(seg, sweepLine above seg)
          enqueueIntersection(seg, sweepLine below seg)
          process(res)
        case e@Right(seg) =>
          println("Got an event: " + e)
          enqueueIntersection(sweepLine above seg, sweepLine below seg)
          sweepLine uncross seg
          process(res)
        case i@Intersection(above, below, point) =>
          println("Got an event: " + i)
          val (newAbove, newBelow) = sweepLine.handleIntersection(above, below)
          enqueueIntersection(newAbove, sweepLine above newAbove)
          enqueueIntersection(newBelow, sweepLine below newBelow)
          process(i :: res)
      }
    } else res


    process().reverse
  }
}
