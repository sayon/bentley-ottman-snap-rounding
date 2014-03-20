package ru.spbau.calcgeom.bentleyottman

import World._
import ru.spbau.calcgeom.bentleyottman.World.Segment
import scala.collection.mutable
//
//object SegmentIntersectionsSolver {
//
//  protected abstract class Event(val dot: Dot) extends Ordered[Event] {
//    def compare(that: Event): Int = -(dot compare that.dot)
//  }
//
//  protected case class LeftEndpoint(segment: Segment) extends Event(segment.left)
//
//  protected case class RightEndpoint(segment: Segment) extends Event(segment.right)
//
//  protected case class Intersection(d: Dot, segments: (Segment, Segment)) extends Event(d)
//
//  protected object Intersection {
//    def apply(fst: Segment, snd: Segment): Option[Intersection] =
//      fst intersect snd match {
//        case None => None
//        case Some(d) => Some(new Intersection(d, fst aboveBelow snd))
//      }
//  }
//
//  //fuck fuck fuck
//  //we need some kind of a binary tree that is both autobalancing and that we can change easily.
//  // to insert a segment, we check its left (x,y) and test it against all other segments crossed right now using left turn.
//  //so a sweep line is a such binary tree that uses a custom predicate to determine where to insert thepoint
//  //init
//
//
//  def apply(segments: List[Segment]): List[Intersection] = {
//
//    val sweepline = new SweepLine(segments.size)
//
//    val events: mutable.PriorityQueue[Event] = new mutable.PriorityQueue[SegmentIntersectionsSolver.this.type#Event]()
//
//    segments.foreach(s => {
//      events.enqueue(new LeftEndpoint(s))
//      events.enqueue(new RightEndpoint(s))
//    }
//    )
//
//    var intersections: List[Intersection] = Nil
//
//    while (!events.isEmpty) {
//      events dequeue() match {
//        case vertex: LeftEndpoint =>
//          val s = vertex.segment
//          sweepline cross s
//          sweepline getAbove s match {
//            case None =>
//            case Some(a) => Intersection(s, a) ifDef {
//              events enqueue _
//            }
//          }
//
//        case vertex: RightEndpoint =>
//          val s = vertex.segment
//          val (b, a) = (sweepline getBelow s, sweepline getAbove s)
//          sweepline uncross s
//          if (b.isDefined && a.isDefined) Intersection(a.get, b.get) ifDef {
//            events enqueue _
//          }
//
//        case intersection: Intersection =>
//          intersections = intersection :: intersections
//
//          sweepline.flipNeighbours(intersection.segments._1, intersection.segments._2)
//
//          val (above, below) = sweepline.aboveBelow(intersection.segments._1, intersection.segments._2)
//          sweepline getAbove above match {
//            case None =>
//            case Some(aboveabove) => Intersection(aboveabove, above) match {
//              case None =>
//              case Some(i) => events enqueue i
//            }
//          }
//
//          sweepline getBelow below match {
//            case None =>
//            case Some(belowbelow) => Intersection(below, belowbelow) match {
//              case None =>
//              case Some(i) => events enqueue i
//            }
//          }
//      }
//    }
//    intersections
//  }
//}


/*private[this] class SweepLine(segCount: Int) {
//
//    class SegmentHolder(val segment: Segment, var prev: Option[SegmentHolder] = None, var next: Option[SegmentHolder] = None) {
//      def swap(that: SegmentHolder) = {
//        val tprev = prev
//        prev = that.prev
//        that.prev = tprev
//        val tnext = next
//        next = that.next
//        that.next = tnext
//      }
//
//    }
//
//    private[this] var buffer = new TreeMap[Segment, SegmentHolder]
//
//    def cross(seg: Segment) = {
//      val holder = new SegmentHolder(seg)
//
//      if (buffer.isEmpty) buffer = buffer + ((seg, holder))
//      else {
//        val exlast = buffer.last._2
//        val exfirst = buffer.head._2
//
//        buffer = buffer + ((seg, holder))
//
//
//        val iter = buffer.iteratorFrom(seg)
//        iter.next()
//
//        val (p: Option[SegmentHolder], n: Option[SegmentHolder]) = if (holder == buffer.head._2) //before start
//          (None, Some(exfirst))
//        else if (iter.hasNext) {
//          //in the middle somewhere
//          val nextHolder = iter.next()._2
//          (Some(nextHolder.prev), Some(nextHolder))
//        }
//        else (Some(exlast), None) //after last
//
//        holder.prev = p
//        holder.next = n
//        holder.prev.ifDef {
//          _.next = Some(holder)
//        }
//        holder.next.ifDef {
//          _.prev = Some(holder)
//        }
//      }
//    }
//
//    def uncross(seg: Segment) = {
//      val holder = buffer(seg)
//      holder.prev.ifDef {
//        _.next = holder.next
//      }
//      holder.next.ifDef {
//        _.prev = holder.prev
//      }
//      buffer = buffer - seg
//    }
//
//    // above ... below
//    def getBelow(seg: Segment): Option[Segment] = {
//      if (buffer.contains(seg)) buffer(seg).next match {
//        case None => None
//        case Some(h) => Some(h.segment)
//      }
//      else None
//    }
//
//    def getAbove(seg: Segment): Option[Segment] = {
//      if (buffer.contains(seg)) buffer(seg).prev match {
//        case None => None
//        case Some(h) => Some(h.segment)
//      }
//      else None
//    }
//
//    private[this] def areNeighbours(fst: SegmentHolder, snd: SegmentHolder): Boolean =
//      (fst.next.isDefined && fst.next == snd) || (snd.next.isDefined && snd.next == fst)
//
//
//    def aboveBelow(fst: Segment, snd: Segment): (Segment, Segment) = if (buffer.contains(fst) && buffer.contains(fst)) {
//      val hfst = buffer(fst)
//      val hsnd = buffer(snd)
//      if (areNeighbours(hfst, hsnd)) {
//        if (hfst.next == hsnd) (fst, snd)
//        else (snd, fst)
//      }
//      else throw new IllegalStateException(s"can't check who is above and who is beyond (not neighbours): $fst $snd")
//    }
//    else throw new IllegalStateException(s"at least one of those segments is not crossed atm: $fst $snd")
//
//    def flipNeighbours(s1: Segment, s2: Segment) =
//      if (buffer.contains(s1) && buffer.contains(s2)) {
//        val fst = buffer(s1)
//        val snd = buffer(s2)
//        if (areNeighbours(fst, snd))
//          fst swap snd
//      }
//  }
*/