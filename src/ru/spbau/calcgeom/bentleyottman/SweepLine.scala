//package ru.spbau.calcgeom.bentleyottman
//
//
//import ru.spbau.calcgeom.bentleyottman.World.Segment
//
//class SweepLine {
//private var x : Double = Double.NegativeInfinity
//  private[this] var buffer = TSet.empty[Segment]
//
//  def above(seg: Segment): Option[Segment] = buffer.previousNode( seg ) flatMap( n=>Some(n.key))
//  def below(seg: Segment): Option[Segment] = buffer.nextNode( seg ) flatMap( n=>Some(n.key))
//
//
//
//  def cross(seg: Segment) = {
//    buffer += seg
//  }
//
//  def uncross(seg: Segment) = {
//    buffer -= seg
//  }
//
//  private[this] def orderedPair(fst: Segment, snd: Segment) = {
//    val b = buffer.asInstanceOf[Node[Segment]]
//    (b find fst, b find snd) match {
//      case (Some(f), Some(s)) if f.findNext == s => (f.key, s.key)
//      case (Some(f), Some(s)) if s.findNext == f => (s.key, f.key)
//      case _ => throw new IllegalStateException("Can not determine which one is above, which one is below!")
//    }
//  }
//
//  def handleIntersection(fst: Segment, snd: Segment): (Segment, Segment) = {
//    val b = buffer.asInstanceOf[Node[Segment]]
//    b.swap(fst, snd)
//    orderedPair(fst, snd)
//  }
//
//}
