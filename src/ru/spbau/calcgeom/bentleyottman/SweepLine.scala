//package ru.spbau.calcgeom.bentleyottman
//
//import ru.spbau.calcgeom.bentleyottman.SplayTree.{Node, Tree, NilTree}
//import ru.spbau.calcgeom.bentleyottman.World.Segment
//
//class SweepLine {
//  private[this] var buffer: Tree[Segment] = NilTree
//
//  def above(seg: Segment): Option[Segment] =
//    buffer match {
//      case NilTree => None
//      case n: Node[Segment] => n find seg match {
//        case None => throw new NoSuchElementException
//        case Some(r) => r.findPrev flatMap (a => Some(a key))
//        }
//      }
//
//
//  def below(seg: Segment): Option[Segment] =
//    buffer match {
//      case NilTree => None
//      case n: Node[Segment] => n find seg match {
//        case None => throw new NoSuchElementException
//        case Some(r) => r.findNext flatMap( b => Some(b key) )
//        }
//      }
//
//  def cross(seg: Segment) = {
//    buffer = buffer add seg
//  }
//
//  def uncross(seg: Segment) = {
//    buffer = buffer remove seg
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
