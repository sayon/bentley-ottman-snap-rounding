package ru.spbau.calcgeom.bentleyottman

import ru.spbau.calcgeom.bentleyottman.SplayTree.{Node, Tree, NilTree}
import ru.spbau.calcgeom.bentleyottman.World.Segment

class SweepLine {
  private[this] var buffer: Tree[Segment] = NilTree

  def above(seg: Segment): Option[Segment] =
    buffer match {
      case NilTree => None
      case n: Node[Segment] => n find seg match {
        case NilTree => throw new NoSuchElementException
        case r: Node[Segment] => r.findPrev match {
          case NilTree => None
          case a: Node[Segment] => Some(a.key)
        }
      }
    }

  def below(seg: Segment): Option[Segment] =
    buffer match {
      case NilTree => None
      case n: Node[Segment] => n find seg match {
        case NilTree => throw new NoSuchElementException
        case r: Node[Segment] => r.findNext match {
          case NilTree => None
          case a: Node[Segment] => Some(a.key)
        }
      }
    }

  def cross(seg: Segment) = {
    buffer add seg
  }

  def uncross(seg: Segment) = {
    buffer remove seg
  }

  def handleIntersection(fst: Segment, snd: Segment): (Segment, Segment) = {

    val b = buffer.asInstanceOf[Node[Segment]]
    b.swap(fst, snd)
    (b find fst, b find snd) match {
      case (f: Node[Segment], s: Node[Segment]) if f.findNext == s => (f.key, s.key)
      case (f: Node[Segment], s: Node[Segment]) if s.findNext == f => (s.key, f.key)
      case _ => throw new IllegalStateException("Can not determine which one is above, which one is below!")
    }
  }

}
