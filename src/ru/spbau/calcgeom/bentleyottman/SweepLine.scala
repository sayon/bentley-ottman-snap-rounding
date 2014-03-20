package ru.spbau.calcgeom.bentleyottman

import ru.spbau.calcgeom.bentleyottman.SplayTree.{Node, Tree, NilTree}
import ru.spbau.calcgeom.bentleyottman.World.Segment

class SweepLine {
  private[this] var buffer: Tree[Segment] = NilTree

  def above(seg:Segment): Option[Segment] =
    buffer match {
      case NilTree => None
      case n: Node[Segment] => n find seg match {
        case NilTree => None
        case r: Node[Segment] => r.findPrev match {
          case NilTree => None
          case a: Node[Segment] => Some(a.key)
        }
      }
    }

  def below(seg:Segment): Option[Segment] =
    buffer match {
      case NilTree => None
      case n: Node[Segment] => n find seg match {
        case NilTree => None
        case r: Node[Segment] => r.findNext match {
          case NilTree => None
          case a: Node[Segment] => Some(a.key)
        }
      }
    }

  def cross(seg: Segment) = {
    buffer += seg

  }

  def uncross(seg: Segment) = {
    buffer -= seg
  }
  def handleIntersection(fst: Segment, snd: Segment) = ???

}
