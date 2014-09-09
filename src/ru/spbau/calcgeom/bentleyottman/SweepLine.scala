package ru.spbau.calcgeom.bentleyottman


import ru.spbau.calcgeom.bentleyottman.World.Segment

class SweepLine {
  private implicit def ordered(s: Segment) : Ordered[Segment] = new Ordered[Segment] {
    override def compare(that: Segment): Int = {
      val fst = s.yForX(getX)
      val snd = that.yForX(getX)
      (fst, snd) match {
        case (Some(x1), Some(x2)) => x1 compare x2
        case (_,_) => throw new IllegalStateException("Trying to compare segments that are not intersected by the current sweep line!")
      }
      0
    }
  }

  private def getX = x
  private var x: Double = Double.NegativeInfinity
  private[this] var buffer = TSet.empty[Segment]

  def above(seg: Segment): Option[Segment] = buffer.previousNode(seg) flatMap (n => Some(n.key))

  def below(seg: Segment): Option[Segment] = buffer.nextNode(seg) flatMap (n => Some(n.key))


  def cross(seg: Segment) = {
    x = seg.left.x
    println("crossing segment " + seg)
    buffer += seg
  }

  def uncross(seg: Segment) = {
    println("uncrossing segment " + seg)
    x = seg.right.x
    buffer -= seg
  }

  private[this] def orderedPair(fst: Segment, snd: Segment):(Segment, Segment) = {
    buffer.find(fst).collect (PartialFunction(_.next))
    def nextIs(fst: Segment, snd: Segment): Boolean = {
      buffer.find(fst) match {
        case None => throw new NoSuchElementException(s" No segment $fst! Sweepline: $buffer" )
        case Some(n) => n.key == snd
      }
    }
    if (nextIs(fst, snd)) (fst, snd) else if (nextIs(snd, fst)) (snd, fst) else throw new IllegalStateException()
    //    (buffer find fst, buffer find snd) match {
    //      case (sf@Some(f), ss@Some(s)) if f.next.isDefined && f.next.get.key == s.key => (f.key, s.key)
    //      case (sf@Some(f), ss@Some(s)) if s.next.isDefined && s.next.get.key == f.key => (s.key, f.key)
    //      case _ => throw new IllegalStateException(s"Can not determine which one is above, which one is below! fst: $fst snd: $snd, set: $buffer")
    //    }
  }

  def handleIntersection(fst: Segment, snd: Segment): (Segment, Segment) = {
    x = (fst intersect snd).get.x
    println(s"handling intersection between $fst and $snd")
    buffer.swapElements(fst, snd)
    orderedPair(fst, snd)
  }

}
