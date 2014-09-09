package ru.spbau.calcgeom.tests

import org.junit._
import ru.spbau.calcgeom.bentleyottman.World.Segment
import ru.spbau.calcgeom.bentleyottman.Intersections


class IntersectionsTest {
  @Test
  def twoSimpleLines() = {
    val fst = new Segment((-1.0, 0.0), (1.0, 0.0))
    val snd = new Segment((1.0, -1.0), (-1.0, 1.0))
    println(Intersections(fst :: snd :: Nil))
  }
}
