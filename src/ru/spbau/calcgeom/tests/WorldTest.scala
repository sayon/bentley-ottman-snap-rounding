package ru.spbau.calcgeom.tests

import org.junit._
import Assert._
import ru.spbau.calcgeom.bentleyottman.World

class SegmentsTest {

  import World._

  @Test
  def equality() = {
    assertEquals(new Segment((1.0, 1.0), (0.0, 0.0)), new Segment((1.0, 1.0), (0.0, 0.0)))
  }
}

