package ru.spbau.calcgeom.tests

import org.junit._
import org.junit.Assert._
import ru.spbau.calcgeom.bentleyottman.TSet

//import ru.spbau.calcgeom.bentleyottman.SplayTree._

class SplayTreeTest {

  @Test
  def create() = {
    val s = TSet(4, 3, 5, 2, 1, 9)
    assertEquals("Set(1, 2, 3, 4, 5, 9)", s.toString())

  }

  @Test
  def add() = {
    val s = TSet(4, 3, 5, 2, 1, 9)
    assertEquals("Set(1, 2, 3, 4, 5, 9)", s.toString())
    s += 6
    assertEquals("Set(1, 2, 3, 4, 5, 6, 9)", s.toString())
  }

  @Test
  def remove() = {
    val s = TSet(4, 3, 5, 2, 1, 9)
    s -= 5
    assertEquals("Set(1, 2, 3, 4, 9)", s.toString())
  }

  @Test
  def prevNext() = {
    def check[T](prv: T, nxt: T)(implicit set: TSet[T]) = {
      println( prv.toString + " parents " + (set find prv).get.ancestors.map (_.key.toString) )
      println( nxt.toString + " parents " + (set find nxt).get.ancestors.map (_.key.toString) )
      assertEquals((set previousNode  nxt).get.key.toString, prv.toString)
      assertEquals((set nextNode prv).get.key.toString, nxt.toString)
    }

    implicit val s = TSet(4, 3, 5, 2, 1, 9)
    println(s.root)
    check(2, 3)
//    check(4, 5)
//    check(5, 9)
  }
}