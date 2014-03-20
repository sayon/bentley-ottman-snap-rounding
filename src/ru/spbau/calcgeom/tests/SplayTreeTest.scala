package ru.spbau.calcgeom.tests

import org.junit._
import Assert._
import ru.spbau.calcgeom.bentleyottman.SplayTree.NilTree

class SplayTreeTest {
  @Test
  def createAndAdd() = {
    assertEquals(NilTree.toString, "Nil")
    assertEquals((NilTree + 4 + 2 + 1).toString(), "( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 4 } Nil )")
    assertEquals((NilTree + 4 + +6 + 42 + 222 + 3 + 9 + 2 + 1).toString(), "( ( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 3 } Nil ) { 4 } ( Nil { 6 } ( ( Nil { 9 } Nil ) { 42 } ( Nil { 222 } Nil ) ) ) )")
    assertEquals((NilTree + 1 + 4 + 2).toString(), "( Nil { 1 } ( ( Nil { 2 } Nil ) { 4 } Nil ) )")
  }

  @Test
  def remove() = {
    val tree = NilTree + 4 + 3 + 2 + 1 + 8
    assertEquals("( ( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 3 } Nil ) { 4 } ( Nil { 8 } Nil ) )",tree.toString())
    assertEquals("( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 4 } ( Nil { 8 } Nil ) )",(tree remove 3).toString)
    assertEquals("( ( Nil { 2 } Nil ) { 4 } ( Nil { 8 } Nil ) )", (tree remove 1).toString)
  }
}