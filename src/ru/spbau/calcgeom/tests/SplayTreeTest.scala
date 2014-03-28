package ru.spbau.calcgeom.tests

import org.junit._
import Assert._
import ru.spbau.calcgeom.bentleyottman.SplayTree.{Node, Tree, NilTree}

class SplayTreeTest {
  @Test
  def objectCreation() = {
    assertEquals("( Nil { 1 } ( Nil { 2 } ( Nil { 3 } ( Nil { 4 } Nil ) ) ) )", Tree(1, 2, 3, 4))
  }

  @Test
  def createAndAdd() = {
    assertEquals(NilTree.toString, "Nil")
    assertEquals((NilTree add 4 add 2 add 1).toString(), "( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 4 } Nil )")
    assertEquals((NilTree add 4 add 6 add 42 add 222 add 3 add 9 add 2 add 1).toString(), "( ( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 3 } Nil ) { 4 } ( Nil { 6 } ( ( Nil { 9 } Nil ) { 42 } ( Nil { 222 } Nil ) ) ) )")
    assertEquals((NilTree add 1 add 4 add 2).toString(), "( Nil { 1 } ( ( Nil { 2 } Nil ) { 4 } Nil ) )")
  }

  def parentsOf[A](tree: Node[A]): String = {
    tree.parents.map(_.key.toString).fold("")(_ + " " + _)
  }

  @Test
  def parents() = {
    val tree = Tree(1, 2, 3, 4)

    assertEquals(" 1", parentsOf(tree.find(2).get))
    assertEquals(" 2 1", parentsOf(tree.find(3).get))
    assertEquals(" 3 2 1", parentsOf(tree.find(4).get))

  }

  @Test
  def nextAndPrev() = {
    val tree = Tree(4, 3, 2, 1, 8)
    def nextTo(i: Int) = tree.find(i).get.findNext.get.key.toString
    def prevTo(i: Int) = tree.find(i).get.findPrev.get.key.toString
    def nextPrev(prev: Int, next: Int) = {
      assertEquals(prev.toString, prevTo(next))
      assertEquals(next.toString, nextTo(prev))
    }
    nextPrev(1, 2)
    nextPrev(4, 8)

  }

  @Test
  def remove() = {
    val tree = Tree(4, 3, 2, 1, 8)
    assertEquals("( ( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 3 } Nil ) { 4 } ( Nil { 8 } Nil ) )", tree.toString())
    assertEquals("( ( ( Nil { 1 } Nil ) { 2 } Nil ) { 4 } ( Nil { 8 } Nil ) )", (tree remove 3).toString)
    assertEquals("( ( Nil { 2 } Nil ) { 4 } ( Nil { 8 } Nil ) )", (tree remove 1).toString)
  }
}