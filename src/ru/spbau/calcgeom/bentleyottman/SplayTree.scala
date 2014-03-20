package ru.spbau.calcgeom.bentleyottman

import scala.annotation.tailrec
import scala.collection.immutable.Queue


object SplayTree {
  protected def splay[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) tree.left match {
      case n: Node[A] => left(n, x)
      case _ => tree
    }
    else if (cmp > 0) tree.right match {
      case n: Node[A] => right(n, x)
      case _ => tree
    }
    else tree
  }

  def left[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) tree.left match {
      case left: Node[A] => rightRollerCoaster(tree.parent, tree, splay(left, x))
      case _ => rightRotation(tree, tree.parent)
    }
    else if (cmp > 0) tree.right match {
      case right: Node[A] => rightZigZag(tree.parent, tree, splay(right, x))
      case _ => rightRotation(tree, tree.parent)
    }
    else rightRotation(tree, tree.parent)
  }

  def right[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) tree.left match {
      case left: Node[A] => leftZigZag(tree.parent, tree, splay(left, x))
      case _ => leftRotation(tree, tree.parent)
    }
    else if (cmp > 0) tree.right match {
      case right: Node[A] => leftRollerCoaster(tree.parent, tree, splay(right, x))
      case _ => leftRotation(tree, tree.parent)
    } else leftRotation(tree, tree.parent)
  }

  protected def rightZigZag[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
    val newX = leftRotation(parent, newRoot)
    grandParent.left = newX
    rightRotation(newX, grandParent)
  }

  protected def leftZigZag[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
    val newX = rightRotation(parent, newRoot)
    grandParent.right = newX
    leftRotation(newX, grandParent)
  }

  protected def rightRollerCoaster[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
    rightRotation(newRoot, rightRotation(parent, grandParent))
  }

  protected def leftRollerCoaster[A](newRoot: Node[A], parent: Node[A], grandParent: Node[A]): Node[A] = {
    leftRotation(leftRotation(grandParent, parent), newRoot)
  }

  protected def leftRotation[A](prevRoot: Node[A], newRoot: Node[A]): Node[A] = {
    prevRoot.right = newRoot.left
    newRoot.left = prevRoot
    newRoot
  }

  protected def rightRotation[A](newRoot: Node[A], prevRoot: Node[A]): Node[A] = {
    prevRoot.left = newRoot.right
    newRoot.right = prevRoot
    newRoot
  }

  object Tree {
    def makeRoot[T](elem: T): Node[T] = {
      val r = new Node[T](elem, null)()
      r.parent = r
      r
    }
  }

  abstract class Tree[+A] extends Alternative[Tree[A]] {


    val Zero = NilTree

    def ifDef[T >: A](f: (Node[T]) => Tree[T]): Tree[T] = this match {
      case NilTree => NilTree
      case n: Node[T] => f(n)
    }

    def +[T >: A](elem: T)(implicit ordering: Ordering[T]): Node[T] = this match {
      case NilTree => Tree.makeRoot(elem)
      case n: Node[T] => n insert elem; n
    }

    def remove [T >: A](elem: T)(implicit ordering: Ordering[T]): Tree[T] = this match {
      case NilTree => throw new IllegalStateException("Can not remove element from the empty tree")
      case n: Node[T] if n.isLeaf => if (n.key == elem) NilTree else throw new NoSuchElementException
      case n: Node[T] => n remove elem; n
    }
  }

  case object NilTree extends Tree[Nothing] {
    override val toString = "Nil"
  }

  case class Node[A](var key: A, var parent: Node[A])
                    (var left: Tree[A] = NilTree,
                     var right: Tree[A] = NilTree) extends Tree[A] with Iterable[A] {

    override def toString() = s"( $left { $key } $right )"

    def unapply[T >: A](t: Node[T]): Option[(Tree[T], Tree[T], Tree[T], T)] = Some(t.left, t.parent, t.right, t.key)

    def isLeftChild = parent.left == this

    def isLeaf = left == NilTree && right == NilTree

    def hasLeft = left != NilTree

    def hasRight = left != NilTree

    def isRightChild = parent.right == this

    @tailrec
    final def root: Node[A] = if (isRoot) this else parent.root

    def isRoot = parent == this

    @tailrec
    final def min: Node[A] = left match {
      case NilTree => this
      case n: Node[A] => n.min
    }

    @tailrec
    final def max: Node[A] = right match {
      case NilTree => this
      case n: Node[A] => n.max
    }


    def parents: Stream[Node[A]] = {
      @tailrec
      def moreParent(node: Node[A], s: Stream[Node[A]]): Stream[Node[A]] = if (!node.isRoot)
        moreParent(node.parent, Stream.cons(node, s))
      else s
      moreParent(this, Stream.empty[Node[A]])
    }

    def findNext: Tree[A] = right.ifDef(_ min) <|> parents.find(_ isLeftChild) match {
      case None => NilTree
      case Some(NilTree) => NilTree
      case Some(n: Node[A]) => n.parent.right ifDef (_ min)
    }


    def findPrev: Tree[A] = left.ifDef(_ max) <|> parents.find(_ isRightChild) match {
      case None => NilTree
      case Some(NilTree) => NilTree
      case Some(n: Node[A]) => n.parent.left ifDef (_ max)
    }

    def insertLeft(elem: A): Node[A] = {
      left = new Node[A](elem, this)()
      left.asInstanceOf[Node[A]]
    }

    def insertRight(elem: A): Node[A] = {
      right = new Node[A](elem, this)()
      right.asInstanceOf[Node[A]]
    }

    def insert(elem: A)(implicit ordering: Ordering[A]): Node[A] = {

      @tailrec
      def insertRoutine(node: Node[A], elem: A)(implicit ordering: Ordering[A]): Node[A] =
        ordering.compare(elem, node.key) match {
          case -1 => node.left match {
            case NilTree => node insertLeft elem
            case l: Node[A] => insertRoutine(l, elem)
          }
          case 0 => throw new IllegalArgumentException("Attempt to insert to a tree an element that is already stored in it: " + elem)
          case 1 => node.right match {
            case NilTree => node insertRight elem
            case r: Node[A] => insertRoutine(r, elem)
          }
          case _ => throw new IllegalArgumentException("Invalid ordering!")
        }

      splay(insertRoutine(this, elem), key)
    }


    def replaceContents(that: Node[A]) = {
      left = that.left
      left match {
        case NilTree =>
        case l: Node[A] => l.parent = this
      }
      right = that.right
      right match {
        case NilTree =>
        case r: Node[A] => r.parent = this
      }
      key = that.key
    }

    def hasOneChild = hasLeft ^ hasRight

    def remove(elem: A)(implicit ordering: Ordering[A]): Unit = find(elem) match {
      case NilTree => throw new NoSuchElementException
      case n: Node[A] => n.remove()
    }


    def remove(): Unit =
      (left,right) match {
        case (NilTree,NilTree) =>
          println(s"removing leaf $key ")
          if (isLeftChild) parent.left = NilTree
          if (isRightChild) parent.right = NilTree
          parent = this
        case (l:Node[A], NilTree) =>
          println( s"replacing contents of $key with ${l.key}")
          replaceContents(l)
        case (NilTree, r:Node[A]) =>
          println( s"replacing contents of $key with ${r.key}")
          replaceContents(r)
        case (l:Node[A], r:Node[A]) =>
          val toSwap = (findPrev <|> findNext).asInstanceOf[Node[A]]
          println( s"swapping $key with ${toSwap.key}")
          toSwap.swapKey(this)
          toSwap.remove()
      }


    def children = (left :: right :: Nil).filter(_ != NilTree).map(_.asInstanceOf[Node[A]])

    def swapKey(against: Node[A]) = {
      val t = key
      key = against.key
      against.key = t
    }

    def swap(efst: A, esnd: A)(implicit ordering: Ordering[A]) = {
      (find(efst), find(esnd)) match {
        case (fst: Node[A], snd: Node[A]) =>
          fst.key = esnd
          snd.key = efst
        case _ => throw new IllegalArgumentException(s"At least one of the elements to be swapped is not present: $efst $esnd")
      }

    }

    def find(elem: A)(implicit ordering: Ordering[A]): Tree[A] =
      ordering.compare(elem, key) match {
        case -1 => left ifDef (_ find elem)
        case 0 => this
        case 1 => right ifDef (_ find elem)
        case _ => throw new IllegalArgumentException("Invalid ordering!")
      }

    private class NodeIterator(tree: Node[A]) extends Iterator[A] {
      private[this] def addChildren(t: Node[A]): Unit = {
        right match {
          case NilTree => Nil
          case n: Node[A] => addChildren(n)
        }
        list = t.key :: list
        left match {
          case NilTree => Nil
          case n: Node[A] => addChildren(n)
        }
      }

      private[this] var list: List[A] = Nil
      addChildren(tree)

      def hasNext: Boolean = list.iterator.hasNext

      def next(): A = list.iterator.next()
    }

    def iterator: Iterator[A] = new NodeIterator(this)


    def bfs(node: Node[A]): Stream[Node[A]] = {
      val q = Queue.empty[Node[A]] enqueue node
      var stream = Stream.empty[Node[A]]

      @tailrec
      def loop(q: Queue[Node[A]]): Unit = {
        val (elem, queue) = q.dequeue
        stream = Stream cons(elem, stream)
        if (queue.nonEmpty) loop(queue enqueue elem.children)
      }

      loop(q)

      stream
    }
  }


}