package ru.spbau.calcgeom.bentleyottman

import scala.annotation.tailrec

object TSet {
  def empty[T <% Ordered[T]] = new TSet[T]

  def apply[T <% Ordered[T]](elems: T*): TSet[T] = {
    val acc = empty[T]
    elems.foreach(acc += _)
    acc
  }
}

class TSet[T <% Ordered[T]] extends scala.collection.mutable.Set[T] {

  sealed class ElementExistsException extends Exception

  var root: Tree = Nil

  def min = root.flatMap( n => Some(n.min) )
  def max = root.flatMap( n => Some(n.max) )
  sealed abstract class Tree {
    def flatMap(f: Node => Option[Node]): Option[Node]

    def find(e: T): Option[Node]

    def copy: Tree

    def toOption: Option[Node]

    def ifNode(f: (Node) => Unit): Unit

    def forceNode = asInstanceOf[Node]

    val isNode: Boolean
  }

  case object Nil extends Tree {
    override def flatMap(f: (Node) => Option[Node]): Option[Node] = None

    override def find(e: T): Option[Node] = None

    override def toOption: Option[Node] = None

    override def copy: Tree = Nil

    override val toString = ""

    override def ifNode(f: (TSet.this.type#Node) => Unit): Unit = {}

    val isNode = false
  }

  object ChildTypeEnum extends Enumeration {
    type ChildType = Value
    val LeftChild, RightChild, Root = Value
  }

  import ChildTypeEnum._

  sealed case class Node(var key: T, var left: Tree = Nil, var right: Tree = Nil, var parent: Tree = Nil) extends Tree {


    def childType: ChildType = parent match {
      case Nil => Root
      case Node(_, l, _, _) if l == this => LeftChild
      case Node(_, _, r, _) if r == this => RightChild
      case _ => throw new IllegalStateException()
    }

    def toOption = Some(this)

    def hasLeft = left != Nil

    def hasRight = right != Nil

    def isRoot = parent == Nil

    def isLeaf = !(hasLeft || hasRight)

    def isLeftChild = childType == LeftChild

    def isRightChild = childType == RightChild

    def add(elem: T): Node = elem compare key match {
      case -1 => left match {
        case Nil => left = new Node(elem, Nil, Nil, this); left.asInstanceOf[Node]
        case n: Node => n add elem
      }
      case 0 => throw new ElementExistsException
      case 1 => right match {
        case Nil => right = new Node(elem, Nil, Nil, this); right.asInstanceOf[Node]
        case n: Node => n add elem
      }
      case _ => throw new IllegalStateException("Invalid comparator")
    }

    def setLeft(node: Node) = {
      left = node
      node.parent = this
    }

    def setRight(node: Node) = {
      right = node
      node.parent = this
    }

    def ancestors: List[Node] = {
      @tailrec
      def getall(n: Node, list: List[Node] = List.empty[Node]): List[Node] = {
        n.parent match {
          case Nil => list
          case par: Node => getall(par, par :: list)
        }
      }
      getall(this).reverse
    }

    def family = this :: ancestors

    def next: Option[Node] = {
      val rightMin = right flatMap (r => Some(r.min))
      lazy val rightChildParent = family find (_ isLeftChild) flatMap (_.parent.toOption)
      rightMin orElse rightChildParent
    }

    def prev: Option[Node] = {
      val leftMax = left flatMap (l => Some(l.max))
      lazy val leftChildParent = family find (_ isRightChild) flatMap (_.parent.toOption)
      leftMax orElse leftChildParent
    }

    def swap(other: Node) = {
      val t = other.key
      other.key = key
      key = t
    }

    def delete(): Unit = {
      def replaceWith(other: Node) = {
        key = other.key
        left = other.left
        right = other.right
      }
      if (this == root && isLeaf) root = Nil
      else this match {
        case Node(_, Nil, Nil, _) => parent match {
          case p@Node(_, l, _, _) if l == this => p.left = Nil
          case p@Node(_, _, r, _) if r == this => p.right = Nil
        }
        case Node(_, n: Node, Nil, _) => replaceWith(n)
        case Node(_, Nil, n: Node, _) => replaceWith(n)
        case n => val toSwapWith = (prev orElse next).get
          toSwapWith swap this
          toSwapWith.delete()
      }
    }

    def min: Node = left match {
      case Nil => this
      case l: Node => l.min
    }

    def max: Node = right match {
      case Nil => this
      case r: Node => r.max
    }

    override def find(e: T): Option[Node] = e compare key match {
      case -1 => left find e
      case 0 => Some(this)
      case 1 => right find e
      case _ => throw new IllegalStateException("Invalid comparer")
    }


    override def flatMap(f: (TSet.this.type#Node) => Option[TSet.this.type#Node]): Option[TSet.this.type#Node] = f(this)

    override def copy: Tree = Node(key, left.copy, right.copy, parent)

    override def toString = s"{$left ($key) $right}"

    override def ifNode(f: (TSet.this.type#Node) => Unit): Unit = f(this)

    override val isNode: Boolean = true
  }

  def find(elem: T): Option[Node] = root find elem

  override def headOption = root match {
    case Nil => None
    case n:Node => Some( n.min.key )
  }

  override def lastOption = root match {
    case Nil => None
    case n:Node => Some(n.max.key)

  }
  override def iterator: Iterator[T] = {
    //can be way more efficient and lazy.
    var list = List.empty[T]
    def routine(t: Tree): Unit = t match {
      case Nil =>
      case n: Node =>
        routine(n.right)
        list ::= n.key
        routine(n.left)
    }
    routine(root)
    list.iterator
  }

  override def contains(elem: T): Boolean = root match {
    case Nil => false
    case n: Node => n.find(elem).isDefined
  }


  override def clone(): TSet[T] = {
    val set = new TSet[T]
    set.root = root.copy.asInstanceOf[set.Tree]
    set
  }

  override def -=(elem: T): this.type = {
    root find elem match {
      case None => throw new NoSuchElementException
      case Some(n: Node) =>   n.delete(); this
    }
  }

  override def +=(elem: T): this.type = {
    Splay(
      root match {
        case Nil => root = new Node(elem); root.asInstanceOf[Node]
        case n: Node => n.add(elem)
      }
    )
    this
  }

  def nextNode(elem: T): Option[Node] = root find elem match {
    case None => throw new NoSuchElementException
    case Some(node: Node) => node.next flatMap (n => Some(n))
  }

  def previousNode(elem: T): Option[Node] = root find elem match {
    case None => throw new NoSuchElementException
    case Some(node: Node) => node.prev flatMap (n => Some(n))
  }

  object Splay {

    def left(x: Node) = {
      val y = x.right.forceNode
      x.right = y.left
      y.left match {
        case n: Node => n.parent = x
        case Nil =>
      }
      y.parent = x.parent
      x.parent match {
        case Nil => root = y
        case p: Node => if (x == p.left) p.left = y else p.right = y
      }
      y.left = x
      x.parent = y
    }

    def right(x: Node) = {
      val y = x.left.forceNode
      x.left = y.right
      y.right match {
        case Nil =>
        case yr: Node => yr.parent = x
      }
      y.parent = x.parent
      x.parent match {
        case Nil => root = y
        case xp: Node => if (x == xp.left) xp.left = y else xp.right = y
      }
      y.right = x
      x.parent = y
    }

    def apply(x: Node): Unit ={
      while (x.parent.isNode) {
        val xp = x.parent.forceNode
        lazy val xpp = xp.parent.forceNode
        (x.childType, xp.childType) match {
          case (LeftChild, Root) => right(xp)
          case (RightChild, Root) => left(xp)
          case (LeftChild, LeftChild) => right(xpp); right(xp)
          case (RightChild, RightChild) => left(xpp); left(xp)
          case (LeftChild, RightChild) => right(xp); left(xp)
          case (RightChild, LeftChild) => left(xp); right(xp)
        }
      }
    }
  }

}