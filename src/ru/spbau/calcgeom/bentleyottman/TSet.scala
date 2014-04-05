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

  sealed abstract class Tree {
    def flatMap(f: Node => Option[Node]): Option[Node]

    def find(e: T): Option[Node]

    def copy: Tree

    def toOption: Option[Node]
  }

  case object Nil extends Tree {
    override def flatMap(f: (Node) => Option[Node]): Option[Node] = None

    override def find(e: T): Option[Node] = None

    override def toOption: Option[Node] = None

    override def copy: Tree = Nil

    override val toString = ""
  }

  sealed case class Node(var key: T, var left: Tree = Nil, var right: Tree = Nil, var parent: Tree = Nil) extends Tree {
    def toOption = Some(this)

    def hasLeft = left != Nil

    def hasRight = right != Nil

    def isRoot = parent == Nil

    def isLeaf = !(hasLeft || hasRight)

    def isLeftChild = parent match {
      case Node(_, l, _, _) if l == this => true
      case _ => false
    }

    def isRightChild = parent match {
      case Node(_, _, r, _) if r == this => true
      case _ => false
    }

    def add(elem: T): Node = {
      def addRoutine(elem: T): Node = {
        elem compare key match {
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
      }
      addRoutine(elem)
      this
    }

    def ancestors: List[Node] = {
      @tailrec
      def getall(n: Node, list: List[Node] = List.empty[Node]): List[Node] = {
        n.parent match {
          case Nil => list
          case par: Node => getall(par, par :: list)
        }
      }
      getall(this)
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
      if (this == root) root = Nil
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
  }

  def find(elem: T): Option[Node] = root find elem

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
      case Some(n: Node) => n.delete(); this
    }
  }

  override def +=(elem: T): this.type = {
    root match {
      case Nil => root = new Node(elem)
      case n: Node => n.add(elem)
    }
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

}


//
//object SplayTree {
//
//  class ElementExistsException extends Exception {
//    override val getMessage = "Such element exists"
//  }
//
//  protected def splay[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
//    val cmp = ordering.compare(x, tree.key)
//    if (cmp < 0) tree.left match {
//      case n: Node[A] => left(n, x)
//      case _ => tree
//    }
//    else if (cmp > 0) tree.right match {
//      case n: Node[A] => right(n, x)
//      case _ => tree
//    }
//    else tree
//  }
//
//  def left[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
//    val cmp = ordering.compare(x, tree.key)
//    if (cmp < 0) tree.left match {
//      case left: Node[A] => rightRollerCoaster(tree.parent.forceNode, tree, splay(left, x))
//      case _ => rightRotation(tree, tree.parent.forceNode)
//    }
//    else if (cmp > 0) tree.right match {
//      case right: Node[A] => rightZigZag(tree.parent.forceNode, tree, splay(right, x))
//      case _ => rightRotation(tree, tree.parent.forceNode)
//    }
//    else rightRotation(tree, tree.parent.forceNode)
//  }
//
//  def right[A](tree: Node[A], x: A)(implicit ordering: Ordering[A]): Node[A] = {
//    val cmp = ordering.compare(x, tree.key)
//    if (cmp < 0) tree.left match {
//      case left: Node[A] => leftZigZag(tree.parent.forceNode, tree, splay(left, x))
//      case _ => leftRotation(tree, tree.parent.forceNode)
//    }
//    else if (cmp > 0) tree.right match {
//      case right: Node[A] => leftRollerCoaster(tree.parent.forceNode, tree, splay(right, x))
//      case _ => leftRotation(tree, tree.parent.forceNode)
//    } else leftRotation(tree, tree.parent.forceNode)
//  }
//
//  protected def rightZigZag[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
//    val newX = leftRotation(parent, newRoot)
//    grandParent.left = newX
//    grandParent.left match {
//      case Nil =>
//      case n: Node[A] => n.parent = grandParent
//    }
//    rightRotation(newX, grandParent)
//  }
//
//  protected def leftZigZag[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
//    val newX = rightRotation(parent, newRoot)
//    grandParent.right = newX
//    grandParent.right match {
//      case Nil =>
//      case n: Node[A] => n.parent = grandParent
//    }
//    leftRotation(newX, grandParent)
//  }
//
//  protected def rightRollerCoaster[A](grandParent: Node[A], parent: Node[A], newRoot: Node[A]): Node[A] = {
//    rightRotation(newRoot, rightRotation(parent, grandParent))
//  }
//
//  protected def leftRollerCoaster[A](newRoot: Node[A], parent: Node[A], grandParent: Node[A]): Node[A] = {
//    leftRotation(leftRotation(grandParent, parent), newRoot)
//  }
//
//  protected def leftRotation[A](prevRoot: Node[A], newRoot: Node[A]): Node[A] = {
//    prevRoot.right = newRoot.left
//
//    newRoot.left = prevRoot
//    prevRoot.right match {
//      case Nil =>
//      case n: Node[A] => n.parent = prevRoot
//    }
//    newRoot.left match {
//      case Nil =>
//      case n: Node[A] => n.parent = prevRoot
//    }
//    newRoot
//  }
//
//  protected def rightRotation[A](newRoot: Node[A], prevRoot: Node[A]): Node[A] = {
//    prevRoot.left = newRoot.right
//    newRoot.right = prevRoot
//    prevRoot.left match {
//      case Nil =>
//      case n: Node[A] => n.parent = prevRoot
//    }
//    newRoot.right match {
//      case Nil =>
//      case n: Node[A] => n.parent = prevRoot
//    }
//    newRoot
//  }
//
//
//  //  class TreeSet[+T <% Ordered[T]] {
//  //    private[this] var tree: Tree = Nil
//  //
//  //    protected def _add(e: T) =
//  //      tree = tree add e
//  //
//  //    def remove(e: T) = ???
//  //  }
//
//  object Tree {
////    def apply[T](elems: T*)(implicit ordering: Ordering[T]): Node =
////      elems.foldLeft[Tree](Nil)((node: Tree, el) => node.add(el)).asInstanceOf[Node]
//  }
//
//  abstract class Tree[+T <% Ordered[T]] {
//
//    def add[U >: T <: Ordered[U]](elem: U): Node[U]
//
//    def find[U >: T <: Ordered[U]] (elem: U): Option[Node[U]]
//
////    def remove(): Tree
//
//    def toOption[U >: T] = this match {
//      case Nil => None
//      case n: Node => Some(n)
//    }
//
//    def forceNode = asInstanceOf[Node]
//
//  }
//
//  case object Nil extends Tree[Nothing] {
//
////    override def add[U >: Nothing <% Ordered[U]](elem: U): Node[U] = new Node[U](elem, Nil, Nil, Nil)
//
//   // override def find(elem: Nothing): Option[SplayTree.this.type#Node[Nothing]] = None
//
//    override def toString = ""
//
//    def remove(): Tree[Nothing] = throw new NoSuchElementException
//
////    override def findAndRemove(e: Nothing): Tree[Nothing] = throw new NoSuchElementException
//
//    //override def find[U >: Nothing <: Ordered[Nothing]](elem: U): Option[Node[U]] = None
//    override def add[U >: Nothing <: Ordered[U]](elem: U): Node[U] = ???
//
//    override def find[U >: Nothing <: Ordered[U]](elem: U): Option[Node[U]] = ???
//  }
//
//  case class Node[T <% Ordered[T]]( key: T, var left: Tree, var right: Tree, var parent: Tree = Nil) extends Tree {
//    def isRoot = parent == Nil
//
//    def hasLeft = left != Nil
//
//    def hasRight = right != Nil
//
//    def isLeftChild = parent match {
//      case Node(_, l, _, _) if l == this => true
//      case _ => false
//    }
//
//    def isRightChild = parent match {
//      case Node(_, _, r, _) if r == this => true
//      case _ => false
//    }
//
//    def isLeaf = !(hasLeft || hasRight)
//
//    def ancestors: List[Node] = {
//      @tailrec
//      def getall(n: Node, list: List[Node] = List.empty): List[Node] = {
//        n.parent match {
//          case Nil => list
//          case par@Node(_, _, _, p) => getall(par, par :: list)
//        }
//      }
//      getall(this)
//    }
//
//    def family = this :: ancestors
//
//    private def insertLeft(elem: T): Node = {
//      val l = new Node(elem, Nil, Nil, this)
//      left = l
//      l
//    }
//
//    private def insertRight(elem: T): Node = {
//      val r = new Node(elem, Nil, Nil, this)
//      right = r
//      r
//    }
//
//    override def add(elem: T): Node = {
//      def addRoutine(elem: T): Node = {
//        elem compare key match {
//          case -1 => left match {
//            case Nil => asInstanceOf[Node].left = new Node(elem, Nil, Nil, this); left.asInstanceOf[Node] //insertLeft(elem)
//            case n => n add elem
//          }
//          case 0 => throw new ElementExistsException
//          case 1 => right match {
//            case Nil => asInstanceOf[Node].right = new Node(elem, Nil, Nil, this); right.asInstanceOf[Node] //insertLeft(elem)
//            case n => n add elem
//          }
//          case _ => throw new IllegalStateException("Invalid comparator")
//        }
//      }
//      addRoutine(elem)
//      splay(this, elem)
//    }
//
//
//    private def detachFromParent() =
//      parent match {
//        case Nil =>
//        case p@Node(_, l, _, _) if l == this => p.left = Nil
//        case p@Node(_, _, r, _) if r == this => p.right = Nil
//      }
////
////    private def replaceWith(other: Node) = {
////      parent = other.parent
////      left = other.left
////      right = other.right
////      key = other.key
////    }
//
//    def min: Node = left match {
//      case Nil => this
//      case l: Node => l.min
//    }
//
//    def max: Node = right match {
//      case Nil => this
//      case r: Node => r.max
//    }
//
//    def next: Option[Node] = {
//      val rightMin = right.toOption flatMap (r => Some(r.min))
//      lazy val rightChildParent = family find (_ isRightChild) flatMap (_.parent.toOption)
//      rightMin orElse rightChildParent
//    }
//
////    def swap(other: Node): Unit = {
////      val k = key
////      key = other.key
////      other.key = k
////    }
//
//    def prev: Option[Node] = {
//      val leftMax = left.toOption flatMap (l => Some(l.min))
//      lazy val leftChildParent = family find (_ isRightChild) flatMap (_.parent.toOption)
//      leftMax orElse leftChildParent
//    }
//
//
//    final def find[U >: T <: Ordered[U]](elem: U): Option[Node[U]] =  elem compare key match {
//      case 0 => Some(this)
//      case -1 => right.find(elem)
//      case 1 => left.find(elem)
//      case _ => throw new IllegalStateException("Invalid comparator implemented for " + elem.getClass.getName)
//    }
//
//    override def toString = s"{$left ($key) $right}"
//
////
////    override def remove(): Tree = this match {
////      case Node(_, _, _, Nil) => Nil
////      case Node(_, Nil, Nil, p: Node) => detachFromParent(); this
////      case Node(_, l: Node, Nil, p) => replaceWith(l); this
////      case Node(_, Nil, r: Node, p) => replaceWith(r); this
////      case Node(k, l, r, p) =>
////        next orElse prev match {
////          case None => throw new IllegalStateException("No previous or next element to remove, but the tree has more, than one element!")
////          case Some(toSwap) => swap(toSwap); toSwap.remove()
////        }
////  }
//
////    override def findAndRemove(e: T): Tree = find ( e) match {
////      case None => throw new NoSuchElementException
////      case Some(n) => n.remove()
////    }
//  }
//}
//
