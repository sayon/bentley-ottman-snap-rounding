package ru.spbau.calcgeom.bentleyottman

object World {

  abstract class PlaneObject


  implicit def DotFromTuple(t: (Double, Double)) = new Dot(t._1, t._2)

  implicit def tupleFromDot(d: Dot) = (d.x, d.y)

  class Circle(val center: Dot, val radius: Double) extends PlaneObject

  def doubleTriangleSquare(a: Dot, b: Dot, c: Dot): Double = (a.x - c.x) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)

  def triangleSquare(a: Dot, b: Dot, c: Dot) = doubleTriangleSquare(a, b, c) / 2.0

  def leftTurn(a: Dot, b: Dot, check: Dot) = doubleTriangleSquare(a, b, check) >= 0
  def above(seg:Segment)(check:Dot) = leftTurn(seg.left, seg.right, check)

  //a.x * b.y * c.z  + a.y * b.z * c.x + b.x * c.y * a.z - a.z*b.y
  case class Dot(x: Double, y: Double) extends PlaneObject with Ordered[Dot] {

    protected[this] def component(f: (Double, Double) => Double)(that: Dot): Dot = new Dot(f(x, that.x), f(y, that.y))

    def +(that: Dot): Dot = component(_ + _)(that)

    def -(that: Dot): Dot = component(_ - _)(that)

    def *(that: Dot): Double = x * that.x + y * that.y

    def X(that: Dot): Double = x * that.y - y * that.x

    def /(that: Dot): Dot = component(_ / _)(that)

    def /(scalar: Double): Dot = new Dot(x / scalar, y / scalar)

    override val toString = s"($x, $y)"

    override def equals(that: Any) = that match {
      case dot: Dot => x == dot.x && y == dot.y
      case _ => false
    }

    override def hashCode = x.hashCode ^ (y.hashCode * 31)

    def compare(that: Dot): Int = {
      val xcomp = x compare that.x
      if (xcomp != 0) xcomp else y compare that.y
    }

    def unapply: (Double, Double) = (x, y)
  }

  type CartesianPlane = List[PlaneObject]

  def vertical( x: Double )(implicit seg: Segment) = new Segment((x,seg.topBox), (x,seg.botBox))

  class Segment(fst: Dot, snd: Dot) extends PlaneObject  {
    val left = if (fst.x < snd.x) fst else snd
    val right = if (fst == left) snd else fst

    val topBox = left.y max right.y
    val botBox = left.y min right.y

    val leftBox = left.x
    val rightBox = right.x

    implicit def toPair: (Dot, Dot) = (left, right)

    def yForX(x: Double) : Option[Double] = {
      if (x < left.x || x > right.x ) None
      else intersect(vertical(x)(this)) flatMap (d => Some(d.x))
    }
//    def compare(that: Segment): Int = {
//      val leftcomp = this.left compare that.left
//      if (leftcomp != 0) leftcomp else this.right compare that.right
//    }

//    def minAndMax(that: Segment): (Segment, Segment) = if (compare(that) == 1) (that, this) else (this, that)

    def aboveBelow(that: Segment): (Segment, Segment) = {
      if (left.y > that.left.y) (this, that)
      else (that, this)
    }

    def crosses(that: Segment): Boolean = leftTurn(left, right, that.left) ^ leftTurn(left, right, that.right)

    lazy val coords = right - left

    implicit def RichDouble(d: Double) = new {
      val EPSILON = 0.00000000001

      def approx(other: Double) = Math.abs(other - d) < EPSILON
    }


    def intersect(that: Segment): Option[Dot] = {

      val (s1_x, s1_y) = this.coords.unapply
      val (s2_x, s2_y) = that.coords.unapply
      val (p0_x, p0_y) = this.left.unapply
      val (p2_x, p2_y) = that.left.unapply

      val s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y)
      val t = (s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y)

      if (s1_x * s2_y - s2_x * s1_y approx 0.0) //collinear!
        None
      else if (s >= 0.0 && s <= 1.0 && t >= 0.0 && t <= 1.0)
        Some(new Dot(p0_x + t * s1_x, p0_y + t * s1_y))
      else None
    }

    override def equals (that: Any) =  that match {
      case that: Segment => left == that.left && right == that.right
      case _ => false
    }

    lazy override val toString = s"[ $left , $right ]"
  }

  class Frame(val leftTop: Dot, val size: (Double, Double)) {
    lazy val leftBot = new Dot(left, bot)
    lazy val rightBot = new Dot(left, bot)
    lazy val rightTop = new Dot(right, bot)
    lazy val left: Double = leftTop.x
    lazy val right: Double = leftTop.x + size._1
    lazy val top: Double = leftTop.y
    lazy val bot: Double = leftTop.y - size._2

    lazy val width: Double = right - left
    lazy val height: Double = top - bot

    lazy val center: Dot = new Dot((right - left) / 2, (top - bot) / 2)

    def isInside(d: Dot) = d.x >= left && d.x <= right && d.y <= top && d.y >= bot

    def project(worldDot: Dot) = worldDot - leftBot

    def scale(sx: Double, sy: Double) = new Frame(
      new Dot(center.x - size._1 * sx / 2, center.y + size._2 * sy / 2),
      (size._1 * sx, size._2 * sy)
    )

    def resize(sx: Double, sy: Double) = new Frame(
      leftTop,
      ((size._1 * sx).toInt, (size._2 * sy).toInt)
    )

    def transpose(delta: Dot) = new Frame(leftTop + delta, size)
  }


}