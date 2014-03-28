package ru.spbau.calcgeom

import java.awt.Dimension
import com.sun.javaws.exceptions.InvalidArgumentException
import scala.Some

package object bentleyottman {

  implicit def richOption[T](o: Option[T]) = new {
    def ifDef(f: (T) => Unit): Unit = o match {
      case None =>
      case Some(t: T) => f(t)
      case _ => throw new InvalidArgumentException(Array("Argument type is invalid for ifDefined extension function!"))
    }
  }

  val FORM_SIZE = new Dimension(800, 680)
  val DRAWING_AREA_SIZE = new Dimension(800, 680)


  //  implicit def withAlternative[T](o:Option[T]) = o.asInstanceOf[Option[T] with AlternativeOption[T]]


}

object Alternative {
  implicit def apply[T](o: Option[T]) = new Alternative[Option[T]] {
    override val Zero: Option[T] = None
    override val thiz:Option[T] = o
  }
}

trait Alternative[+T]  {
  val Zero: T
  val thiz: T
  def <|>[U >: T](that: Alternative[U]): U = thiz match {
    case Zero => that.thiz
    case e: T => e
    case _ => throw new IllegalStateException("WTF")
  }
}

trait AlternativeOption[T] extends Alternative[Option[T]] {
  val Zero = None
}