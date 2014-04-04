package ru.spbau.calcgeom

import java.awt.Dimension
import com.sun.javaws.exceptions.InvalidArgumentException
import scala.Some

package object bentleyottman {


  val FORM_SIZE = new Dimension(800, 680)
  val DRAWING_AREA_SIZE = new Dimension(800, 680)


  //  implicit def withAlternative[T](o:Option[T]) = o.asInstanceOf[Option[T] with AlternativeOption[T]]


}
