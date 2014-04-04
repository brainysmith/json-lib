package com.identityblitz.json

import scala.annotation.implicitNotFound

/**
 *
 */
@implicitNotFound("No JSON writer found for type ${T}. Try to implement an implicit JWriter.")
trait JWriter[-T] {

  def write(o: T): JVal

}

object JWriter extends DefaultJWriters

trait DefaultJWriters {

  implicit object IntJWriter extends JWriter[Int] {
    def write(o: Int): JVal = JNum(o)
  }

  implicit object StringJWriter extends JWriter[String] {
    def write(o: String): JVal = JStr(o)
  }

  implicit object BooleanJWriter extends JWriter[Boolean] {
    def write(o: Boolean): JVal = JBool(o)
  }

  implicit def arrayJWriter[T](implicit writer: JWriter[T]): JWriter[Array[T]] = new JWriter[Array[T]] {
    def write(o: Array[T]): JVal = JArr(o.map(t => Json.toJson(t)(writer)))
  }

  implicit def mapJWriter[T](implicit writer: JWriter[T]): JWriter[Map[String, T]] = new JWriter[Map[String, T]] {
    def write(o: Map[String, T]): JVal = JObj(o.map(e => (e._1, writer.write(e._2))).toSeq)
  }

  implicit object JValWriter extends JWriter[JVal] {
    def write(o: JVal): JVal = o
  }

  implicit def traversableJWriter[T : JWriter] = new JWriter[Traversable[T]] {
    def write(o: Traversable[T]): JVal = JArr(o.map(Json.toJson(_)).toArray)
  }

  implicit def optionJWrite[T : JWriter] = new JWriter[Option[T]] {
    def write(o: Option[T]): JVal = o match {
      case Some(v) => Json.toJson(v)
      case None => JUndef
    }
  }

}
