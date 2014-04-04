package com.identityblitz.json

/**
 *
 */
object Json {

  def toJson[T](o: T)(implicit writer: JWriter[T]) = writer.write(o)

  def fromJson[T](v: JVal)(implicit reader: JReader[T]): T = reader.read(v).fold[T](e => throw new IllegalStateException(e.toString()))(v => v)

  sealed trait JValWrapper extends NotNull

  case class JValWrapperImpl(value: JVal) extends JValWrapper

  implicit def jval2JValWrapper[T](value: T)(implicit writer: JWriter[T]) = JValWrapperImpl(writer.write(value))

  def obj(fields: (String, JValWrapper)*): JObj = JObj(fields.map(e => (e._1, e._2.asInstanceOf[JValWrapperImpl].value)))

  def arr(elements: JValWrapper*): JArr = JArr(elements.map(_.asInstanceOf[JValWrapperImpl].value).toArray)

}
