package com.identityblitz.jwt

import com.identityblitz.json._
import com.identityblitz.json.JSuccess
import org.joda.time.DateTime

/**
 * This class represents IntDate type of JSON Web Token. The type contain the number of seconds from 1970-01-01T0:0:OZ UTC
 * until the specified UTC date/time.
 */
sealed case class IntDate(value: Int) {

  if (value <= 0) throw new IllegalArgumentException("The number of second from epoch must be non negative.")

  def before(d: IntDate): Boolean = d.value < value

  def after(d: IntDate): Boolean = d.value > value

  override def toString: String = value.toString
}

object IntDate {

  implicit object JIntDateReader extends JReader[IntDate] {
    def read(v: JVal): JResult[IntDate] = v match {
      case o: JNum => JSuccess(IntDate(o.as[Int]))
      case _ => JError("json.error.expected.number")
    }
  }

  implicit object JIntDateWriter extends JWriter[IntDate] {
    def write(o: IntDate): JVal = JNum(o.value)
  }

  def now: IntDate = IntDate((new DateTime().getMillis / 1000).toInt)

}
