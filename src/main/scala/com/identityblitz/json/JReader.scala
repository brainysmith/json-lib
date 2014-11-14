package com.identityblitz.json

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom

@implicitNotFound("No JSON reader found for type ${T}. Try to implement an implicit JReader.")
trait JReader[T] {

  def read(v: JVal): JResult[T]

}

object JReader extends DefaultJReaders

trait DefaultJReaders {

  implicit object IntJReader extends JReader[Int] {
    def read(v: JVal): JResult[Int] = v match {
      case JNum(i) => JSuccess(i.toInt)
      case _ => JError("json.error.expected.number")
    }
  }

  implicit object LongJReader extends JReader[Long] {
    def read(v: JVal): JResult[Long] = v match {
      case JNum(i) => JSuccess(i.toLong)
      case _ => JError("json.error.expected.number")
    }
  }

  implicit object StringJReader extends JReader[String] {
    def read(v: JVal): JResult[String] = v match {
      case JStr(s) => JSuccess(s)
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object BooleanJReader extends JReader[Boolean] {
    def read(v: JVal): JResult[Boolean] = v match {
      case JBool(b) => JSuccess(b)
      case _ => JError("json.error.expected.boolean")
    }
  }

  implicit def arrayJReader[T : ClassTag](implicit reader: JReader[T]): JReader[Array[T]] = new JReader[Array[T]] {
    def read(v: JVal): JResult[Array[T]] = v match {
      case JArr(a) => JSuccess(a.map(e => e.as[T](reader)).toArray[T])
      case _ => JError("json.error.expected.array")
    }
  }

  implicit def mapJReader[T: ClassTag](implicit reader: JReader[T]): JReader[Map[String,T]] = new JReader[Map[String,T]] {
    def read(v: JVal): JResult[Map[String,T]] = v match {
      case JObj(a) => JSuccess(a.map(e => e._1.toString -> e._2.as[T](reader)).toMap[String,T])
      case _ => JError("json.error.expected.map")
    }
  }

  implicit object JValReader extends JReader[JVal] {
    def read(v: JVal): JResult[JVal] = JSuccess(v)
  }

  implicit object JNumReader extends JReader[JNum] {
    def read(v: JVal): JResult[JNum] = v match {
      case o: JNum => JSuccess(o)
      case _ => JError(("json.error.expected.number"))
    }
  }

  implicit object JStrReader extends JReader[JStr] {
    def read(v: JVal): JResult[JStr] = v match {
      case o: JStr => JSuccess(o)
      case _ => JError(("json.error.expected.string"))
    }
  }

  implicit object JBoolReader extends JReader[JBool] {
    def read(v: JVal): JResult[JBool] = v match {
      case o: JBool => JSuccess(o)
      case _ => JError(("json.error.expected.boolean"))
    }
  }

  implicit object JObjReader extends JReader[JObj] {
    def read(v: JVal): JResult[JObj] = v match {
      case o: JObj => JSuccess(o)
      case _ => JError(("json.error.expected.object"))
    }
  }

  implicit val higherKinded = scala.language.higherKinds

  implicit def traversableJReader[I[_], A](implicit cb: CanBuildFrom[I[_], A, I[A]], jr: JReader[A]) = new JReader[I[A]] {
    def read(v: JVal): JResult[I[A]] = {
      v match {
        case a: JArr => {
          val res = a.map(e => e).zipWithIndex.map{case (jv, idx) => jr.read(jv) match {
            case JSuccess(j) => Right(j)
            case JError(e) => Left("element(" + idx + "): " + e.mkString("; "))
          }}

          val errors = res.filter(_.isLeft)
          if(errors.isEmpty) {
            val builder = cb()
            res.foreach{case Right(e) => builder += e}
            JSuccess(builder.result())
          }
          else {
            JError(errors.map{case Left(e) => e}.toSeq)
          }
        }
        case s: JVal => jr.read(s) match {
          case JSuccess(j) => JSuccess((cb() += j).result())
          case JError(er) => JError(er)
        }
        case _ => JError("json.error.expected.array")
      }
    }
  }

  implicit def optionJReader[T](implicit reader: JReader[T]) = new JReader[Option[T]]{
    def read(v: JVal): JResult[Option[T]] = v match {
      case JUndef => JSuccess(None)
      case jv => reader.read(jv) match {
        case JSuccess(res) => JSuccess(Option(res))
        case e: JError => e
      }
    }
  }

}
