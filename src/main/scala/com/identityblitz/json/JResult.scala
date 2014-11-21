package com.identityblitz.json

import java.util.NoSuchElementException

/**
 *
 */

sealed trait JResult[+T] {

  def fold[B](ef: Seq[String] => B)(sf: T => B): B = this match {
    case JSuccess(s) => sf(s)
    case JError(e) => ef(e)
  }

  def get: T

  def getOpt: Option[T] = this match {
    case JSuccess(s) => Option(s)
    case _ => None
  }

  def map[B](f: T => B): JResult[B] = this match {
    case JSuccess(v) => JSuccess(f(v))
    case j: JError => j
  }

  def and[Z >: T, B, R](s: JResult[B])(implicit bld: (JSuccess[Z], JSuccess[B]) => JSuccess[R]) = this match {
    case j: JSuccess[Z] => j ++ s
    case j: JError => j ++ s
  }

  def $[R <: Applicative](implicit v: JResult[T] => R) = v(this)

}

trait Applicative

case class Applicative1[A1](t: JResult[A1]) extends Applicative { def lift[T](f: A1 => T): JResult[T] = t map f }
case class Applicative2[A1, A2](t: JResult[(A1, A2)]) extends Applicative { def lift[T](f: (A1, A2) => T): JResult[T] = t map f.tupled }
case class Applicative3[A1, A2, A3](t: JResult[(A1, A2, A3)]) extends Applicative { def lift[T](f: (A1, A2, A3) => T): JResult[T] = t map f.tupled }
case class Applicative4[A1, A2, A3, A4](t: JResult[(A1, A2, A3, A4)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4) => T): JResult[T] = t map f.tupled }
case class Applicative5[A1, A2, A3, A4, A5](t: JResult[(A1, A2, A3, A4, A5)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5) => T): JResult[T] = t map f.tupled }
case class Applicative6[A1, A2, A3, A4, A5, A6](t: JResult[(A1, A2, A3, A4, A5, A6)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6) => T): JResult[T] = t map f.tupled }
case class Applicative7[A1, A2, A3, A4, A5, A6, A7](t: JResult[(A1, A2, A3, A4, A5, A6, A7)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7) => T): JResult[T] = t map f.tupled }
case class Applicative8[A1, A2, A3, A4, A5, A6, A7, A8](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8) => T): JResult[T] = t map f.tupled }
case class Applicative9[A1, A2, A3, A4, A5, A6, A7, A8, A9](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => T): JResult[T] = t map f.tupled }
case class Applicative10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => T): JResult[T] = t map f.tupled }
case class Applicative11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => T): JResult[T] = t map f.tupled }
case class Applicative12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => T): JResult[T] = t map f.tupled }
case class Applicative13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => T): JResult[T] = t map f.tupled }
case class Applicative14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => T): JResult[T] = t map f.tupled }
case class Applicative15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => T): JResult[T] = t map f.tupled }
case class Applicative16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => T): JResult[T] = t map f.tupled }
case class Applicative17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => T): JResult[T] = t map f.tupled }
case class Applicative18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => T): JResult[T] = t map f.tupled }
case class Applicative19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => T): JResult[T] = t map f.tupled }
case class Applicative20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => T): JResult[T] = t map f.tupled }
case class Applicative21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => T): JResult[T] = t map f.tupled }
case class Applicative22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]) extends Applicative { def lift[T](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => T): JResult[T] = t map f.tupled }

import scala.language.implicitConversions

object JsonTools {

  implicit def jBuilder2[A1, A2](a1: JSuccess[A1], a2: JSuccess[A2]) = (a1, a2) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1, v2)) }
  implicit def jBuilder3[A1, A2, A3](a: JSuccess[(A1, A2)], b: JSuccess[A3]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v2)) }
  implicit def jBuilder4[A1, A2, A3, A4](a: JSuccess[(A1, A2, A3)], b: JSuccess[A4]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v2)) }
  implicit def jBuilder5[A1, A2, A3, A4, A5](a: JSuccess[(A1, A2, A3, A4)], b: JSuccess[A5]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v2)) }
  implicit def jBuilder6[A1, A2, A3, A4, A5, A6](a: JSuccess[(A1, A2, A3, A4, A5)], b: JSuccess[A6]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v2)) }
  implicit def jBuilder7[A1, A2, A3, A4, A5, A6, A7](a: JSuccess[(A1, A2, A3, A4, A5, A6)], b: JSuccess[A7]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v2)) }
  implicit def jBuilder8[A1, A2, A3, A4, A5, A6, A7, A8](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7)], b: JSuccess[A8]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v2)) }
  implicit def jBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, A9](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8)], b: JSuccess[A9]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v2)) }
  implicit def jBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9)], b: JSuccess[A10]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v2)) }
  implicit def jBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)], b: JSuccess[A11]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v2)) }
  implicit def jBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)], b: JSuccess[A12]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v2)) }
  implicit def jBuilder13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)], b: JSuccess[A13]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v2)) }
  implicit def jBuilder14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)], b: JSuccess[A14]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v2)) }
  implicit def jBuilder15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)], b: JSuccess[A15]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v2)) }
  implicit def jBuilder16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)], b: JSuccess[A16]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v2)) }
  implicit def jBuilder17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)], b: JSuccess[A17]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v2)) }
  implicit def jBuilder18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)], b: JSuccess[A18]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v1._17, v2)) }
  implicit def jBuilder19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)], b: JSuccess[A19]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v1._17, v1._18, v2)) }
  implicit def jBuilder20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)], b: JSuccess[A20]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v1._17, v1._18, v1._19, v2)) }
  implicit def jBuilder21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)], b: JSuccess[A21]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v1._17, v1._18, v1._19, v1._20, v2)) }
  implicit def jBuilder22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](a: JSuccess[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)], b: JSuccess[A22]) = (a, b) match { case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16, v1._17, v1._18, v1._19, v1._20, v1._21, v2)) }

  implicit def applicative1[A1](t: JResult[A1]) = Applicative1(t)
  implicit def applicative2[A1, A2](t: JResult[(A1, A2)]) = Applicative2(t)
  implicit def applicative3[A1, A2, A3](t: JResult[(A1, A2, A3)]) = Applicative3(t)
  implicit def applicative4[A1, A2, A3, A4](t: JResult[(A1, A2, A3, A4)]) = Applicative4(t)
  implicit def applicative5[A1, A2, A3, A4, A5](t: JResult[(A1, A2, A3, A4, A5)]) = Applicative5(t)
  implicit def applicative6[A1, A2, A3, A4, A5, A6](t: JResult[(A1, A2, A3, A4, A5, A6)]) = Applicative6(t)
  implicit def applicative7[A1, A2, A3, A4, A5, A6, A7](t: JResult[(A1, A2, A3, A4, A5, A6, A7)]) = Applicative7(t)
  implicit def applicative8[A1, A2, A3, A4, A5, A6, A7, A8](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8)]) = Applicative8(t)
  implicit def applicative9[A1, A2, A3, A4, A5, A6, A7, A8, A9](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]) = Applicative9(t)
  implicit def applicative10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]) = Applicative10(t)
  implicit def applicative11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]) = Applicative11(t)
  implicit def applicative12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]) = Applicative12(t)
  implicit def applicative13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]) = Applicative13(t)
  implicit def applicative14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]) = Applicative14(t)
  implicit def applicative15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]) = Applicative15(t)
  implicit def applicative16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]) = Applicative16(t)
  implicit def applicative17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]) = Applicative17(t)
  implicit def applicative18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]) = Applicative18(t)
  implicit def applicative19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]) = Applicative19(t)
  implicit def applicative20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]) = Applicative20(t)
  implicit def applicative21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]) = Applicative21(t)
  implicit def applicative22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](t: JResult[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]) = Applicative22(t)

}

case class JSuccess[V](value: V) extends JResult[V] {

  def get: V = value

  override def toString: String = "JSuccess(" + value + ")"

  def ++[B, R](s: JResult[B])(implicit bld: (JSuccess[V], JSuccess[B]) => JSuccess[R]) = s match {
    case v: JSuccess[B] => bld.apply(this, v)
    case e: JError => e
  }

}

case class JError(errors: Seq[String]) extends JResult[Nothing] {

  def get = throw new NoSuchElementException("JError.get")

  override def toString: String = "JError(" + errors + ")"

  def ++[B, R](s: JResult[B]) = s match {
    case JSuccess(_) => this
    case JError(e) => JError(errors ++ e)
  }

}

object JError {

  def apply(error: String) = new JError(Seq(error))

}

