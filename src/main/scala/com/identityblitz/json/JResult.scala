package com.identityblitz.json

import scala.Predef._
import java.util.NoSuchElementException

/**
 *
 */

trait JResult[+T] {

  def fold[B](ef: Seq[String] => B)(sf: T => B): B = this match {
    case JSuccess(s) => sf(s)
    case JError(e) => ef(e)
  }

  def get: T

  def getOpt: Option[T] = this match {
    case JSuccess(s) => Option(s)
    case _ => None
  }

}

case class JSuccess[V](value: V) extends JResult[V] {

  def get: V = value

}

case class JError(errors: Seq[String]) extends JResult[Nothing] {

  def get = throw new NoSuchElementException("JError.get")
}

object JError {

  def apply(error: String) = new JError(Seq(error))

}

