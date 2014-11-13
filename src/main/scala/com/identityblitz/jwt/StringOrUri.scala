package com.identityblitz.jwt

import java.net.URI
import org.apache.commons.lang.StringUtils
import com.identityblitz.json._
import com.identityblitz.json.JSuccess

/**
 * This class represents StringOrUri type of JSON Web token
 */
sealed trait StringOrUri {

  def string: Option[String]

  def uri: Option[URI]

}

object StringOrUri {

  def apply(str: String) = {
    if(StringUtils.isBlank(str))
      throw new IllegalArgumentException("StringOrUri can not be blank.")
    if(str.indexOf(':') == -1) new StringVersion(str) else new UriVersion(new URI(str))
  }

  implicit object JStringOrUriReader extends JReader[StringOrUri] {
    def read(v: JVal): JResult[StringOrUri] = v match {
      case o: JStr => JSuccess(StringOrUri(o))
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object JStringOrUriDateWriter extends JWriter[StringOrUri] {
    def write(o: StringOrUri): JVal = JStr(o.toString)
  }

}

private sealed case class StringVersion(private val content: String) extends StringOrUri {
  def string: Option[String] = Option(content)

  def uri: Option[URI] = None

  override def toString: String = content.toString
}

private sealed case class UriVersion(private val content: URI) extends StringOrUri{
  def string: Option[String] = None

  def uri: Option[URI] = Option(content)

  override def toString: String = content.toString
}
