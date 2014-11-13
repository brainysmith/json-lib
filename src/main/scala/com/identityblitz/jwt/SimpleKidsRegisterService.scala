package com.identityblitz.jwt

import org.apache.commons.codec.binary.Base64
import scala.util.Try
import com.identityblitz.json.JVal

trait SimpleKidsRegisterService extends JwkToolkit {

  object SimpleKidsRegister extends KidsRegister {

    private val store = Option(getClass.getClassLoader.getResource("default.jwkset"))
      .flatMap(u => Try(JVal.parse(u.openStream()).as[Array[JWK]]).toOption)
      .map(a => a.toSeq.groupBy(_.alg.getOrElse("*"))).getOrElse(Map.empty)

    private val defaults = store.getOrElse("*", Seq())

    def apply(kid: String): JWK = ???

    def get(kid: String): Option[JWK] = ???

    def defaultKids(alg: String): Seq[JWK] = store.getOrElse(alg, Seq.empty) ++ defaults

    def defaultKids(alg: String, ctx: Map[String, String]): Seq[JWK] = ???

  }

  implicit val kidsRegister = SimpleKidsRegister

}
