package com.identityblitz.jwt

import org.scalatest.{Matchers, FlatSpec}
import java.lang.IllegalArgumentException

object BaseJwtToolkit extends AlgorithmsKit with JwsToolkit with SimpleCryptoService with SimpleKidsRegisterService {
  override def extensionsNames: Set[String] = Set("exp")
}

class JwsTest extends FlatSpec with Matchers {

  import BaseJwtToolkit._

  behavior of "JWS toolkit"

  it should "deserialize JWS with [none] algorithm" in {
    val plainJwt = "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ."
    val jwt = JWT[ClaimsSet](plainJwt)

    jwt.header.alg.name should be (none.name)
    jwt.payload.iss should be (Some(StringOrUri("joe")))
    jwt.payload.exp should be (Some(new IntDate(1300819380)))
    jwt.payload.names should contain ("http://example.com/is_root")
  }

  import JWSNameKit._

  it should "build JWS with [none] algorithm" in {
    val noneJwt =  builder.alg(none)
      .header(typ % "JWT")
      .payload(cs(
      iss % StringOrUri("joe"),
      aud % Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3"))
    )).build

    noneJwt.header.alg.name should be (none.name)
    noneJwt.header.typ should be (Some("JWT"))
    noneJwt.payload.iss should be (Some(StringOrUri("joe")))
    noneJwt.payload.aud should be (Some(Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3")).toList))
  }

  it should "build JWS with [HS256] algorithm" in {
    val hs256Jwt = builder.alg(HS256).
      header(typ % "JWT")
      .payload(cs(
      iss % StringOrUri("joe"),
      aud % Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3")))).build

    hs256Jwt.header.alg.name should be (HS256.name)
    hs256Jwt.header.typ should be (Some("JWT"))
    hs256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    hs256Jwt.payload.aud should be (Some(Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3")).toList))
    hs256Jwt.asBase64 should be ("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLCJhdWQiOlsiYXVkMSIsImF1ZDIiLCJhdWQzIl19.52wjxDM6eY-5E8gbL7Ka1l5AiyqLcnyONuCQUxG5yA0")
  }

  it should "deserialize JWS with [HS256] algorithm" in {
    val strHs256Jwt = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    val hs256Jwt = JWT[ClaimsSet](strHs256Jwt)

    hs256Jwt.header.alg.name should be (HS256.name)
    hs256Jwt.header.typ should be (Some("JWT"))
    hs256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    hs256Jwt.payload.exp should be (Some(new IntDate(1300819380)))
    hs256Jwt.payload.names should contain ("http://example.com/is_root")
  }

  it should "deserialize JWS with [RS256] algorithm" in {
    val strRs256Jwt = "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw"
    val rs256Jwt = JWT[ClaimsSet](strRs256Jwt)

    rs256Jwt.header.alg.name should be (RS256.name)
    rs256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    rs256Jwt.payload.exp should be (Some(new IntDate(1300819380)))
    rs256Jwt.payload.names should contain ("http://example.com/is_root")
  }

  it should "build JWS with [RS256] algorithm" in {
    val rs256Jwt = builder.alg(RS256).
      header()
      .payload(cs(
      iss % StringOrUri("joe"),
      exp % IntDate(1300819380),
      "http://example.com/is_root" % true))
      .build

    rs256Jwt.header.alg.name should be (RS256.name)
    rs256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    rs256Jwt.payload.exp should be (Some(new IntDate(1300819380)))
    rs256Jwt.payload.names should contain ("http://example.com/is_root")
    rs256Jwt.asBase64 should be ("eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLCJleHAiOjEzMDA4MTkzODAsImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.el3lmx2zFYSGmoOC5sJFjV4nCFyb6_2nY5WDSv_d9L2cw857vQBhjV2xybTQz5_4IIVLxpollxyomEQpC1xwZSZoU9lrmNau2TGg1iFGjyIXrtZy-UxV0t_xSwujFlA_WNFjw6eLI00ji3EcuOiMpqPa8IOTfXijtgkCx7oVweb2IVO6ZjMcssvhA7s3ezF8YHf6ewHK74UF4o0RuKn4K1PjBbmxDu3TXMOp69IvbnCj2ku--9QI7H9DFjiNVyWWnpz3wekGZuUePAj5GkrbPgvwhVVUiTcczYy55MUaF7mPjkb7JGEk2sH4lCa1Jlvz9xgYMdYTfbwmT9Wgvq_Usg")
  }

  it should "deserialize JWS with [ES256] algorithm" in {
    val strEs256Jwt = "eyJhbGciOiJFUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"
    val es256Jwt = JWT[ClaimsSet](strEs256Jwt)

    es256Jwt.header.alg.name should be (ES256.name)
    es256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    es256Jwt.payload.exp should be (Some(new IntDate(1300819380)))
    es256Jwt.payload.names should contain ("http://example.com/is_root")
  }

  it should "build JWS with [ES256] algorithm" in {
    val es256Jwt = builder.alg(ES256).
      header()
      .payload(cs(
      iss % StringOrUri("joe"),
      exp % IntDate(1300819380),
      "http://example.com/is_root" % true))
      .build

    es256Jwt.header.alg.name should be (ES256.name)
    es256Jwt.payload.iss should be (Some(StringOrUri("joe")))
    es256Jwt.payload.exp should be (Some(new IntDate(1300819380)))
    es256Jwt.payload.names should contain ("http://example.com/is_root")
    val es256JwtCheck = JWT[ClaimsSet](es256Jwt asBase64)
  }

  it should "deserialize JWS with [ES512] algorithm" in {
    val strEs512Jwt = "eyJhbGciOiJFUzUxMiJ9.UGF5bG9hZA.AdwMgeerwtHoh-l192l60hp9wAHZFVJbLfD_UxMi70cwnZOYaRI1bKPWROc-mZZqwqT2SI-KGDKB34XO0aw_7XdtAG8GaSwFKdCAPZgoXD2YBJZCPEX3xKpRwcdOO8KpEHwJjyqOgzDO7iKvU8vcnwNrmxYbSW9ERBXukOXolLzeO_Jn"
    implicit object stringPConverter extends PayloadConverter[String]{
      def toBytes(orig: String): Array[Byte] = orig.getBytes("UTF-8")
      def fromBytes(a: Array[Byte]): String = new String(a, "UTF-8")
    }

    val es512Jwt = JWT[String](strEs512Jwt)
    es512Jwt.header.alg.name should be (ES512.name)
    es512Jwt.payload should be ("Payload")
  }

  it should "process [crit] Header parameter correctly" in {
    builder.alg(ES256).
      header(
        typ % "JWT",
        crit % Array("exp"),
        exp % new IntDate(1300819380)
      )
      .payload(cs(
      iss % StringOrUri("joe"),
      exp % IntDate(1300819380),
      "http://example.com/is_root" % true))
      .build
  }

  it should "reject JWS with [crit] Header parameter containing name that is absent in a header" in {
    a [IllegalStateException] should be thrownBy {
      builder.alg(ES256).
        header(
          typ % "JWT",
          crit % Array("exp")
        )
        .payload(cs(
        iss % StringOrUri("joe"),
        exp % IntDate(1300819380),
        "http://example.com/is_root" % true))
        .build
    }
  }

  it should "reject JWS with [crit] Header parameter containing unsupported extension" in {
    a [IllegalArgumentException] should be thrownBy {
      builder.alg(ES256).
        header(
          typ % "JWT",
          crit % Array("iss"),
          iss % StringOrUri("joe")
        )
        .payload(cs(
        iss % StringOrUri("joe"),
        exp % IntDate(1300819380),
        "http://example.com/is_root" % true))
        .build
    }
  }
  
}
