package com.identityblitz.jwt

trait JweToolkit extends AlgorithmsKit with JwtToolkit {

  /**
   * This traits is represents JWS header.
   */
  trait JWE extends Header[JWE]

}
