package com.identityblitz.jwt

import javax.crypto.{SecretKey, Mac}
import javax.crypto.spec.SecretKeySpec
import java.security.cert.X509Certificate
import java.security.cert.CertificateFactory
import java.io.ByteArrayInputStream
import java.security._
import java.security.spec.{RSAPrivateKeySpec, RSAPublicKeySpec}
import com.identityblitz.CryptoUtils
import org.bouncycastle.asn1._
import java.math.BigInteger

trait SimpleCryptoService extends JwkToolkit {

  object SimpleCrypto extends CryptoService {

    def sign(desiredAlg: String, key: Key, plainText: Array[Byte]): Array[Byte] = {
      key match {
        case s: SecretKey =>
          val hs256 = Mac.getInstance(desiredAlg)
          hs256.init(key)
          hs256.doFinal(plainText)
        case p: PrivateKey =>
          val sig = Signature.getInstance(desiredAlg)
          sig.initSign(p)
          sig.update(plainText)
          sig.sign()
      }
    }

    def sign(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = {
      alg match {
        case "HS256" =>
          val hs256 = Mac.getInstance("HmacSHA256")
          hs256.init(new SecretKeySpec(jwk.asInstanceOf[SymmetricKey].k, "HmacSHA256"))
          hs256.doFinal(plainText)
        case "RS256" =>
          val sig = Signature.getInstance("SHA256withRSA")
          val key = jwk.asInstanceOf[RsaPrivateKey]
          sig.initSign(KeyFactory.getInstance("RSA").generatePrivate(new RSAPrivateKeySpec(key.n, key.d)))
          sig.update(plainText)
          sig.sign()
        case "ES256" =>
          val sig = Signature.getInstance("SHA256withECDSA")
          val key = jwk.asInstanceOf[EcPrivateKey]
          sig.initSign(CryptoUtils.genECPrivateKey(key.crv, key.d))
          sig.update(plainText)
          val seq = ASN1Primitive.fromByteArray(sig.sign()).asInstanceOf[ASN1Sequence]
          val r = seq.getObjectAt(0).asInstanceOf[DERInteger].getValue.toByteArray
            .reverse.padTo(32, 0x00.toByte).reverse
          val s = seq.getObjectAt(1).asInstanceOf[DERInteger].getValue.toByteArray
            .reverse.padTo(32, 0x00.toByte).reverse
          r ++ s
      }
    }

    def verify(desiredAlg: String, key: Key, plainText: Array[Byte], signature: Array[Byte]): Boolean = {
      key match {
        case s: SecretKey =>
          val hmac = Mac.getInstance(desiredAlg)
          hmac.init(key)
          val calculated = hmac.doFinal(plainText)
          calculated.deep == signature.deep
        case p: PublicKey =>
          val sig = Signature.getInstance(desiredAlg)
          sig.initVerify(p)
          sig.update(plainText)
          sig.verify(signature)
      }
    }

    def encrypt(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = ???

    def decrypt(alg: String, jwk: JWK, cipherText: Array[Byte]): Array[Byte] = ???

    def createX509Cert(der: Array[Byte]): X509Certificate = {
      val factory = CertificateFactory.getInstance("X.509")
      factory.generateCertificate(new ByteArrayInputStream(der)).asInstanceOf[X509Certificate]
    }

    def verifyX509CertChain(chain: Array[X509Certificate]): Option[X509Certificate] = ???

  }

  implicit val cryptoService = SimpleCrypto

}
