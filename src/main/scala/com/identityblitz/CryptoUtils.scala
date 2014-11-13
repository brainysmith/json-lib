package com.identityblitz

import java.security._
import java.math.BigInteger
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.spec.{ECPrivateKeySpec, ECPublicKeySpec}
import org.bouncycastle.asn1._

object CryptoUtils {

  def calcSha1(data: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(data)
  }

  def genECPublicKey(curv: String, x: Array[Byte], y: Array[Byte]): PublicKey = {
    val spec = ECNamedCurveTable.getParameterSpec(curv)
    val point = spec.getCurve.createPoint(new BigInteger(1, x), new BigInteger(1, y), false)
    val keySpec = new ECPublicKeySpec(point, spec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePublic(keySpec)
  }

  def genECPrivateKey(curv: String, d: Array[Byte]): PrivateKey = {
    val spec = ECNamedCurveTable.getParameterSpec(curv)
    val keySpec = new ECPrivateKeySpec(new BigInteger(1, d), spec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePrivate(keySpec)
  }

  def encodeECSignature(join: Array[Byte]) = {
    if(join.length % 2 != 0)
      throw new IllegalStateException("incorrect EC signature format")

    val plen = join.length >> 1
    val r = new BigInteger(1, join.take(plen))
    val s = new BigInteger(1, join.takeRight(plen))
    val v: ASN1EncodableVector = new ASN1EncodableVector
    v.add(new DERInteger(r))
    v.add(new DERInteger(s))
    new DERSequence(v).getEncoded(ASN1Encoding.DER)
  }

  def decodeECSignature(encoded: Array[Byte]) = {
    val seq = ASN1Primitive.fromByteArray(encoded).asInstanceOf[ASN1Sequence]
    val rb = seq.getObjectAt(0).asInstanceOf[DERInteger].getValue.toByteArray.dropWhile(_ == 0)
    val sb = seq.getObjectAt(1).asInstanceOf[DERInteger].getValue.toByteArray.dropWhile(_ == 0)

    if(rb.length > 48 || sb.length > 48) {
      rb.reverse.padTo(66, 0x00.toByte).reverse ++ sb.reverse.padTo(66, 0x00.toByte).reverse
    }
    else if(rb.length > 32 || sb.length > 32) {
      rb.reverse.padTo(48, 0x00.toByte).reverse ++ sb.reverse.padTo(48, 0x00.toByte).reverse
    }
    else {
      rb.reverse.padTo(32, 0x00.toByte).reverse ++ sb.reverse.padTo(32, 0x00.toByte).reverse
    }
  }

}
