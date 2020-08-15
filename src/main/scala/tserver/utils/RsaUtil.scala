package tserver.utils

import java.security.KeyFactory
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64

import javax.crypto.Cipher
import zio.{Task, ZIO}

object RsaUtil {

  private val algorithm = "RSA/ECB/NoPadding"
  private val decoder = Base64.getDecoder

  def encrypt(input: Array[Byte], publicKey: String):Task[Array[Byte]] = ZIO.effect {
    val factory = KeyFactory.getInstance("RSA")
    val encodedKeySpec = new X509EncodedKeySpec(decoder.decode(publicKey))
    val pub = factory.generatePublic(encodedKeySpec)
    val cipherEncrypt = Cipher.getInstance(algorithm)
    cipherEncrypt.init(Cipher.ENCRYPT_MODE, pub)
    cipherEncrypt.doFinal(input)
  }

  def decrypt(input: Array[Byte], privateKey: String):Task[Array[Byte]] = ZIO.effect {
    val factory = KeyFactory.getInstance("RSA")
    val encodedKeySpec = new PKCS8EncodedKeySpec(decoder.decode(privateKey))
    val pr = factory.generatePrivate(encodedKeySpec)
    val cipherEncrypt = Cipher.getInstance(algorithm)
    cipherEncrypt.init(Cipher.DECRYPT_MODE, pr)
    cipherEncrypt.doFinal(input)//.takeRight(size)
  }

}
