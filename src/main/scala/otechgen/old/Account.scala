package otechgen.old

import scorex.crypto.signatures.Curve25519

case class Account(privKey: ByteStr, pubKey: ByteStr)

object Account {

  def generate(secret: String): Account = {
    val (ok, pub) = Curve25519.createKeyPair(secret.getBytes())
    new Account(ByteStr(ok), ByteStr(pub))
  }

}