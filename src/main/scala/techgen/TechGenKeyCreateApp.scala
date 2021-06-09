package techgen

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.util.ScorexEncoding

object TechGenKeysCreateApp extends App with ScorexEncoding {

  val (priv: PrivateKey25519, pub: PublicKey25519Proposition) =
     PrivateKey25519Companion.generateKeys("genesis".getBytes)

  println("Keys: ")
  println("PRIVATE: ")
  println(encoder.encode(priv.privKeyBytes))
  println("PUBLIC: ")
  println(pub.toString())

}
